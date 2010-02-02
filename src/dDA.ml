(*
 * DDG: Data-Dependency Analysis implementation
 * Copyright (C) 2009
 * Andrey Serebryansky, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)
open Printf

(** Signature for dependence distance representation *)
module type DependenceDistanceSig=
sig
	module Info : DDAInfo.Sig
	
	type t
	
	(** [distance_value dd var] returns dependence distance value for given variable/iteration variable *)
	val distance_value : t -> Info.Variable.t -> Info.Value.t
	
	val toString : t -> string  
end

(** Signature for dependency representation *)
module type DependencySig=
sig
	module Info : DDAInfo.Sig
	module DependenceDistance : DependenceDistanceSig with module Info = Info
	
	type t
	
	val from : t -> Info.Statement.t
	
	val _to : t -> Info.Statement.t
	
	val distance : t -> DependenceDistance.t
	
	val toString : t -> string
end

(** Loop statement dependency test signature *)
module type LoopStatementDependencyTestSig= 
sig
	module Info : DDAInfo.Sig
	module DStruct : DDGStructure.Sig with module Info = Info
	module DependenceDistance : DependenceDistanceSig with module Info = Info
	module Dependency : DependencySig with module Info = Info and module DependenceDistance = DependenceDistance
	
	(** [get_dependencies l] returns the list of dependencies between loop statements with corresponding dependency distance *)
	val get_dependencies : Info.Loop.t -> Dependency.t list
end

(** Main signature for dependency analysis framework *)
module type Sig = 
sig
  module DDAI : DDAInfo.Sig
  module G : CFG.Sig with type Node.info = DDAI.NodeInfo.t and type Edge.info = DDAI.EdgeInfo.t
  module DG : DDG.Sig
	module DStruct : DDGStructure.Sig
	module DependenceDistance : DependenceDistanceSig with module Info = DDAI
	module Dependency : DependencySig with module Info = DDAI and module DependenceDistance = DependenceDistance
	module GCDLoopStatementDependencyTest : LoopStatementDependencyTestSig with module Info = DDAI and module DependenceDistance = DependenceDistance and module Dependency = Dependency
	
	(** [analyze g] analyzes CFG g and returns a data dependency graph *)
	val analyze : G.t -> (DDAI.Loop.t -> Dependency.t list) -> Dependency.t list
end

module Make (D : DDAInfo.Sig)
						(CG : CFG.Sig with type Node.info = D.NodeInfo.t and type Edge.info = D.EdgeInfo.t)
						(DG : DDG.Sig)
						(DS : DDGStructure.Sig with module Info = D)=
struct
	module DDAI = D
  module G = CG
  module DG = DG
	module DStruct = DS
	module VHash = Hashtbl.Make(DDAI.Variable)
	
	module DependenceDistance=
	struct
		module Info = DDAI
		
		type t = {d:Info.Value.t VHash.t}
		
		let distance_value dd v = VHash.find dd.d v
		
		let make table = {d=table}
		
		let toString dd = "TODO!"
	end
	
	module Dependency=
	struct
		module Info = DDAI
		module DependenceDistance = DependenceDistance
		
		type t = (Info.Statement.t * Info.Statement.t * DependenceDistance.t)
		
		let from (f, _, _) = f
		
		let _to (_, t, _) = t
		
		let distance (_, _, dd) = dd
		
		let make f t dd = (f, t, dd)
		
		let toString (f, t, dd) = "From:"^(Info.Statement.toString f)
															^"\n"
															^"To:"^(Info.Statement.toString t)
															^"\n"
															^"Distance:"^(DependenceDistance.toString dd) 
	end
	
	module Util=
	struct
		module Info = DDAI
		module C = Info.Coefficient
		module Val = Info.Value
		
		(* [eval cs] evaluates expression involving coefficients. Fails with exception if one of the coefficients doesn't have a value *)
		let rec check_coeffs c =
				match c with 
					| [] -> ()
					| hd::tl -> 
							match (Info.Coefficient.get_value hd) with
								| None -> failwith "Exception"
								| _ -> check_coeffs tl 
		
		let eval coeffs =
			let rec eval_int acc c = 
				match c with
					| [] -> acc
					| hd::tl ->
						let update_acc a h = match C.get_value h with
							| Some v -> Val.add a v
							| None -> a 
							in 
						eval_int (update_acc acc hd) tl
				in
			check_coeffs coeffs;
			eval_int (Val.make 0) coeffs
			
		let simplify i_vars htbl_l htbl_r free_l free_r = 
			let htbl_result = VHash.create 65535 in
			List.iter (fun x ->
				if VHash.mem htbl_l x then 
					if VHash.mem htbl_r x then
						VHash.add htbl_result x (Val.subtract (VHash.find htbl_l x) (VHash.find htbl_r x))
					else
						VHash.add htbl_result x (VHash.find htbl_l x)
				else
					if VHash.mem htbl_r x then
						VHash.add htbl_result x (Val.subtract (Val.make 0) (VHash.find htbl_r x))
					else
						()  
				)  i_vars; (htbl_result, (Val.subtract free_r free_l))
	end
	
	module GCDLoopStatementDependencyTest=
	struct
		module Info = DDAI
		module DependenceDistance = DependenceDistance
		module Dependency = Dependency
		module DStruct = DStruct
		module Val = Info.Value
		module S = Info.Statement
		module L = Info.Loop
		module SS = Info.SubscriptSpec
		module SSC = Info.SubscriptSpecComponent
		module C = Info.Coefficient
		module V = Info.Variable
		module R = Info.Reference
		
		let gcd values =
			let zero = Val.make 0 in 
			let rec gcd_of_two a b = 
				if Val.equal a zero then a
				else
					if Val.equal b zero then b
					else
						if Val.gt a b then 
							gcd_of_two (Val._mod a b) b
						else
							gcd_of_two a (Val._mod b a)
					in
			match values with
				| [] -> Val.make 0
				| hd::tl -> List.fold_left (fun x y -> gcd_of_two x y) hd tl
		
		let get_dependencies l =
			LOG (printf "get_dependencies is called for GCD test\n");
			let statements = L.gather_statements l in
			let ivars = L.gather_iteration_variables l in
			let analyze_statement stmt = 
				let write_coefficients_table = VHash.create 65535 in
				let var_w_ref = S.writes stmt in
				let var_w = R.variable var_w_ref in 
				let var_w_ss = R.subscript var_w_ref in
				let hash_coefficients table ss =
					Util.check_coeffs (List.map (fun x -> SSC.coefficient x) (SS.components ss)); 
					List.iter 
							(fun x -> 
									let v_opt = SSC.variable x in
									match v_opt with
										| Some v -> 
											match C.get_value (SSC.coefficient x) with
												| Some value -> VHash.add table v value
												| None -> ()
										| None -> ())
																			 (SS.components ss);()
										 in
				let gather_free_coefficients ss = List.map (fun x -> SSC.coefficient x) (List.find_all (fun x -> SSC.is_free x) (SS.components ss)) in
				match var_w_ss with
					| Some var_w_ss_s ->
						hash_coefficients write_coefficients_table var_w_ss_s;
						let var_w_free_coeffs = gather_free_coefficients var_w_ss_s in
						let var_w_free = Util.eval var_w_free_coeffs in 
						let analyze_statement_for stmt_t = 
							let read_references = List.find_all (fun x -> V.equals (R.variable x) var_w) (S.reads stmt_t) in
							let calc_dependency ref = 
								match (R.subscript ref) with
									| Some ss -> 
															let read_coefficients_table = VHash.create 65535 in
															hash_coefficients read_coefficients_table ss;
															let var_r_free = Util.eval (gather_free_coefficients ss) in
															let (lp, rp) = Util.simplify ivars write_coefficients_table read_coefficients_table var_w_free var_r_free in
															let coeff_values = List.concat (List.map (fun x -> if VHash.mem lp x then [VHash.find lp x] else []) ivars) in
															let coeff_gcd = gcd coeff_values in
															if not (Val.equal (Val._mod coeff_gcd rp) (Val.make 0)) then
															[Dependency.make stmt stmt_t (DependenceDistance.make (VHash.create 0))]
															else
															[]
									| _ -> failwith "Reference to array variable without subscript"; []
								in
							[] 
						in
								
						[]
					| _ -> []
				in
(*					let analyze_statement_reads_for var*)
(*				List.iter (fun x -> VHash.add write_coefficients_table )   *)
			[]
	end
	
	let analyze g dcheck_fun = 
		LOG (printf "Analysis started...\n");
		LOG (printf "Initializing dependency graph...\n");
		List.concat (List.map (fun x ->
			let info = G.Node.info x in
			LOG (printf "Processing node %s \n" (DDAI.NodeInfo.toString info));
			match DDAI.NodeInfo.loop info with
				| Some l ->
					LOG (printf "Applying analysis to loop...\n"); 
					dcheck_fun l
				| None -> [] 
			) (G.nodes g)) 
    
end

