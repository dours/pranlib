(*
 * LV: implements 'Live variables' data analysis problems
 * Copyright (C) 2008
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
open DFACommon

module NodeInfo=
struct
	type t = Statement.t list
	
	let toString statements = 
		let rec toString' statements acc = 
			match statements with
				| [] -> acc
				| hd :: tl -> toString' tl (DFACommon.Statement.toString hd)^acc
			in
			toString' statements ""
end

module EdgeInfo=
struct
	type t = Empty
	
	let toString _ = "edge"
end

module LVSemilattice=
struct
	type t = L of BitVector.t | Bottom
	
	let top =
		printf "Top lattice element is requested\n"; 
		L BitVector.empty (* Make sure this is the right way. How should we provide a way to list all statements in an application *)
	
	let bottom = Bottom
	
	let cap x y = match (x, y) with
		| (_, Bottom) -> Bottom
		| (Bottom, _) -> Bottom
		| (L (bv_x), L (bv_y)) -> L (BitVector.cap bv_x bv_y)

	let equal x y = match (x, y) with
		| (Bottom, Bottom) -> true
		| (Bottom, _) -> false
		| (_, Bottom) -> false
		| (L (bv_x), L(bv_y)) -> BitVector.equal bv_x bv_y
	let toString x = ""
end

module Repr=
struct
	type node = NodeInfo.t
	type edge = EdgeInfo.t
end

module LVAdapter=
struct
	module P = Repr
	module L = Semilattice.Make(LVSemilattice)
	module S = Statement
	module BV = BitVector
	module BVE = BitVectorElement
	
	let flow node l =
		printf "Flow is called for node %s\n" (NodeInfo.toString node);
    let statements = node in (* Node statements *)
		let gen_bv =
			printf "Calculating gen for %s\n" (NodeInfo.toString statements); 
			DFAHelper.gen statements in (* GEN *)
		printf "GEN for %s is %s\n" (NodeInfo.toString statements) (BitVector.toString gen_bv);
		let kill_bv =
			printf "Calculating kill for %s\n" (NodeInfo.toString statements);
			DFAHelper.kill statements in (* KILL *)
    printf "KILL for %s is %s\n" (NodeInfo.toString statements) (BitVector.toString kill_bv);
    let rec process_recursively input gen_v kill_v result =
      let process_only_lattice_element l = l in
      let process_gen_case g = g in
      let process_kill_case _ = failwith "Invalid case matched" in
      let process_lgen_case l g = 
        let l_or_g = List.map (fun x -> 
                                                    let xs = BitVectorElement.var x in
                                                    let es = BitVector.lookup_element xs g in
                                                    let state =
                                                      match es with 
                                                      | Some (e) -> ((BitVectorElement.state x)||(BitVectorElement.state e))
                                                      | None -> BitVectorElement.state x || true (* considering that missing gen element evaluates to true *) in
                                                    BitVectorElement.construct xs state) l in
        let existence_map = List.map (fun x -> if List.exists (fun y -> Variable.equals (BitVectorElement.var x) (BitVectorElement.var y)) l_or_g then [] else [x]) g in
        List.concat [l_or_g;List.concat existence_map] in
          
     let process_lkill_case l k = 
      let l_and_not_k = List.map (fun x ->
                                                    let xs = BitVectorElement.var x in
                                                    let es = BitVector.lookup_element xs k in
                                                    let state = 
                                                      match es with
                                                        | Some(e) -> ((BitVectorElement.state x) && (not (BitVectorElement.state e)))
                                                        | None -> BitVectorElement.state x && true in
                                                      BitVectorElement.construct xs state) l in
        let existence_map = List.map (fun x -> if List.exists (fun y -> Variable.equals (BitVectorElement.var x) (BitVectorElement.var y)) l_and_not_k then [] else [x]) k in
        List.concat [l_and_not_k; List.concat existence_map] in
                                                       
    match (input, gen_v, kill_v) with
			| ([], [], []) -> result
			| (l, [], []) -> 
				printf "INPUT case matched\n";
				process_only_lattice_element l
			| ([], g, []) ->
				printf "GEN case matched %s\n" (BitVector.toString g);
				process_gen_case g
			| ([], [], k) ->
				printf "KILL case matched %s\n" (BitVector.toString k);
				process_kill_case k
      | (l, g, []) -> 
        printf "L|GEN case matched \n";
        process_lgen_case l g
      | ([], g, k) -> 
        printf "GEN|KILL case matched \n";
        g
      | (l, [], k) ->
        printf "L|KILL case matched \n";
        process_lkill_case l k
			| (input_hd::input_tl, gen_hd::gen_tl, kill_hd::kill_tl) ->
				let new_result = (BVE.var input_hd, (BVE.state gen_hd	|| (BVE.state input_hd && BVE.state kill_hd)))::result
				in process_recursively input_tl	gen_tl	kill_tl new_result
		in
		match l with
			| LVSemilattice.Bottom ->
				printf "Flow calculated to Bottom\n"; 
				LVSemilattice.Bottom
			| LVSemilattice.L (l_vector) ->
				printf "Flow calculated to lattice element\n";
				LVSemilattice.L(process_recursively l_vector gen_bv kill_bv [])
	
	let init _ = L.top
end

module LVResults (A: ProgramView.Abstractor with
                    type Abstract.node = DFACommon.Statement.t list and
										type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge) =
struct
	module LVAdapter'=ProgramView.BackwardAdapter(LVAdapter)
	module PView=ProgramView.Make(LVAdapter')(A)(G)
	module Analyze=DFAEngine.RevPost(PView)(DFST.Make(G))
	
	type lvInfo = DFACommon.BitVector.t
	
	let before n =
		match G.ins n with
          |  []       -> DFACommon.BitVector.empty
          | hd :: _  ->
						let analyzeResults = match Analyze.get hd with
						| LVSemilattice.L v -> v
						| LVSemilattice.Bottom -> DFACommon.BitVector.empty in
						analyzeResults

	let after n = 
		let g_outs = G.outs n in
         match g_outs with
		| [] -> DFACommon.BitVector.empty
		| hd::_ ->
			printf "Analyzing hd\n";
			match Analyze.get hd with
			| LVSemilattice.L v ->
				printf "Encountered a lattice element\n"; 
				v
			| LVSemilattice.Bottom ->
				printf "Bottom lattice element encountered\n"; 
				DFACommon.BitVector.empty
end

module TestModule=
struct
	let print_smth s = printf "print_smth"; s
end