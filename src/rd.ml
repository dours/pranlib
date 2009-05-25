(*
* RD: implements 'Reaching definitions' data analysis problems. Implementaion
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
open DFACommon
open Printf

module RBitVectorElement = 
struct
  type t = Statement.t * bool
	
  let construct var b = (var, b)
  
	let and_op x y = 
		match (x, y) with
			| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit && y_bit)
	
	let or_op x y =
		match (x, y) with
			| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit || y_bit)

	let equal x y = 
		match (x, y) with
			| ((x_var, _), (y_var, _)) -> Statement.equals x_var y_var

	let statement x = match x with 
		| (v, _) -> v
		| _ -> failwith "Invalid argument"

	let state x = match x with
		| (_, b) -> b
		| _ -> failwith "Invalid argument"
	
	let toString (v, b) = "E["^(Statement.toString v)^","^(string_of_bool b)^"]"
end

module RBitVector= 
struct
	type t = RBitVectorElement.t list
	
	let empty = []
	
	let cap bv1 bv2 =
		let rec append_or_cap list1 list2 = 
			match list1 with
				| [] -> list2
				| hd::tl -> if List.exists (fun x -> RBitVectorElement.equal x hd) list2 then 
					let rec lookup_and_change elem lst =
						match lst with
							| [] -> []
							| hd1::tl1 ->  if RBitVectorElement.equal hd1 elem then (RBitVectorElement.and_op hd1 elem) :: tl else hd1::(lookup_and_change elem tl1)
					in
					lookup_and_change hd list2
					else hd::(append_or_cap tl list2)
		in
		append_or_cap bv1 bv2
		
		let equal x y =
			let rec equal' x y = match (x, y) with
				| ([], []) -> true
				| ([], _) -> false
				| (_, []) -> false
				| (hd_x::tl_x, hd_y::tl_y) -> 
					if RBitVectorElement.equal hd_x hd_y then equal' tl_x tl_y 
					else false
			in
			equal' x y
      
    let rec lookup_element var bit_vector = 
      let compare v bit_vector_element = Statement.equals v (RBitVectorElement.statement bit_vector_element) in
        match bit_vector with
          | [] -> None
          | hd::tl -> if compare var hd then Some(hd) else lookup_element var tl 
			
		let toString bv = 
			let strRepr = List.map (fun x -> RBitVectorElement.toString x) bv in
			"BV["^(List.fold_right (fun x y -> 
				let delimiter = if x == "" || y == "" then "" else ";" in
				x^delimiter^y) strRepr "")^"]"
			
end

module RDSemilattice(A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge)=
struct
	type t = L of RBitVector.t
  
  let make_empty b = 
    let nodes = G.nodes G.graph in 
    let bit_vector_parts = List.map (fun node -> let anode = A.node node in List.map (fun s -> RBitVectorElement.construct s b ) anode) nodes in
      List.concat bit_vector_parts
      
  let make_empty_top _ = make_empty true
  
  let make_empty_bottom _ = make_empty false
	
	let top = L (make_empty_top ()) (* Make sure this is the right way. How should we provide a way to list all statements in an application *)
	
	let bottom = L (make_empty_bottom ())
  
  let cap x y = match (x, y) with
		| (L (bv_x), L (bv_y)) -> L (RBitVector.cap bv_x bv_y)

	let equal x y = match (x, y) with
		| (L (bv_x), L(bv_y)) -> RBitVector.equal bv_x bv_y
  
	let toString x = ""
end

module Repr=
struct
	type node = NodeInfo.t
	type edge = EdgeInfo.t
end

module RDAdapter(A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge)=
struct
	module P = Repr
  module RDSemilattice' = RDSemilattice(A)(G)
	module L = Semilattice.Make(RDSemilattice')
	module S = Statement
	module BV = RBitVector
	module BVE = RBitVectorElement
  
  let prsv statements = 
    let bottom = RDSemilattice'.make_empty_bottom () in
    List.map (fun e -> let s = RBitVectorElement.statement e in
                        if (List.exists (fun x -> Statement.lp_match x s && not (x == s)) statements) 
                        then (RBitVectorElement.construct s true) else e) bottom
  let gen statements = 
    let bottom = RDSemilattice'.make_empty_bottom () in
      List.map (fun e -> let s = RBitVectorElement.statement e in 
                        if (List.exists (fun x -> x == s) statements) then 
                        (RBitVectorElement.construct s true) else e) bottom  
  
	let flow node l = 
		let statements = node in (* Node statements *)
    printf "Calculating flow for node %s\n" (NodeInfo.toString node);
		let gen_bv = gen statements in
		let prsv_bv = prsv statements in
		let rec process_recursively input gen_v prsv_v result =
      match (input, gen_v, prsv_v) with
			| ([], [], []) ->
        printf "Empty lists case matched\n"; 
        result
      | (input_hd::input_tl, gen_hd::gen_tl, prsv_hd::prsv_tl) ->
        printf "Full case matched\n";
        printf "Input: %s\n" (RBitVector.toString input);
        printf "Gen: %s\n" (RBitVector.toString gen_v);
        printf "Prsv: %s\n" (RBitVector.toString prsv_v);
				let new_result = (BVE.statement input_hd, (BVE.state gen_hd	|| (BVE.state input_hd && BVE.state prsv_hd)))::result
				in process_recursively input_tl	gen_tl	prsv_tl new_result
		in
		match l with
		| RDSemilattice'.L (l_vector) ->
        printf "Flow calculated to lattice element\n";	
        RDSemilattice'.L(process_recursively l_vector gen_bv prsv_bv [])
	
	let init _ = L.top 
end

module RDResults (A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge)=
struct
  module RDSemilattice' = RDSemilattice(A)(G)
  module RDAdapter'' = RDAdapter(A)(G)
	module RDAdapter'=ProgramView.ForwardAdapter(RDAdapter'')
	module PView=ProgramView.Make(RDAdapter')(A)(G)
	module Analyze = DFAEngine.RevPost (PView) (DFST.Make (G))
	
	type rdInfo = RBitVector.t
	
	let before n = match G.ins n with
		| [] ->
      printf "No incoming edges\n"; 
      RBitVector.empty
		| hd::_ ->
      printf "Calling Analyze.get\n"; 
      match Analyze.get hd with
			| RDSemilattice'.L v ->
        printf "Lattice element encountered\n"; 
        v
	let after n = match G.outs n with
		| [] ->
      printf "No outgoing edges\n"; 
      RBitVector.empty
		| hd::_ ->
      printf "Calling Analyze.get\n"; 
      match Analyze.get hd with
			| RDSemilattice'.L v ->
        printf "Lattice element encountered\n"; 
        v
end