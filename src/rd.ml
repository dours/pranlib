(*
 * RD: implements 'Reaching definitions' data analysis problems
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
module RDSemilattice
                (DFAC : DFACommon.Sig)
                (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
                (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge)=
  struct
    type t = L of DFAC.RBitVector.t
    
    let make_empty b = 
      let nodes = G.nodes G.graph in 
      let bit_vector_parts = List.map (fun node -> let anode = A.node node in List.map (fun s -> DFAC.RBitVectorElement.construct s b ) anode) nodes in
        List.concat bit_vector_parts
        
    let make_empty_top _ = make_empty true
    
    let make_empty_bottom _ = make_empty false
  	
    let top = L (make_empty_top ()) (* Make sure this is the right way. How should we provide a way to list all statements in an application *)
  	
    let bottom = L (make_empty_bottom ())
    
    let cap x y = match (x, y) with
  		| (L (bv_x), L (bv_y)) -> L (DFAC.RBitVector.cap bv_x bv_y)
  
    let equal x y = match (x, y) with
  		| (L (bv_x), L(bv_y)) -> DFAC.RBitVector.equal bv_x bv_y
    
    let toString = function
			| L rbv -> DFAC.RBitVector.toString rbv
			| _ -> ""
    end
    
    module Repr (DFAC : DFACommon.Sig)=
    struct
    	type node = DFAC.NodeInfo.t
    	type edge = DFAC.EdgeInfo.t
    end
     
    module RDAdapter
    		   (DFAC : DFACommon.Sig)
               (A: ProgramView.Abstractor with type Abstract.node = DFAC.NodeInfo.t and type Abstract.edge = DFAC.EdgeInfo.t)
               (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge)=
	struct
		module P = Repr(DFAC)
	    module RDSemilattice' = RDSemilattice(DFAC)(A)(G)
		module L = Semilattice.Make(RDSemilattice')
		module S = DFAC.Statement
		module BV = DFAC.RBitVector
		module BVE = DFAC.RBitVectorElement
  
	  let prsv statements = 
	    let bottom = RDSemilattice'.make_empty_bottom () in
	    List.map (fun e -> let s = BVE.statement e in
	                        if (List.exists (fun x -> S.lp_match x s && not (x == s)) statements) 
	                        then (BVE.construct s true) else e) bottom
	  let gen statements = 
	    let bottom = RDSemilattice'.make_empty_bottom () in
	      List.map (fun e -> let s = BVE.statement e in 
	                        if (List.exists (fun x -> x == s) statements) then 
	                        (BVE.construct s true) else e) bottom  
  
	let flow node l = 
		let statements = node in (* Node statements *)
        LOG(printf "%s: Calculating flow for node %s\n" (DFAC.print_time ()) (DFAC.NodeInfo.toString node));
		let gen_bv = gen statements in
		let prsv_bv = prsv statements in
		let rec process_recursively input gen_v prsv_v result =
      match (input, gen_v, prsv_v) with
			| ([], [], []) ->
        LOG(printf "%s: Empty lists case matched\n" (DFAC.print_time ())); 
        result
      | (input_hd::input_tl, gen_hd::gen_tl, prsv_hd::prsv_tl) ->
        LOG(printf "%s: Full case matched\n" (DFAC.print_time ()));
        LOG(printf "Input: %s\n" (BV.toString input));
        LOG(printf "Gen: %s\n" (BV.toString gen_v));
        LOG(printf "Prsv: %s\n" (BV.toString prsv_v));
				let new_result = List.concat [result;[(BVE.statement input_hd, (BVE.state gen_hd	|| (BVE.state input_hd && BVE.state prsv_hd)))]]
				in
				LOG(printf "New result: %s\n" (BV.toString new_result)); 
				process_recursively input_tl	gen_tl	prsv_tl new_result
	  | _ -> result
		in
		match l with
		| RDSemilattice'.L (l_vector) ->
        LOG(printf "%s: Calculating flow for lattice element: %s\n" (DFAC.print_time ()) (BV.toString l_vector));
				let new_vector = process_recursively l_vector gen_bv prsv_bv [] in
				LOG(printf "%s: Calculated flow to element: %s\n" (DFAC.print_time()) (BV.toString new_vector));
(*				let f b = b+1 in 
				let r = Scanf.bscanf Scanf.Scanning.stdib "%d" f in
				LOG(printf "%s\n" (Pervasives.string_of_int r));*) 	
        RDSemilattice'.L(new_vector)
	
	let init _ = L.top 
    end
    
    module RDResults
    				(DFAC : DFACommon.Sig)
    				(A: ProgramView.Abstractor with type Abstract.node = DFAC.NodeInfo.t and type Abstract.edge = DFAC.EdgeInfo.t)
    				(G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge)=
    struct
        module RDSemilattice' = RDSemilattice(DFAC)(A)(G)
        module RDAdapter'' = RDAdapter(DFAC)(A)(G)
    	module RDAdapter'=ProgramView.ForwardAdapter(RDAdapter'')
    	module PView=ProgramView.Make(RDAdapter')(A)(G)
    	module Analyze = DFAEngine.RevPost (PView) (DFST.Make (G))
    	
    	type rdInfo = DFAC.Statement.t list
    	
    	let reachable_statements rbv = List.map (fun rbve -> DFAC.RBitVectorElement.statement rbve) (List.filter (fun rbve -> DFAC.RBitVectorElement.state rbve) rbv)
    	
    	let before n = match G.ins n with
    		| [] ->
          LOG(printf "%s: No incoming edges\n" (DFAC.print_time ())); 
					[]
    		| hd::_ ->
          LOG(printf "%s: Calling Analyze.get\n" (DFAC.print_time ())); 
          match Analyze.get hd with
    			| RDSemilattice'.L v ->
            LOG(printf "%s: Lattice element encountered\n" (DFAC.print_time ())); 
            reachable_statements v
    	let after n = match G.outs n with
    		| [] ->
          LOG(printf "%s: No outgoing edges\n" (DFAC.print_time ()));
					 []
    		| hd::_ ->
          LOG(printf "%s: Calling Analyze.get\n" (DFAC.print_time ())); 
          match Analyze.get hd with
    			| RDSemilattice'.L v ->
            LOG(printf "%s: Analyze.get: Lattice element encountered\n" (DFAC.print_time ())); 
            reachable_statements v
  end