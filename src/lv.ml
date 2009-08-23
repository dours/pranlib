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

module LVSemilattice
               (DFAC : Sig)
               (A: ProgramView.Abstractor with
                    type Abstract.node = DFAC.Statement.t list and
										type Abstract.edge = DFAC.EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge)=
struct
	type t = L of DFAC.BitVector.t
  
  let make ival = 
    let nodes = G.nodes G.graph in
    let all_bit_vectors = 
    let bit_vector_parts = List.map 
                           (fun node ->
                                let anode = A.node node in 
                                let statement_parts =
                                let extract_from_mvp mvp = List.map (fun v -> DFAC.BitVectorElement.construct v false) mvp in 
                                  List.map 
                                    (fun s -> 
                                             match s with (lp, rp) ->
                                              let lp_extracted = extract_from_mvp lp in
                                              let rp_extracted = extract_from_mvp rp in
                                              List.concat [lp_extracted;rp_extracted]) anode in List.concat statement_parts) nodes in
    List.concat bit_vector_parts in
    let rec remove_duplicates bv result = match bv with 
      | [] -> List.rev result
      | hd::tl -> 
        let v = DFAC.BitVectorElement.var hd in
        if (List.exists (fun e -> DFAC.V.equals v (DFAC.BitVectorElement.var e)) tl) then
          remove_duplicates tl result
        else 
          remove_duplicates tl (hd::result)
        in
    remove_duplicates all_bit_vectors []
    
  let make_top _ = make true
  
  let make_bottom _ = make false 
	
	let top =
		printf "Top lattice element is requested\n"; 
		L (make_top ())
	
	let bottom = L (make_bottom ())
	
	let cap x y = match (x, y) with
		| (L (bv_x), L (bv_y)) -> L (DFAC.BitVector.cap bv_x bv_y)

	let equal x y = match (x, y) with
		| (L (bv_x), L(bv_y)) -> DFAC.BitVector.equal bv_x bv_y
	let toString x = ""
end

module Repr (DFAC : Sig)
=
struct
	type node = DFAC.NodeInfo.t
	type edge = DFAC.EdgeInfo.t
end

module LVAdapter
               (DFAC : Sig)
               (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
               (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge)=
struct
	module P = Repr(DFAC)
  	module LVSemilattice'=LVSemilattice(DFAC)(A)(G)
	module L = Semilattice.Make(LVSemilattice')
	module S = DFAC.Statement
	module BV = DFAC.BitVector
	module BVE = DFAC.BitVectorElement
  	module NI = DFAC.NodeInfo
	
	let flow node l =
		printf "Flow is called for node %s\n" (NI.toString node);
    let statements = node in (* Node statements *)
		let gen_bv =
			printf "Calculating gen for %s\n" (NI.toString statements); 
			DFAC.DFAHelper.gen statements (LVSemilattice'.make_bottom ()) in (* GEN *)
		printf "GEN for %s is %s\n" (NI.toString statements) (BV.toString gen_bv);
		let kill_bv =
			printf "Calculating kill for %s\n" (NI.toString statements);
			DFAC.DFAHelper.kill statements (LVSemilattice'.make_bottom()) in (* KILL *)
    printf "KILL for %s is %s\n" (NI.toString statements) (BV.toString kill_bv);
    let rec process_recursively input gen_v kill_v result =
      match (input, gen_v, kill_v) with
			| ([], [], []) -> result
			| (l, [], []) -> 
				printf "INPUT case matched with %s\n" (BV.toString l);
				l
			| (input_hd::input_tl, gen_hd::gen_tl, kill_hd::kill_tl) ->
        printf "Full case matched with [in=%s; gen=%s; kill=%s]\n" (BVE.toString input_hd) (BVE.toString gen_hd) (BVE.toString kill_hd);
				let new_result = (BVE.var input_hd, (BVE.state gen_hd	|| (BVE.state input_hd && BVE.state kill_hd)))::result
				in process_recursively input_tl	gen_tl	kill_tl new_result
		in
		match l with
			| LVSemilattice'.L (l_vector) ->
				printf "Flow calculated to lattice element\n";
				LVSemilattice'.L(process_recursively l_vector gen_bv kill_bv [])
	
	let init _ = L.top
  end
   
module LVResults 
               (DFAC : Sig)
               (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
               (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge) =
struct
    module LVSemilattice' = LVSemilattice(DFAC)(A)(G)
    module LVAdapter''=LVAdapter(DFAC)(A)(G)
	module LVAdapter'=ProgramView.BackwardAdapter(LVAdapter'')
	module PView=ProgramView.Make(LVAdapter')(A)(G)
	module Analyze=DFAEngine.RevPost(PView)(DFST.Make(G))
	
	type lvInfo = DFAC.BitVector.t
	
	let before n =
		match G.ins n with
          |  []       -> DFAC.BitVector.empty
          | hd :: _  ->
						let analyzeResults = match Analyze.get hd with
						| LVSemilattice'.L v -> v
						in
						analyzeResults

	let after n = 
		let g_outs = G.outs n in
         match g_outs with
		| [] -> DFAC.BitVector.empty
		| hd::_ ->
			printf "Analyzing hd\n";
			match Analyze.get hd with
			| LVSemilattice'.L v ->
				printf "Encountered a lattice element\n"; 
				v
end
