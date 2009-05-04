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

module NodeInfo=
struct
	type t = Statement.t list
end

module EdgeInfo =
struct
	type t = Empty

	let toString _ = "edge"
end

module RDSemilattice=
struct
	type t = L of BitVector.t | Bottom
	
	let top = L BitVector.empty (* Make sure this is the right way. How should we provide a way to list all statements in an application *)
	
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

module RDAdapter=
struct
	module P = Repr
	module L = Semilattice.Make(RDSemilattice)
	module S = Statement
	module BV = BitVector
	module BVE = BitVectorElement
	
	let flow node l = 
		let statements = node in (* Node statements *)
		let gen_bv = DFAHelper.gen statements in (* GEN *)
		let kill_bv = DFAHelper.kill statements in (* KILL *)
		let rec process_recursively input gen_v kill_v result = match (input, gen_v, kill_v) with
			| ([], [], []) -> result
			| (input_hd::input_tl, gen_hd::gen_tl, kill_hd::kill_tl) ->
				let new_result = (BVE.var input_hd, (BVE.state gen_hd	|| (BVE.state input_hd && BVE.state kill_hd)))::result
				in process_recursively input_tl	gen_tl	kill_tl new_result
		in
		match l with
			| RDSemilattice.Bottom -> RDSemilattice.Bottom
			| RDSemilattice.L (l_vector) ->	RDSemilattice.L(process_recursively l_vector gen_bv kill_bv [])
	
	let init _ = L.top 
end

module RDResults (A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge)=
struct
	module RDAdapter'=ProgramView.ForwardAdapter(RDAdapter)
	module PView=ProgramView.Make(RDAdapter')(A)(G)
	module Analyze = DFAEngine.RevPost (PView) (DFST.Make (G))
	
	type rdInfo = DFACommon.BitVector.t
	
	let before n = match G.ins n with
		| [] ->
      printf "No incoming edges\n"; 
      DFACommon.BitVector.empty
		| hd::_ ->
      printf "Calling Analyze.get\n"; 
      match Analyze.get hd with
			| RDSemilattice.L v ->
        printf "Lattice element encountered\n"; 
        v
			| RDSemilattice.Bottom ->
        printf "Bottom element encountered\n"; 
        DFACommon.BitVector.empty
	
	let after n = match G.outs n with
		| [] ->
      printf "No outgoing edges\n"; 
      DFACommon.BitVector.empty
		| hd::_ ->
      printf "Calling Analyze.get\n"; 
      match Analyze.get hd with
			| RDSemilattice.L v ->
        printf "Lattice element encountered\n"; 
        v
			| RDSemilattice.Bottom -> DFACommon.BitVector.empty
end