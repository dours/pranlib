(*
 * UEU: implements 'upwards-exposed uses' data flow analysis problem
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

(** Variable - a bit vector element for UEU *)
module BitVectorElement=
struct
	type t = DFACommon.Variable.t
	
	let equal x y = DFACommon.Variable.equals x y
end

module BitVector=
struct
	type t = BitVectorElement.t list
	
	let empty = []
	
	let cap x y = 
		let rec cap' x y result = 
			match x with 
				| [] -> result
				| hd::tl -> if List.exists (fun a -> BitVectorElement.equal a hd) y then cap' tl y (hd::result)
										else cap' tl y result
		in cap' x y []
	
	let equal x y = 
		let rec check_recursively x y = 
			match x with
				| [] -> true
				| hd::tl -> if List.exists (fun a -> BitVectorElement.equal a hd) y then check_recursively tl y
										else false
		in
		(check_recursively x y) && ((List.length x)==(List.length y))
end

module UNodeInfo=
struct
	type t = DFACommon.Statement.t
end

module UEdgeInfo=
struct
	type t = Empty
end

module UEUSemilattice=
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
	type node = UNodeInfo.t
	type edge = UEdgeInfo.t
end

module UEUAdapter=
struct
	module P = Repr
	module L = Semilattice.Make(UEUSemilattice)
	module S = DFACommon.Statement
	module BV = BitVector
	module BVE = BitVectorElement
	module V = DFACommon.Variable
	
	let init _ = L.top
	
	let flow node l =
		let statements = node in
		let update rp =
		let rec update' variables bv = 
					let update'' v bv = 
						if List.exists (fun bve -> V.equals bve v) bv then bv
						else v::bv
					in
					match variables with 
						| [] -> bv
						| hd::tl -> update'' hd (update' tl bv)
					in
				update' rp BV.empty
		in 
		let use s =
			match s with
				| S.Other -> BV.empty
				| S.Assignment (lp, rp) -> 
				update (List.concat rp)
		in
		let def s =
			match s with
				| S.Other -> BV.empty
				| S.Assignment (lp, rp) ->
				update lp
		in
		let calculate_recursively bv d u = 
			let add_if_not_exists v lst = if List.exists (fun x -> V.equals x v) lst then lst
																	else v::lst
			in
			let remove_if_exists v lst = 
				let rec remove_if_exists' l result =
					match l with
						| [] -> result
						| hd::tl -> if V.equals v hd then remove_if_exists' tl result
												else remove_if_exists' tl (hd::result)
				in
			remove_if_exists' lst []
			in
				let out_minus_def o df =
					let rec out_minus_def' o df result =
						match o with
							| [] -> result
							| hd::tl -> if List.exists (fun x -> V.equals x hd) df then out_minus_def' tl df result
													else out_minus_def' tl df (hd::result)
					in
					out_minus_def' o df []  
			in
			let rec use_plus_omd u omd = 
				match u with 
					| [] -> omd
					| hd::tl -> if List.exists (fun x -> V.equals x hd) omd then use_plus_omd tl omd
											else use_plus_omd tl (hd::omd)
			in
			use_plus_omd u (out_minus_def bv d)
		in
		match l with
			| UEUSemilattice.Bottom -> UEUSemilattice.Bottom
			| UEUSemilattice.L l_element -> UEUSemilattice.L (calculate_recursively l_element (def statements) (use statements))
end

module UEUResults (A: ProgramView.Abstractor with
                    type Abstract.node = UNodeInfo.t and
                    type Abstract.edge = UEdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge)=
struct
	module UEUAdapter'=ProgramView.BackwardAdapter(UEUAdapter)
	module PView=ProgramView.Make(UEUAdapter')(A)(G)
	module Analyze = DFAEngine.RevPost (PView) (DFST.Make (G))
	
	type ueuInfo = BitVector.t
	
	let before n = match G.ins n with
		| [] -> BitVector.empty
		| hd :: _ -> match Analyze.get hd with
			| UEUSemilattice.L v -> v
			| UEUSemilattice.Bottom -> BitVector.empty

	let after n = match G.outs n with
		| [] -> BitVector.empty
		| hd :: _ -> match Analyze.get hd with
			| UEUSemilattice.L v -> v
			| UEUSemilattice.Bottom -> BitVector.empty
end