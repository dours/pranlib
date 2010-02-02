(*
 * DDGStructure: Interface, describing the underlying structure of DDG
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
module type DDGNodeInfoSig=
sig
	module V : DDAInfo.VariableSig
	module S : DDAInfo.StatementSig with module V = V
	
	type t
	
	val make : S.t -> t
end

module type DDGEdgeTypeSig=
sig
	type t
	
	val get_true : unit -> t
	val get_anti : unit -> t
	val get_input : unit-> t
	val get_output : unit -> t
	
	val is_true : t -> bool
	val is_anti : t -> bool
	val is_input : t -> bool
	val is_output : t -> bool
end

module type DDGEdgeInfoSig=
sig
	module T : DDGEdgeTypeSig
	
	type t
	
	(* [get_type e] returns the edge's type *)
	val get_type : t -> T.t
	
	(* [equals e1 e2] checks if 2 edges are equal *)
	val equals : t -> t -> bool
end

module type ISGPointSig=
sig
	module V : DDAInfo.VariableSig
	module IVV : DDAInfo.IterationVariableValueSig with module V = V
	module T : DDGEdgeTypeSig
	module ISGEdge : DDGEdgeInfoSig with module T = T
		
	type t
	
	(* [make_point ivl] creates an ISG point with specified iteration variable values *)
	val make_point : (IVV.t * int) list -> t
	
	(* [get_ivalue isgp v] returns the value of given iteration variable for specified point *)
	val get_ivalue : t -> V.t -> IVV.t 
	
	(* [get_ivalues p] returns the list of values with nesting levels *)
	val get_ivalues : t -> (IVV.t * int) list
	
	(* [incoming_edges p] returns the list of dependency edges coming into given point *)
	val incoming_edges : t -> ISGEdge.t list
	
	(* [outgoing_edges p] returns the list of dependency edges coming out of given point *)
	val outgoing_edges : t -> ISGEdge.t list
	
	(* [add_incoming_edge p e] adds an edge to the list of incoming edges *)
	val add_incoming_edge : t -> ISGEdge.t -> t
	
	(* [add_outgoing_edge p e] adds an edge to the list of outgoing edges *)
	val add_outgoing_edge : t -> ISGEdge.t -> t
end

module type ISGSig=
sig
	module V : DDAInfo.VariableSig
	module IVV : DDAInfo.IterationVariableValueSig with module V = V
	module L : DDAInfo.LoopSig with module V = V and module IVV = IVV
	module T : DDGEdgeTypeSig
	module E : DDGEdgeInfoSig with module T = T
	module P : ISGPointSig with module V = V and module IVV = IVV and module T = T and module ISGEdge = E
	
	type t
	
	(* [iteration_variables isg] returns all iteration variables with corresponding nesting levels *)
	val iteration_variables : t -> (V.t * int) list
	
	(* [loops isg] returns all loops with corresponding nesting levels *)
	val loops : t -> (L.t * int) list
	
	(* [nesting_deepness isg] returns the deepness of the loop nest *)
	val nesting_deepness : t -> int
	
	(* [source e] returns the source of the dependency edge *)
	val source : t -> E.t -> P.t
	
	(* [dest e] returns the destination of the dependency edge *)
	val dest : t -> E.t -> P.t
	
	(* [iterate p v] retrieves the next point given the current point by iterating over the designated variable *)
	val iterate : t -> P.t -> V.t -> P.t
	
	(* [add_edge isg e s d] adds edge from s to d and returns updates isg, s and d*)
	val add_edge : t -> E.t -> P.t -> P.t -> (t * P.t * P.t)
	
	(* [make l] creates an ISG for given loop nest *)
	val make : L.t -> t
end
	
module type Sig=
sig
	module Info : DDAInfo.Sig
	module N : DDGNodeInfoSig
	module T : DDGEdgeTypeSig
	module E : DDGEdgeInfoSig with module T = T
	module P : ISGPointSig with module V = Info.Variable and module IVV = Info.Loop.IVV and module T = T and module ISGEdge = E
	module ISG : ISGSig with module V = Info.Variable and module IVV = Info.Loop.IVV and module L = Info.Loop and module T = T and module E = E and module P = P
	
	module G : CFG.Sig with type Node.info = Info.NodeInfo.t and type Edge.info = Info.EdgeInfo.t
	
	type t
	
	val start : t -> N.t
	
	val finish : t -> N.t
	
	val make : G.t -> t 
end

module Make(I:DDAInfo.Sig)(G:CFG.Sig with type Node.info = I.NodeInfo.t and type Edge.info = I.EdgeInfo.t) : Sig with module Info = I and module G = G 