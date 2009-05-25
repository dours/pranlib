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
(** Upwards-exposed uses analysis bit vector element *)
module BitVectorElement:
sig
	type t = DFACommon.Variable.t
	
	val equal : t -> t -> bool
end

(** Upwards-exposed uses analysis bit-vector - a semilattice element *)
module BitVector:
sig
	type t = BitVectorElement.t list
	
	val empty : t
	
	val cap : t -> t -> t
	
	val equal : t -> t -> bool
end

(** Upwards-exposed uses analysis semilattice *)
module UEUSemilattice:
sig
	include Semilattice.Base
end

(** Node information *)
module UNodeInfo:
sig
	(** Type of node information *)
	type t = DFACommon.Statement.t 
end

(** Edge information *)
module UEdgeInfo:
sig
	(** Type of edge information *)
	type t = Empty
end

(** Upwards-exposed uses analysis results construction functor *)
module UEUResults (A: ProgramView.Abstractor with
                    type Abstract.node = UNodeInfo.t and
                    type Abstract.edge = UEdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge):
sig
	(** Upwards-exposed uses analysis information *)
	type ueuInfo = BitVector.t
	
	(** [before n] returns upwards-exposed uses information before executing node n *)
	val before : G.Node.t -> ueuInfo
	
	(** [after n] returns upwards-exposed uses information after executing node n *)
	val after : G.Node.t -> ueuInfo 
end

