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
open DFACommon

(** Element of a bit-vector *)
module RBitVectorElement : 
sig
  (** Type of bit vector element. Reaching definitions analysis works on bit vectors consisting of statements *)
	type t = Statement.t * bool
  
  val construct : Statement.t -> bool -> t
	
	val and_op : t -> t -> t
	
	val or_op : t -> t -> t
	
	val equal : t -> t -> bool
	
	val statement : t -> Statement.t
	
	val state : t -> bool
	
	val toString : t -> string 
end

(** Bit-vector *)
module RBitVector: 
sig
	type t = RBitVectorElement.t list
	
	val empty : t
  
  val cap : t -> t -> t
	
	val equal : t -> t -> bool
  
  val lookup_element : Statement.t -> t -> RBitVectorElement.t option
	
	val toString : t -> string  
end

(** Semilattice *)
module RDSemilattice (A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge):
sig
	include Semilattice.Base
  
  val make_empty : bool -> RBitVector.t
  
  val make_empty_top : unit -> RBitVector.t
  
  val make_empty_bottom : unit -> RBitVector.t
end			

(** Reaching definitions analysis results module *)
module RDResults (A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge) :
sig
	(** Reaching definitions information (Basically a bit-vector) *)
	type rdInfo = RBitVector.t
	
	(** [before n] returns reaching definitions information before executing node statements *)
	val before : G.Node.t -> rdInfo
	
	(** [after n] returns reaching definitions information after executing node statements *) 
	val after : G.Node.t -> rdInfo 
end