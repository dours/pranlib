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
open DFACommon

(** Live variables analysis semilattice *)
module LVSemilattice :
sig
	include Semilattice.Base
end			

(** Node information *)
module NodeInfo :
sig
	(** Type of node information. Each node contains a list of statements *)
	type t = Statement.t list
	
	val toString : t -> string
end

(** Edge information *)
module EdgeInfo :
sig
	(** Type of edge information *)
	type t = Empty
	
	val toString : t -> string
end

(** Live variable analysis results creation functor *)
module LVResults (A: ProgramView.Abstractor with
                    type Abstract.node = DFACommon.Statement.t list and
										type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge) :
sig
	(** Type of live variables analysis information *)
	type lvInfo = DFACommon.BitVector.t
	
	(** [before n] Returns live variables informaton on entrance into node n *)
	val before : G.Node.t -> lvInfo 
	
	(** [after n] Returns live variables information on exit of node n *)
	val after : G.Node.t -> lvInfo
end