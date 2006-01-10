(*
 * PathCovering: minimal path covering construction.
 * Copyright (C) 2004-2006
 * Dmitri Boulytchev, St.Petersburg State University
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

(** {1 Path Covering construction and manipulation} *)

(** Simple version: a minimal path covering in reducible graphs with no weights *)
module MakeSimple (D : DFST.Sig) :
  sig

    (** A path representer: either a single node or a list of nodes with attached outgoing edge *)
    type path = Single of D.G.Node.t | Path of (D.G.Node.t * D.G.Edge.t) list

    (** A list of edges in the covering *)
    val edges : unit -> D.G.Edge.t list 

    (** A list of separetedly selected nodes *)
    val nodes : unit -> D.G.Node.t list

    (** A list of all paths in the covering *)
    val paths : unit -> path list 
	
    (** Visualizes graph + its minimal covering *)
    val toDOT : unit -> string
	
  end
