(*
 * CFO: control flow optimizations.
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

(** {1 Interface to simple Control-Flow Optimizations} *)

(** Helper type to specify an interface to program under optimization *)
module type Helper =
  sig

    (** Type of the node label info *)
    type t

    (** Merge function *)
    val merge : t -> t -> t

    (** Emptyness check *)
    val empty : t -> bool
     
  end

(** Instantiation functor *)
module Make (D : DFST.Sig) (H : Helper with type t = D.G.Node.info) :
  sig

    (** Returns the list of straight lines *)
    val straightLines : unit -> D.G.Node.t list list

    (** Returns graph with collapsed straight lines, its starting node, and number
        of nodes merged. [H.merge] is used to merge node labels across straight lines. Important
        note: the graph is modified in-place so all attached data (for example its DFST) 
        may become invalid *)
    val collapseStraightLines : unit -> (D.G.t * D.G.Node.t) * int

    (** Removes unreachable code. Returns updated graph, its starting node, and
        number of removed nodes. Important note: the graph is modified in-place so all attached 
        data (for example its DFST) may become invalid *)
    val removeUnreachableCode : unit -> (D.G.t * D.G.Node.t) * int 

    (** Removes empty nodes. [H.empty] is used as check function.
        Returns updated graph, its starting node, and number of removed nodes. Important
        note: the graph is modified in-place so all attached data (for example its DFST) 
        may become invalid *)
    val removeEmptyNodes : unit -> (D.G.t * D.G.Node.t) * int

  end    
