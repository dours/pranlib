(*
 * Alts: alt finding algorithms.
 * Copyright (C) 2005, 2006
 * Sergey Galanov, St.Petersburg State University
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

(** {1 Alt (single-entry region) construction and manipulation} *)

(** Alt finding constructor *)
module Make (D: DFST.Sig) : 
  sig

    (** The DFST module *)
    module T : DFST.Sig with module G = D.G

    (** The graph module *)
    module G : Digraph.Sig with type t = D.G.t and module Node = D.G.Node and module Edge = D.G.Edge

    (** The graph data *)
    val graph : G.t

    (** The starting node *)
    val start : G.Node.t

    (** Maximal alt module type *)
    module type MA = 
      sig

        (** [get node] gets maximal alt with header [node] using default hooks *)
	val get : G.Node.t -> G.Node.t list

      end

    (** Basic algorithm for finding maximal alt *)
    module MA : MA

    (** Maximal alts hierarchy finding module *)
    module HIER : 
      sig

        (** Dominance tree *)
        module Tree : Tree.Tree with type t = G.Node.t

        (** Maximal alt *)
        module MA : MA

        (** Hierarchy printer *)
        val toDOT : unit -> string

      end

    (** Create a maximal alt module from dominance tree *)
    module MAFromDom (T: Tree.Tree with type t = G.Node.t) : MA

  end
