(*
 * Alts: alts formation algorithms.
 * Copyright (C) 2005
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
 * (enclosed in the file LGPL).
 *)

(** Alts finding algorithms *)

(** Maximal alt (single-entry subgraph) finding *)
module Alt (G: Digraph.Sig) : sig

    type info = G.Node.t * G.Node.t list
        (** Alt info - entry node and list of nodes *)

    type dfst = Cfa.DFST(G).info
        (** Depth-first search tree type *)

    val create : dfst -> G.Node.t -> info
        (** [create dfst entry] returns maximal alt info built for
         * graph with [dfst] built for it with entry node [entry] *)
end

(** Maximal alts hierarchy building module *)
open Treebuilder

module Hierarchy (G: Digraph.Sig) : sig

    module TREE_BUILDER:(Treebuilder.SIG with type t = Alt(G).info)
        (** Tree builder module for alts *)

    type dfst = Cfa.DFST(G).info
        (** Depth-first search tree type *)

    type info = 
        (** Maximal alts hierarchy structure *)
    { 
        dfst: dfst;
            (** Depth-first search tree for graph *)
        alts: TREE_BUILDER.info_out 
            (** Hierarchy itself *)
    }

    val create : dfst -> info
        (** [create dfst] returns maximal alts hierarchy built for 
          * graph which has [dfst] built for it *)

    val toString : info -> string
        (** [toString info] returns textual representation of the 
          * maximal alts hierarchy passed in [hinfo] *)

    val test : info -> bool
        (** [test info] tests whether maximal alts hierarchy [info] is 
          * correct *)

end
