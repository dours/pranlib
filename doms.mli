(*
 * Doms: Dominance tree contruction.
 * Copyright (C) 2005
 * Serjic Shkredov, St.Petersburg State University
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

(** Dominance tree construction *)
open Treebuilder

(** Dominance tree *)
module DOM (G: Digraph.Sig) :
sig

  type dfst = Cfa.DFST(G).info
  (** Depth first search tree type *)

  module INT_TREE_BUILDER : Treebuilder.SIG with type t = int
  (** Tree builder module. Creates tree stucture using algorithm specific information. *)

  type info = INT_TREE_BUILDER.info_out
  (** Type of the dominance tree *)

  val create_dfs : G.t -> G.Node.t -> info
  (** Creates dominance tree using [graph] and [start node] of graph *)

  val create : G.t -> dfst -> info
  (** Creates dominance tree using [graph] and [deep first search tree] of graph *)

end
