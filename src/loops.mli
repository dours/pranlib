(*
 * Loops: Loops finding algorithms.
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

(** Loops: Loops finding algorithms. *)

open Treebuilder

(** Loops *)
module LOOP (G    : Digraph.Sig) 
           (UFS  : Unionfind.S with type elt=G.Node.t)  
           (UFSI : Unionfind.S with type elt=int) :
sig

  module NODE_TREE_BUILDER:(Treebuilder.SIG with type t = G.Node.t)
  (** Nested loops tree construction. [Node] representation. *)

  module INT_TREE_BUILDER:(Treebuilder.SIG with type t = int)
  (** Nested loops tree construction. [int] representation. *)

  type dfst = Cfa.DFST(G).info 
  (** Depth first search tree of a graph *)

  type info = NODE_TREE_BUILDER.info_out
  (** Loops tree type. [Node] representation *)

  type info_i = INT_TREE_BUILDER.info_out
  (** Loops tree type. [int] representation *)

  val loops_havlak: G.t -> dfst -> info
  (** [Havlak] loops finding algorithm *)
   
  val loops_havlak_i: G.t -> dfst -> info
  (** Improved [Havlak] loops finding algorithm *)
   
  val loops_gao_lee : G.t -> dfst -> info_i
  (** [Sreedhar-Gao-Lee] loops finding algorithm *)   

end
