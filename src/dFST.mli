(*
 * DFST: Depth-First Search Tree Construction.
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

(** {1 Depth-First Search Tree construction and manipulation} *)

(** Sort of the edge with regard to DFST *)
type sort = Tree | Forward | Back | Cross

(** General signature for DFST *)
module type Sig =
  sig

    (** A graph module the DFST is built for *)
    module G : Digraph.Sig

    (** Exception raised when the DFST properties are queried for the unreachable 
        node/edge *)
    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
 
    (** Exception raised when parameter for the [pre'1] or [post'1] goes out of
        the valid range *)
    exception RangeError of int

    (** [reachedNode node] tests whether [node] is reached after DFST is built *)
    val reachedNode : G.Node.t -> bool

    (** [reachedEdge edge] tests whether [edge] is reached after DFST is built *)
    val reachedEdge : G.Edge.t -> bool

    (** [isValid n] checks whether [n] is a valid number to be used as a parameter 
        for [pre'1] or [post'1] *)
    val isValid : int -> bool

    (** [pre node] returns preorder number of the [node] or raises [Unreachable] exception 
        if [node] is unreachable *)
    val pre : G.Node.t -> int  

    (** [post node] returns postorder number of the [node] or raises [Unreachable] exception 
        if [node] is unreachable *) 
    val post : G.Node.t -> int  

    (** [pre'1 num] returns node whose preorder number is [num]. Raises [RangeError] if
        no such node exists *)
    val pre'1 : int -> G.Node.t  

    (** [post'1 num] returns node whose postorder number is [num]. Raises [RangeError] if
        no such node exists *)
    val post'1 : int -> G.Node.t  

    (** [sort edge] returns DFST sort of the [edge] or raises [Unreachable] exception 
        if [edge] is unreachable*) 
    val sort : G.Edge.t -> sort 

    (** Returns graph the DFST was built for *)
    val graph : G.t              

    (** Returns the starting node of the graph *)
    val start : G.Node.t

    (** Type of tree node *) 
    type t = G.Node.t

    (** Root of the DFST *)
    val root : t

    (** Parent function *)
    val parent : t -> t option

    (** Children function  *)
    val children : t -> t list

    (** DOT visualizer *)
    module DOT :
      sig

        (** Node wrapper *)
	module Node : DOT.Node with type t = G.Node.t

        (** Edge wrapper *)
	module Edge : Digraph.DOT.Edge with type t = G.Edge.t

	include Digraph.DOT.S with type graph = G.t and type node = G.Node.t and type edge = G.Edge.t and type parm = unit

      end

  end

(** Depth-First Search Tree Constructor *)
module Make (G : Digraph.Sig) (X : sig val graph : G.t val start : G.Node.t end) : Sig with module G = G
