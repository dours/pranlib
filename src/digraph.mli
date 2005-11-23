(*
 * Graph: basic directed graph library.
 * Copyright (C) 2004
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
 * (enclosed in the file LGPL).
 *)

(** Common signature and implementation functor of directed graphs *)

(** {1 Directed graph signature} *)
module type Sig =
  sig

    (** Module to denote graph nodes *)
    module Node :
      sig

        type t
           (** Type of the node *)

        type info
           (** Type of the node label *)

        val toString : t -> string
           (** [toString node] returns string representation of the [node] *)

        val hash     : t -> int
           (** [hash node] returns hash value of the [node] *)

        val equal    : t -> t -> bool
           (** [equal node node'] returns [true] iff [node] is the same as [node'] *)

        val info     : t -> info
           (** [info node] returns label of the [node] *)

        val compare  : t -> t -> int
           (** [compare node node'] compares [node] to [node'] *)

      end

    (** Module to denote graph edges *)
    module Edge :
      sig

        type t
           (** Type of the edge *)

        type info
           (** Type of the edge label *)

        val toString : t -> string
           (** [toString edge] returns string representation of the [edge] *)

        val hash     : t -> int
           (** [hash edge] returns hash value of the [edge] *)

        val equal    : t -> t -> bool
           (** [equal edge edge'] returns [true] iff [edge] is the same as [edge'] *)

        val info     : t -> info
           (** [info edge] returns label of the [edge] *)

        val compare  : t -> t -> int
           (** [compare edge edge'] compares [edge] to [edge'] *)

      end

    type t
       (** Type of the graph *)

    (** {2 Access Functions} *)

    val src : Edge.t -> Node.t
       (** [src edge] returns origin node of the [edge] *)

    val dst : Edge.t -> Node.t
       (** [dst edge] returns destination node of the [edge] *)

    val ins  : Node.t -> Edge.t list
       (** [ins node] returns list of incoming edges of the [node] *)

    val outs : Node.t -> Edge.t list
       (** [outs node] returns list of outgoing edges of the [node] *)

    val nnodes : t -> int
       (** [nnodes graph] returns number of nodes in the [graph]*)

    val nedges : t -> int
       (** [nedges graph] returns number of edges in the [graph]*)

    val nodes  : t -> Node.t list
       (** [nodes graph] returns list of all nodes in the [graph]*)

    val edges  : t -> Edge.t list
       (** [nodes graph] returns list of all edges in the [graph]*)

    (** {2 Create Functions} *)

    val create : unit -> t
       (** creates empty graph *)

    val insertNode : t -> Node.info -> t * Node.t
       (** [insertNode graph info] inserts new node with label [info] into [graph] and returns
           updated graph and created node *)

    val insertEdge : t -> Node.t -> Node.t -> Edge.info -> t * Edge.t
       (** [insertEdge graph src dst info] inserts new edge from [src] to [dst] with label [info] 
           into [graph] and returns updated graph and created edge *)

    val deleteEdges : t -> (Edge.t -> bool) -> t
       (** [deleteEdges graph func] deletes all edges sutisfying [func] from [graph] and returnds
           updated graph *)

    val deleteEdge : t -> Edge.t -> t
       (** [deleteEdge graph edge] deletes [edge] from [graph] and returns 
           updated graph *)

    val deleteNodes : t -> (Node.t -> bool) -> t
       (** [deleteNodes graph func] deletes all nodes sutisfying [func] from [graph] and returnds
           updated graph *)

    val deleteNode : t -> Node.t -> t
       (** [deleteNode graph node] deletes [node] from [graph] and returns 
           updated graph *)

    (** {2 Update Functions} *)

    val replaceNode : t -> Node.t -> Node.info -> t * Node.t
       (** [replaceNode graph node info] replaces [node] in [graph] with fresh node with label [info]
	   preserving all adjacent edges. Returns updated graph and created node *)

    val replaceEdge : t -> Edge.t -> Edge.info -> t * Edge.t
       (** [replaceEdge graph edge info] replaces [edge] in [graph] with fresh edge with label [info]
	   preserving adjacent nodes. Returns updated graph and created edge *)

    (** {2 Print Functions} *)

    val print : t -> (Node.t -> string) * (Edge.t -> string) -> string
       (** [print graph (nodeString, edgeString)] creates textual representation of the graph in
           DOT program format using [nodeString] and [edgeString] as functions to print labels of
           nodes and edges correspondingly *)

    val printClustered : t -> Node.t list list -> (Node.t -> string) * (Edge.t -> string) -> string
       (** [printClustered graph clusters (nodeString, edgeString)] creates textual representation of the 
           graph in DOT program format using [nodeString] and [edgeString] as functions to print labels of
           nodes and edges correspondingly. Graph printed in clustered form according to [clusters] *)

    val toString : t -> string
       (** [toString graph] creates default textual representation of the [graph] in DOT program format *)

  end

(** {1 Directed graph instantiation} *)

(** Signature of labels *)
module type Info = 
  sig 

    type t 
       (** Type of the label *)

    val toString : t -> string 
       (** [toString label] returns string representation of the [label] *)

  end

(** Instantiation functor fro directed graph *)
module Make (NodeInfo : Info) (EdgeInfo : Info): Sig with type Node.info = NodeInfo.t and type Edge.info = EdgeInfo.t
(** [Make(NodeLabel)(EdgeLabel)] creates {b mutable} implementation of directed graph with nodes and edges labeled
    with values of types [NodeLabel] and [EdgeLabel] correspondingly *)
