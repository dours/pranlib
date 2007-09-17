(*
 * Digraph: basic directed graph implementation.
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

(** {1 Common signature and implementation functor for directed graphs} *)

(** Item module type; item contains some info and can be printed, hashed and compared *)
module type Item =
  sig

    (** Type of the item *)
    type t

    (** Type of the item info *)
    type info

    (** [toString item] returns string representation of the [item] *)
    val toString : t -> string

    (** [hash item] returns hash value of the [item] *)
    val hash : t -> int

    (** [equal item item'] returns [true] iff [item] is the same as [item'] *)
    val equal : t -> t -> bool

    (** [info item] returns label of the [item] *)
    val info : t -> info

    (** [compare item item'] compares [item] to [item'] *)
    val compare : t -> t -> int
 
    (** Integer key *)
    val index : t -> int
 
  end

(** Tiny interface to GraphViz graph drawing library. See DOT language specification for details *)
module DOT :
  sig

    (** DOT graph node wrapper *)
    module type Node =
      sig
	
	include DOT.Node

      end

    (** DOT graph edge wrapper *)
    module type Edge =
      sig

        (** Type for edge representation *)
	type t

        (** Gets list of edge attributes *)
	val attrs : t -> (string * string) list

        (** Gets label of the edge *)
	val label : t -> string
          
      end

    (** Signature of DOT digraph printer *)
    module type S =
      sig

	include DOT.Sig

        (** Type of the edge *)
	type edge

        (** DOT represenation for edge *)
	val edge  : edge -> string

        (** DOT representation for list of edges *)
	val edges : edge list -> string

        (** Type of vizualizer parameter *)
	type parm 

        (** DOT visualizer *)
	val toDOT : parm -> string

        (** DOT clustered visualizer *)
	module Clustered :
	  sig

	    (** Visualizing graph with clusters *)
	    val toDOT : parm -> Clusters.t -> string

	  end

      end

    module Printer (G : DOT.Graph) (N : Node) : DOT.Sig with type graph = G.t and type node = N.t

  end

(** Basic directed graph signature *)
module type Base =
  sig

    module Node : Item (** Module to denote graph nodes *)
    module Edge : Item (** Module to denote graph edges *)

    (** Type of the graph *)
    type t

    (** {2 Access Functions} *)

    (** [src edge] returns origin node of the [edge] *)
    val src : Edge.t -> Node.t

    (** [dst edge] returns destination node of the [edge] *)
    val dst : Edge.t -> Node.t

    (** [ins node] returns list of incoming edges of the [node] *)
    val ins : Node.t -> Edge.t list

    (** [outs node] returns list of outgoing edges of the [node] *)
    val outs : Node.t -> Edge.t list

    (** [pred node] returns list of predecessors for [node] *)
    val pred : Node.t -> Node.t list

    (** [succ node] returns list of successors for [node] *)
    val succ : Node.t -> Node.t list

    (** [nnodes graph] returns number of nodes in the [graph]*)
    val nnodes : t -> int

    (** [nedges graph] returns number of edges in the [graph]*)
    val nedges : t -> int

    (** [nodes graph] returns list of all nodes in the [graph]*)
    val nodes : t -> Node.t list

    (** [nodes graph] returns list of all edges in the [graph]*)
    val edges : t -> Edge.t list

    (** [lastEdge graph] returns maximal index over all [graph] edges *)
    val lastEdge : t -> int

    (** [lastNode graph] returns maximal index over all [graph] nodes *)
    val lastNode : t -> int
  
    (** {2 Create Functions} *)

    (** creates empty graph *)
    val create : unit -> t

    (** [insertNode graph info] inserts new node with label [info] into [graph] and returns
        updated graph and created node 
    *)
    val insertNode : t -> Node.info -> t * Node.t

    (** [insertEdge graph src dst info] inserts new edge from [src] to [dst] with label [info] 
        into [graph] and returns updated graph and created edge 
    *)
    val insertEdge : t -> Node.t -> Node.t -> Edge.info -> t * Edge.t

    (** [deleteEdges graph func] deletes all edges satisfying [func] from [graph] and returns
        updated graph 
    *)
    val deleteEdges : t -> (Edge.t -> bool) -> t

    (** [deleteEdge graph edge] deletes [edge] from [graph] and returns 
        updated graph. Deleting edge that does not belong to the graph has no effect 
    *)
    val deleteEdge : t -> Edge.t -> t

    (** [deleteNodes graph func] deletes all nodes sutisfying [func] from [graph] and returnds
        updated graph 
    *)
    val deleteNodes : t -> (Node.t -> bool) -> t

    (** [deleteNode graph node] deletes [node] from [graph] and returns 
        updated graph. Deleting node that does not belong to the graph has no effect 
    *)
    val deleteNode : t -> Node.t -> t

    (** {2 Update Functions} *)

    (** [replaceNode graph node info] replaces [node] in [graph] with fresh node with label [info]
        preserving all adjacent edges. Returns updated graph and created node.  Replacing node that
        does not belong to the graph raises [Failure "node does not belong to the graph"] 
    *)
    val replaceNode : t -> Node.t -> Node.info -> t * Node.t

    (** [replaceEdge graph edge info] replaces [edge] in [graph] with fresh edge with label [info]
        preserving adjacent nodes. Returns updated graph and created edge. Replacing edge that
        does not belong to the graph raises [Failure "edge does not belong to the graph"] 
    *)
    val replaceEdge : t -> Edge.t -> Edge.info -> t * Edge.t

    (** Copying function. Beware of mutable node/edge info *)
    val copy : t -> t

  end

(** Directed graph signature *)
module type Sig =
  sig

    include Base

    (** DOT printer *)
    module DOT : DOT.S with type node = Node.t and type graph = t

    (** Exported DOT vizualizer *)
    val toDOT : t -> string

    (** Exported DOT clustered vizualizer *)
    module Clustered :
      sig

	(** Clustered DOT printer *)
	val toDOT : t -> DOT.Clusters.t -> string

      end

  end

(** {2 Directed graph instantiation} *)

(** Signature of labels *)
module type Info = 
  sig 

    (** Type of the label *)
    type t 

    (** [toString label] returns string representation of the [label] *)
    val toString : t -> string 

  end

(** Instantiation functor for directed graph. [Make(NodeLabel)(EdgeLabel)] creates {b mutable} 
    implementation of directed graph with nodes and edges labeled with values of types [NodeLabel] and 
    [EdgeLabel] correspondingly 
 *)
module Make (NodeInfo : Info) (EdgeInfo : Info): Sig with type Node.info = NodeInfo.t and type Edge.info = EdgeInfo.t

(** Functor to construct graph printers *)
module Printer 
    (G : Base) 
    (N : DOT.Node with type t = G.Node.t) 
    (E : DOT.Edge with type t = G.Edge.t) : DOT.S with 
  type graph = G.t and type node = N.t and type edge = E.t and type parm = G.t


