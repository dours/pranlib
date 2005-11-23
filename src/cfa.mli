(*
 * Cfa: control flow analysis library.
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
 * (enclosed in the file COPYING).
 *)

(** Control flow analysis facilities *)

val fold_num : ('a -> int -> 'a) -> 'a -> int -> 'a
(** [fold_num func init n] calculates value of [(func (n-1) (func 1 (func 0 init))...)] *)

(** Minimal graph signature for depth-first search tree construction *)
module type G_DFST =
    sig

      (** Module to denote graph nodes *)
      module Node :
	  sig

	    type t
               (** Type of the node *)

	    val hash : t -> int
               (** Hash function *)

	    val equal : t -> t -> bool
               (** Equality *)

	    val toString : t -> string
               (** Text representation of the node *)
	  end

      (** Module to denote graph edgess *)
      module Edge :
	  sig

	    type t
               (** Type of the edge *)

	    val hash : t -> int
               (** Hash function *)

	    val equal : t -> t -> bool
               (** Equality *)

	    val toString : t -> string
               (** Text representation of the edge *)
	  end

      type t 
         (** Type of the graph *)

      val nnodes : t -> int
	  (** [nnodes graph] returns number of nodes in the [graph]*)

      val nedges : t -> int
	  (** [nedges graph] returns number of edges in the [graph]*)

      val dst : Edge.t -> Node.t
	  (** [dst edge] returns destination node of the [edge] *)

      val outs : Node.t -> Edge.t list
	  (** [outs node] returns list of outgoing edges of the [node] *)

      val print : t -> (Node.t -> string) * (Edge.t -> string) -> string
	  (** [print graph (nodeString, edgeString)] creates textual representation of the graph in
             DOT program format using [nodeString] and [edgeString] as functions to print labels of
             nodes and edges correspondingly *)

    end

(** Depth-Ffirst Search Tree *)
module DFST (G : G_DFST) :
    sig

      type sort = Tree | Forward | Back | Cross
          (** Sort of the edge *)

      type info = 
          (** DFST info structure *)
      {
        pre    : G.Node.t -> int;  (** [pre node] returns preorder number of the [node] *)
        post   : G.Node.t -> int;  (** [post node] returns postorder number of the [node] *)

        pre'1  : int -> G.Node.t;  (** [pre'1 num] returns node whose preorder number is [num] *)
        post'1 : int -> G.Node.t;  (** [post'1 num] returns node whose postorder number is [num] *)

        sort   : G.Edge.t -> sort; (** [sort edge] returns DFST sort of the [edge] *)

        graph  : G.t;              (** returns graph this info was built for *)
        start  : G.Node.t;         (** returns start node of the graph this info was built for *)

        nodeString : G.Node.t -> string; (** [nodeString node] returns textual representation of the [node] 
                                             comprising textual representation of the label,  pre- and 
                                             postorder numbers *)

        edgeString : G.Edge.t -> string; (** [edgeString edge] returns textual representation of the [edge] comprising
                                             textual representation of the label and sort *)

        toString   : unit -> string;     (** returns textual representation of the graph and its DFST in DOT program format *)

        replaceNode : G.Node.t -> G.Node.t -> unit; (** [replaceNode node node'] updates DFST info to maintain consistency 
                                                        after [! G.replaceNode] call *)

	replaceEdge : G.Edge.t -> G.Edge.t -> unit; (** [replaceEdge edge edge'] updates DFST info to maintain consistency 
                                                        after [! G.replaceEdge] call *)
      }

      val create : G.t * G.Node.t -> info
         (** [create (graph, start)] returns DFST info built for [graph] with starting node [start] *)

    end

(** Minimal graph signature for control flow optimizations *)
module type G_CFO =
    sig

      (** Module to denote graph nodes *)
      module Node :
	  sig

	    type t
               (** Type of the node *)

	    type info
               (** Type of the node label *)

	    val info : t -> info
               (** [info node] returns label of the [node] *)

	    val hash : t -> int
               (** Hash function *)

	    val equal : t -> t -> bool
               (** Equality *)

	    val toString : t -> string
               (** Text representation of the node *)
	  end

      (** Module to denote graph edgess *)
      module Edge :
	  sig

	    type t
               (** Type of the edge *)

	    type info
               (** Type of the edge label *)

	    val info : t -> info
               (** [info edge] returns label of the [edge] *)

	    val hash : t -> int
               (** Hash function *)

	    val equal : t -> t -> bool
               (** Equality *)

	    val toString : t -> string
               (** Text representation of the edge *)
	  end

      type t 
         (** Type of the graph *)

      val nnodes : t -> int
	  (** [nnodes graph] returns number of nodes in the [graph]*)

      val nedges : t -> int
	  (** [nedges graph] returns number of edges in the [graph]*)

      val src : Edge.t -> Node.t
          (** [src edge] returns origin node of the [edge] *)

      val dst : Edge.t -> Node.t
	  (** [dst edge] returns destination node of the [edge] *)
   
      val ins  : Node.t -> Edge.t list
          (** [ins node] returns list of incoming edges of the [node] *)

      val outs : Node.t -> Edge.t list
	  (** [outs node] returns list of outgoing edges of the [node] *)

      val print : t -> (Node.t -> string) * (Edge.t -> string) -> string
	  (** [print graph (nodeString, edgeString)] creates textual representation of the graph in
             DOT program format using [nodeString] and [edgeString] as functions to print labels of
             nodes and edges correspondingly *)

      val insertEdge  : t -> Node.t -> Node.t -> Edge.info -> t * Edge.t
          (** [insertEdge graph src dst info] inserts new edge from [src] to [dst] with label [info] 
              into [graph] and returns updated graph and created edge *)

      val deleteNode  : t -> Node.t -> t
          (** [deleteNode graph node] deletes [node] from [graph] and returns 
              updated graph *)

      val deleteNodes : t -> (Node.t -> bool) -> t
          (** [deleteNodes graph func] deletes all nodes sutisfying [func] from [graph] and returnds
              updated graph *)

      val replaceNode : t -> Node.t -> Node.info -> t * Node.t
	  (** [replaceNode graph node info] replaces [node] in [graph] with fresh node with label [info]
	      preserving all adjacent edges. Returns updated graph and created node *)
    
    end

(** Node info service functions *)
module type IS =
  sig

    type t
       (** Type of info *)

    val merge : t -> t -> t
       (** Merge function *)
   
    val empty : t -> bool
       (** Empty info detector *)
  end

(** Control Flow Optimizations *)
module Optimize (G : G_CFO) (S : IS with type t = G.Node.info) :
    sig

      type info = DFST(G).info

      val collapseStraightLines : info -> G.Node.t list list -> info * int
         (** [collapse info lines] merges each basic block of [info.graph], passed as [lines], into 
             single node; Returns rebuilt DFST info and number of merged nodes *)

      val removeEmptyNodes : info -> info * int
         (** [removeEmptyNodes info] removes from [info.graph] empty nodes as detected by [S.empty] and returns 
             rebuilt DFST info and number of removed nodes *)

      val detectStraightLines : info -> G.Node.t list list
         (** [detectStraightLines info] detects linear sequencies of nodes ({i basic blocks}) in the [info.graph] 
             and returns list of them *)

      val removeUnreachableCode : info -> info * int
         (** [removeUnreachableCode info] removes nodes of [info.graph], unreachable from [info.start] and returns
             rebuilt DFST info and number of removed nodes *)

    end

(** Strongly Connected Components*)
module SCC (G : Digraph.Sig) :
    sig

      type dfst = DFST(G).info
         (** Type of DFST info for given graph *)

      type info = 
	 (** SCC info structure *)     
      {
        sccs     : unit -> (G.Node.t * G.Node.t list) list; 
          (** returns list of pairs (v{_i}, S{_i}) where S{_i} is strongly connected component,
              v{_i} - its node with minimum postorder number *)
        dfst     : dfst;
          (** returns DFST info for graph SCC info was built for *)  
        toString : unit -> string;
          (** returns textual graph representation in DOT program format clustered according to SCC splitting *)
      }

      val detectSCS  : dfst -> G.Node.t -> G.Node.t list
         (** [detectSCS info node] returns strongly connected subgraph for [node]. Returned subgraph consists of
             all nodes with postorder number greater or equal that that of [node] such that [node] is reachable from all 
             of them *)
 
      val detectSCCs : dfst -> info
         (** [detectSCCs info] detects strongly connected components for [info.graph] *)

    end

(** Minimal Path Covering *)
module PathCovering (G : Digraph.Sig) :
    sig

      type dfst = DFST(G).info
         (** Type of DFST info for given graph *)

      type info = 
         (** Minimal path covering info structure *)
      {
        dfst     : dfst;                  (** returns DFST info for graph PathCovering info was built for *)
        edges    : unit -> G.Edge.t list; (** returns list of edges comprising minimal path covering *)
	nodes    : unit -> G.Node.t list; (** returns list of isolated nodes *)
        toString : unit -> string;        (** returns textual representation of the graph with its minimal path covering *)
      }

      val create : dfst -> info
         (** [create info] returns PathCovering info built for [info.graph]. This covering is minimal unless graph 
             is non-reducible *)

      type path = Single of G.Node.t | Path of (G.Node.t * G.Edge.t) list
         (** Path representation. Each path is either a single node or a list of pairs (node, edge), where node is node in 
             the path, edge is outgoing edge to the next node *)

      val linearize : info -> path list 
         (** [linearize info] returns list of reconstructed from [info] paths. *)
    end
