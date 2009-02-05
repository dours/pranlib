(*
 * DOT: basic graph printing interface.
 * Copyright (C) 2006
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

(** {1 Simple interface to GraphViz graph drawing toolset} *)

(** Signature of an object information representation *)
module type Info = 
  sig
    (** Type of the object *)
    type t

    (** Label of the object *)
    val label : t -> string  

    (** List of object attributes *)
    val attrs : t -> (string * string) list
  end

(** Extended object information signature *)
module type ExtInfo =
  sig
    (** Underying information module *)
    include Info

    (** Name of the object *)
    val name  : t -> string 
  end

(** Module to provide no information *)
module Empty : Info with type t = unit

(** Module to provide type for cluster structure within a graph  *)
module Clusters : 
  sig
    (** type for a cluster forest *)
    type 'a t = 'a cluster list 
    and 'a cluster = Node of 'a * 'a t | Leaf of 'a
  end

(** Graph representation *)
module type Graph =
  sig
    (** Graph node representation *)
    module Node : ExtInfo

    (** Graph edge representation*)
    module Edge :
      sig
        (** Underying information module *)
        include Info
        
        (** [nodes e] return a pair of nodes (s, d) forming the edge *)
        val nodes : t -> Node.t * Node.t
      end

    (** Underying information module *)
    include ExtInfo

    (** Kind of the graph (directed or undirected) *)
    val kind : t -> [`Digraph | `Graph]
    
    (** [nodes g] returns a list of graph nodes *)
    val nodes : t -> Node.t list

    (** [edges g] returns a list of graph edges *)
    val edges : t -> Edge.t list
  end

(** Representation of a graph with clusters *)
module type ClusteredGraph = 
  sig
    (** Underlying graph *)
    include Graph

    (** Cluster information *)
    module Cluster :
      sig
        (** Underying information module *)
        include ExtInfo
   
        (** [nodes c] returns a list of nodes settled directly on the clusters top level *)
        val nodes : t -> Node.t list
      end

  end
 
(** General signature for DOT printer helper *)
module type Sig =
  sig

    (** Type of vizualizer parameter*)
    type parm

    (** DOT visualizer *)
    val toDOT : parm -> string

  end

(** A functor to create graph printer *)
module Printer (G : Graph) : Sig with
  type parm = G.t 

(** A functor to create clustered graph printer *)
module ClusteredPrinter (CG : ClusteredGraph) : Sig with
  type parm = CG.t * (CG.Cluster.t Clusters.t)
