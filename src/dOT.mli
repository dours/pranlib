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

(** Interface to graph attributes *)
module type Graph =
  sig
    
    (** Type of the graph *)
    type t

    (** Keyword (e.g. "digraph", see DOT language description) *)
    val keyword : t -> string

    (** Name of the graph *)
    val name : t -> string 

    (** List of graph attributes *)
    val attrs : t -> (string * string) list
        
  end
      
(** Interface to node attributes *)
module type Node =
  sig
    
    (** Type of the node *)
    type t

    (** List of node attributes *)
    val attrs : t -> (string * string) list        

    (** Label of the node *)
    val label : t -> string        

    (** Name of the node *)
    val name  : t -> string
        
  end

(** General signature for DOT printer helper *)
module type Sig =
  sig

    (** Type of the graph *)
    type graph

    (** Type of the graph node*)
    type node

    (** Graph header *)
    val header : graph -> string

    (** Graph footer *)
    val footer : graph -> string

    (** Node representation *)
    val node : node -> string

    (** Node list representation *)
    val nodes : node list -> string

    (** Attribute printer: [attributes label attrs] returns a string 
        representation of [attrs] and [label] *)
    val attributes : string -> (string * string) list -> string

    (** Module to provide type for cluster structure within a graph *)
    module Clusters :
      sig

        (** Type for cluster tree; clusters have to form a nested structure *)
	type t = Node of node list * t list | Leaf of node list

      end

  end

(** Printer --- a functor to instantiate basic graph printing methods *)
module Printer (G : Graph) (N : Node) : Sig with type graph = G.t and type node = N.t
