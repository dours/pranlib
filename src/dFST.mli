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
    module G : CFG.Sig

    (** [reachedNode node] tests whether [node] is reached after DFST is built *)
    val reachedNode : G.Node.t -> bool

    (** [reachedEdge edge] tests whether [edge] is reached after DFST is built *)
    val reachedEdge : G.Edge.t -> bool

    (** A preorder numeration module  *)
    module Pre : Order.Sig with module G = G

    (** A postorder numeration module *)
    module Post : Order.Sig with module G = G
    
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
        module Node : DOT.ExtInfo with type t = G.Node.t
 	
        (** Edge wrapper *)
        module Edge : DOT.Info with type t = G.Edge.t

	include DOT.Sig with type parm = unit
      end

  end

(** Ordered Depth-First Tree constructor. Order defines the order for outgoing edge processing *)
module MakeOrdered (G : CFG.Sig) (O : sig val order : G.Edge.t list -> G.Edge.t list end) : Sig with module G = G

(** Depth-First Search Tree Constructor; outgoins edges are processed in an arbitrary order *)
module Make (G : CFG.Sig) : Sig with module G = G
