(*
 * ProgramView: program abstraction module for DFA.
 * Copyright (C) 2007
 * Gennadiy Sych, St.Petersburg State University
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

(** {1 Program abstraction interface for DFA} *)
 
(** {2 General signature for program representation} *)
module type Repr =
  sig

    (** Information in CFG nodes *)
    type node

    (** Information on CFG edges *)
    type edge

  end

(** {2 Abstractor } *)
       
(** Abstractor converts concrete program representation into abstract 
    representation needed for certain DFA instance
  *)
module type Abstractor =
  sig
    
    (** Concrete program representation *)
    module Concrete : Repr

    (** Abstract program representation *)
    module Abstract : Repr
    
    (** Conversion function for node information *)
    val node : Concrete.node -> Abstract.node

    (** Conversion function for edge information *)
    val edge : Concrete.edge -> Abstract.edge

  end

(** {2 Adapters } *)

(** Adapter implements generic DFA framework for certain program representation *)
module type Adapter =
  sig

    (** Program representation *)
    module P : Repr 

    (** Semilattice *)
    module L : Semilattice.Sig

    (** Egde information augmented with dataflow fact *)
    type edge = P.edge * L.t
    
    (** Type of node flow function. Flow function [f (in, out)] transforms
        lists of augmented information for incoming ([in]) and outgoing ([out]) 
        edges for the node into lists of labels for that sets of edges
     *)
    type flow = edge list * edge list -> L.t list * L.t list
 
    (** Node flow function. [f node] returns flow function
        which transforms labels of incoming/outgoing edges
     *) 
    val flow : P.node -> flow

    (** Initial value for incoming and outgoing edges *)
    val init : P.node -> (P.edge list * P.edge list -> L.t list * L.t list)

  end

(** UniAdapter implements unidirectional DFA framework for certain program representation *)
module type UniAdapter =
  sig

    (** Program representation *)
    module P : Repr

    (** Semilattice *)
    module L : Semilattice.Sig

    (** Flow function for node *)
    val flow : P.node -> (L.t -> L.t)

    (** Initial labeling *)
    val init : P.node -> L.t

  end

(** Building forward DFA adapter *)
module ForwardAdapter (X : UniAdapter) : Adapter with
  module P = X.P and
  module L = X.L

(** Building backward DFA adapter *)
module BackwardAdapter (X : UniAdapter) : Adapter with
  module P = X.P and
  module L = X.L

(** {2 Program view for DFA } *)

(** General signature for program abstraction *)
module type Sig =
  sig

    (** Adapter *)
    module Adapter : Adapter

    (** Abstractor *)
    module Abstractor : Abstractor with 
      type Abstract.node = Adapter.P.node and 
      type Abstract.edge = Adapter.P.edge

    (** Semilattice of facts *)
    module L : Semilattice.Sig with
      type t = Adapter.L.t

    (** Control flow graph to analyze *)
    module G : CFG.Sig with 
      type Node.info = Abstractor.Concrete.node and
      type Edge.info = Abstractor.Concrete.edge

    (** [flow node] returns the flow function associated with the given node *)
    val flow : G.Node.info -> Adapter.flow

    (** [init node] returns initial semilattice element associated with the given node *)
    val init : G.Node.info -> (Adapter.P.edge list * Adapter.P.edge list -> Adapter.L.t list * Adapter.L.t list)

  end  
    
(** Constructor: creates ProgramView for given Adapter, Abstractor and 
    Control Flow Graph
  *)
module Make 
    (Adapter    : Adapter) 
    (Abstractor : Abstractor with type Abstract.node = Adapter.P.node and type Abstract.edge = Adapter.P.edge) 
    (G          : CFG.Sig with type Node.info = Abstractor.Concrete.node and type Edge.info = Abstractor.Concrete.edge) : 
  Sig with
    module Adapter    = Adapter    and
    module Abstractor = Abstractor and
    module L          = Adapter.L  and
    module G          = G
    
