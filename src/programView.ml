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

module type Repr =
  sig

    type node
    type edge

  end

module type Abstractor =
  sig
    
    module Concrete : Repr
    module Abstract : Repr
    
    val node : Concrete.node -> Abstract.node
    val edge : Concrete.edge -> Abstract.edge

  end

module type Adapter =
  sig

    module P : Repr 
    module L : Semilattice.Sig

    type edge = P.edge * L.t    
    type flow = edge list * edge list -> L.t list * L.t list
 
    val flow : P.node -> flow
    val init : P.node -> (P.edge list * P.edge list -> L.t list * L.t list)

  end

module type UniAdapter =
  sig

    module P : Repr
    module L : Semilattice.Sig

    val flow : P.node -> (L.t -> L.t)
    val init : P.node -> L.t

  end

module type Sig =
  sig

    module Adapter : Adapter

    module Abstractor : Abstractor with 
      type Abstract.node = Adapter.P.node and 
      type Abstract.edge = Adapter.P.edge

    module L : Semilattice.Sig with
      type t = Adapter.L.t

    module G : CFG.Sig with 
      type Node.info = Abstractor.Concrete.node and
      type Edge.info = Abstractor.Concrete.edge

    val flow : G.Node.info -> Adapter.flow
    val init : G.Node.info -> (Adapter.P.edge list * Adapter.P.edge list -> Adapter.L.t list * Adapter.L.t list)

  end  

module PrepareAdapter (X : UniAdapter) =
  struct

    module P = X.P
    module L = X.L

    type edge = P.edge * L.t    
    type flow = edge list * edge list -> L.t list * L.t list

  end

module ForwardAdapter (X : UniAdapter) =
  struct

    include PrepareAdapter (X)
 
    let flow node =
      (fun (ins, outs) ->
	let x = X.flow node (List.fold_right (fun (_, y) z -> L.cap z y) ins L.top) in
	[], List.map (fun _ -> x) outs
      )

    let init node = (fun (_, outs) -> let x = X.init node in [], List.map (fun _ -> x) outs)

  end

module BackwardAdapter (X : UniAdapter) =
  struct

    include PrepareAdapter (X)

    let flow node =
      (fun (ins, outs) ->
	let x = X.flow node (List.fold_right (fun (_, y) z -> L.cap z y) outs L.top) in
	List.map (fun _ -> x) ins, []
      )

    let init node = (fun (ins, _) -> let x = X.init node in List.map (fun _ -> x) ins, [])

  end
    
module Make (Adapter    : Adapter) 
            (Abstractor : Abstractor with type Abstract.node = Adapter.P.node and type Abstract.edge = Adapter.P.edge) 
            (G          : CFG.Sig with type Node.info = Abstractor.Concrete.node and type Edge.info = Abstractor.Concrete.edge)
    =
  struct
 
    module Adapter = Adapter 
	
    module Abstractor = Abstractor

    module L = Adapter.L
    module G = G

    let flow n = Adapter.flow (Abstractor.node n) 
    let init n = Adapter.init (Abstractor.node n)
              
  end
