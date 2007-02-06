(*
 * DFAEngine: implements forward, backward and bidirectional DFA engines.
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

module type Sig =
  sig                         

    module G : CFG.Sig

    module L : Semilattice.Sig

    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 

    exception RangeError  of int

    val analyse : G.t -> G.Node.t -> L.t

  end


module Forward (AV : AlgView.Sig) =
  struct
    
    module PV = ProgramView.Make (AV)

    module G = AV.VA.G

    module L = AV.L

    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 

    exception RangeError  of int

    type ('a) s =
    {
      markup : 'a;
    }

    let dfa g =

      let marking = Urray.make (G.nnodes g) L.bottom in
 
      let markup node =
        let m = Urray.get marking (G.Node.index node) in
        if m = L.bottom then raise (Unreachable (`Node node)) else m
      in

      let before v =
        let rec fold before preds =
          match preds with
          | [] -> before
          | h :: t ->
            fold (L.cap before (markup h)) t
        in
        fold L.bottom (G.pred v)
      in
      
      let rec traverse = function
        | [] -> { markup = markup }
        | v :: t ->
          let after = PV.flow v (before v) in
          if after != (markup v) then 
            begin
              Urray.set marking (G.Node.index v) after;
              traverse ((G.succ v) @ t)
            end
          else
            traverse t
      in
      traverse [G.start]
 
    let data g = lazy (dfa g)

    let analyse g = (fun node -> (Lazy.force (data g)).markup node)

  end