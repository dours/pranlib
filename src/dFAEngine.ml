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

    module PV : ProgramView.Sig

    exception Unreachable of [ `Node of PV.G.Node.t | `Edge of PV.G.Edge.t ] 

    exception RangeError  of int

    val analyse : PV.G.t -> PV.G.Node.t -> PV.AV.L.t

  end



module Forward (PV : ProgramView.Sig) =
  struct

    module PV = PV
    
    exception Unreachable of [ `Node of PV.G.Node.t | `Edge of PV.G.Edge.t ] 

    exception RangeError  of int

    type ('a) s =
    {
      markup : 'a;
    }

    let dfa g =

    (* seems that following code is not completely correct =( *)

      let marking = Urray.make (PV.G.nnodes g) PV.AV.L.bottom in
 
      let markup node =
        let m = Urray.get marking (PV.G.Node.index node) in
        if m = PV.AV.L.bottom then raise (Unreachable (`Node node)) else m
      in
      let before v = List.fold_left 
                       (fun x y -> PV.AV.L.cap x (markup y))
                       PV.AV.L.bottom
                       (PV.G.pred v) in
      let rec traverse = function
        | [] -> { markup = markup }
        | v :: t ->
          let after = PV.flow v (before v) in
          if PV.AV.L.equal after (markup v) then (
            traverse t
          ) else (
            Urray.set marking (PV.G.Node.index v) after;
            traverse ((PV.G.succ v) @ t)
          )
      in
      traverse [PV.G.start]
 
    let data g = lazy (dfa g)

    let analyse g = (fun node -> (Lazy.force (data g)).markup node)

  end
