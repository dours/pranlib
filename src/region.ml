(*
 * Region: Region Construction.
 * Copyright (C) 2004-2006
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
    module G : Digraph.Sig
    module F : Order.Sig

    module NodeSet : Set.S with type elt = G.Node.t

    val build : G.Node.t -> G.Node.t * NodeSet.t
  end

open List
open Printf

module Make (G : Digraph.Sig) (F : Order.Sig with module G = G) =
  struct
    
    module G = G
    module F = F

    module NodeSet = Set.Make (G.Node)

    let rec region visited binumber scc = function
    | [] -> scc
    | hd :: tl ->
        LOG (printf "    Visiting %d\n" (F.number hd));
        let scc, visited, frontier = 
          fold_left 
            (fun (scc, visited, frontier) edge ->
                let candidate = G.src edge in
                LOG (printf "    Considering candidate %d..." (F.number candidate));
                if not (NodeSet.mem candidate visited) && (F.number candidate > binumber)
                then begin
                  LOG (printf "    added frontier.\n");
                  scc, (NodeSet.add candidate visited), candidate :: frontier        
                end
                else begin
                LOG (printf "    skipped.\n");
                  scc, visited, frontier 
                end
            ) 
            ((NodeSet.add hd scc), visited, tl) 
            (G.ins hd) 
        in
        region visited binumber scc frontier

    let build u = u, region NodeSet.empty (F.number u) NodeSet.empty [u]
        
  end