(*
 * Region: Region Construction.
 * Copyright (C) 2004-2006
 * Gennadiy A. Sych, St.Petersburg State University
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

        val build : G.Node.t -> G.Node.t * G.Node.t list
    end

open List

module Make (G : Digraph.Sig) (F : Order.Sig with module G = G) =
    struct
        module G = G
        module F = F

        module NodeSet = Set.Make (G.Node)

        let rec region visited binumber scc = function
(*          | [] -> visited, scc*)
		  | [] -> scc
          | hd :: tl ->
              LOG (Printf.fprintf stderr "    Visiting %d\n" (F.number hd));
              let scc, visited, frontier = 
            fold_left 
              (fun (scc, visited, frontier) edge ->
                let candidate = G.src edge in
                LOG (Printf.fprintf stderr "    Considering candidate %d..." (F.number candidate));
                if not (NodeSet.mem candidate visited) && (F.number candidate > binumber)
                then begin
                  LOG (Printf.fprintf stderr "    added frontier.\n");
                  scc, (NodeSet.add candidate visited), candidate :: frontier        
                end
                else begin
                  LOG (Printf.fprintf stderr "    skipped.\n");
                  scc, visited, frontier 
                end
              ) 
              (hd :: scc, visited, tl) 
              (G.ins hd) 
              in
              region visited binumber scc frontier

        let build u = u, region NodeSet.empty (F.number u) [] [u]
        
(*
        let build u =
            let module S = Set.Make (G.Node) in
            let rec create_area a frontier visited =

        LOG (
        printf "area    : %s\n" (let module M = View.List (G.Node) in M.toString a);
        printf "frontier: %s\n" (let module M = View.List (G.Node) in M.toString frontier);
        printf "visited : %s\n" (let module M = View.Set  (G.Node) in M.toString visited)
        );

                match frontier with
                | [] -> a
                | hd :: tl ->
                    let rec create_frontier ancs frontier visited =
                        match ancs with
                        | [] -> frontier, visited
                        | h :: t ->
                            if (F.direct h) >= (F.direct u) && (not (S.mem h visited)) then
                                create_frontier t (h :: frontier) (S.add h visited)
                            else
                                create_frontier t frontier (S.add h visited)
                        in
                        let frontier, visited = create_frontier (G.pred hd) tl visited in
                        create_area (hd :: a) frontier visited
                in
                u, (create_area [] [u] S.empty)
*)
    end