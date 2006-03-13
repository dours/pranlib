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

        (* type t*)
        
        val build : G.Node.t -> G.Node.t * G.Node.t list
    end

module Make (G : Digraph.Sig) (F : Order.Sig with module G = G) =
    struct
        module G = G
        module F = F

        type t = G.Node.t list
    
        let build u =
            let module S = Set.Make (G.Node) in
            let rec create_area a frontier visited =
(*
        LOG (
        printf "area    : %s\n" (let module M = View.List (G.Node) in M.toString a);
        printf "frontier: %s\n" (let module M = View.List (G.Node) in M.toString frontier);
        printf "visited : %s\n" (let module M = View.Set  (G.Node) in M.toString visited)
        );
*)
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
    end