(*
 * Doms: Dominance tree contruction.
 * Copyright (C) 2005
 * Serjic Shkredov, St.Petersburg State University
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
 * (enclosed in the file LGPL).
 *)

exception Unknown_exception

open Printf
open Treebuilder

module DOM (G: Digraph.Sig) = 
struct

  module DFST = Cfa.DFST(G)
  module INT_TREE_BUILDER = Treebuilder.Make(TYPE_INT)

  type dfst = Cfa.DFST(G).info  
  type info = Treebuilder.Make(TYPE_INT).info_out

  (* creates array of parents in tree (using post number!) *)
  let frw_compute_reverse_tree dfs g =
      let arr = Array.create ((G.nnodes g) + 1) 0 in   
      List.iter (fun el -> 
                    if dfs.DFST.sort el == DFST.Tree 
                    then arr.((dfs.DFST.pre (G.dst el)) + 1) <- ((dfs.DFST.pre (G.src el)) + 1)
                ) (G.edges g);
      arr

  let apply_int from tooo f = let rec apply_n n = if n <= tooo then apply_n (n+1); f n in apply_n from

  (*sophisticated implementation of fast algorithm*)
  let create g dfs = 
      let g_size     = G.nnodes g in
      let g_size1    = g_size+1 in
      let creare_arr = Array.create g_size1 in 
      let ancestor   = creare_arr 0
      and label      = Array.init g_size1 (fun x -> x)

      (*array indexed*)
      and parent     = frw_compute_reverse_tree dfs g 
      and bucket     = Array.create g_size1 [] 
      and dm         = creare_arr 0
      and semi       = Array.init g_size1 (fun x -> x) 
      and size       = creare_arr 1 
      and child      = creare_arr 0 in

      (* path compression *)
      let rec compress v =                
         let anc_v = ancestor.(v) in
         if ancestor.(anc_v) <> 0 then (
            compress anc_v;
            if semi.(Array.get label anc_v) < semi.(Array.get label v) then label.(v) <- (label.(anc_v));
            ancestor.(v) <- (ancestor.(anc_v))
         ) 
      in

      (* link implementaion *) 
      let link v w =               
          Array.set size 0 0; Array.set label 0 0; Array.set semi 0 0;
          let rec iter s =
              let child_s = Array.get child s in
              if semi.(label.(w)) < semi.(label.(child_s)) 
              then (
                 if size.(s) + size.(child.(child_s)) >= 2 * size.(child_s) 
                 then (
                    ancestor.(child_s) <- s;
                    child.(s) <- (child.(child_s));
                    iter s 
                 ) 
                 else (
                    Array.set size child_s (Array.get size s); 
                    Array.set ancestor s child_s;
                    iter (Array.get ancestor s) 
                 )
              ) 
              else s 
          in
          let s = iter w in
          label.(s) <- label.(w);
          size.(v) <- size.(v) + size.(w);
          let s = if size.(v) < 2 * size.(w) 
                  then ( 
                    let tmp = child.(v) in
                    child.(v) <- s;
                    tmp
                  ) 
                  else s 
          in 
          let rec iter1 s =
              if s <> 0 then (ancestor.(s) <- v; iter1 child.(s)) 
          in
          iter1 s 
      in

      (* eval implementaion *)
      let eval v =                   
          let anc_v_n = ancestor.(v) in
          if anc_v_n == 0  
          then label.(v)   
          else
            let l_anc_v = label.(anc_v_n) 
            and label_v = label.(v) in
            if semi.(l_anc_v) >= semi.(label_v) then label_v else l_anc_v 
      in

      (*implicit calculation*)
      for i = g_size downto 2 do 
          LOG (printf "implicit %i\n" i);
          let w = dfs.DFST.pre'1 (i-1) in
          let parent_i = parent.(i) in
          LOG (printf "    parent %i\n" parent_i);
          List.iter (fun v_eg ->
                        let u = eval (1 + dfs.DFST.pre (G.src v_eg)) in
                        let semi_u = semi.(u) in 
                        if semi_u < semi.(i) then semi.(i) <- semi_u
                    ) (G.ins w); 
          let semi_i = semi.(i) in
          LOG (printf "    semi %i\n" semi_i);
          bucket.(semi_i) <- i :: bucket.(semi_i);
          link parent_i i;
          List.iter (fun v -> 
                        let u = eval v in
                        dm.(v) <- if semi.(u) < semi.(v) 
                                  then (LOG (printf "  dm %i <- %i\n" v u); u) 
                                  else (LOG (printf "  dm %i <- %i\n" v parent_i); parent_i)
                    ) bucket.(parent_i);
          bucket.(parent_i) <- [] 
      done;

      (*explicit calculations*)
      for i = 2 to g_size do 
          let w = dfs.DFST.pre'1 (i-1) in
          let dom_i = dm.(i) in
          if  dom_i <> semi.(i) then dm.(i) <- dm.(dom_i)
      done;

      INT_TREE_BUILDER.create
      {
        INT_TREE_BUILDER.iter = 
          (fun f -> 
              for i = 1 to g_size - 1 do
                  LOG (printf "dom %i %i\n" i (dm.(i+1) - 1));
                  f i (dm.(i+1) - 1)
              done 
          );
        INT_TREE_BUILDER.root = 0;   
        INT_TREE_BUILDER.size = g_size
      }

  let create_dfs g node = 
    let dfs = DFST.create (g,node) in
    create g dfs

end





















