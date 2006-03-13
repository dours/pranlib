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
 * (enclosed in the file COPYING).
 *)

module type Sig =
  sig

    module T : DFST.Sig
    module G : Digraph.Sig

    type t = G.Node.t

    val root       : t
    val parent     : t -> t option
    val children   : t -> t list
    val dominates  : t -> t -> bool
    val dominators : t -> t list

    module Tree :
      sig

    include DOT.Sig with type graph = unit and type node = t

    val toDOT : unit -> string

      end

    module DOT :
      sig

    module Node : DOT.Node with type t = G.Node.t
    module Edge : Digraph.DOT.Edge with type t = G.Edge.t

    include Digraph.DOT.S with 
       type graph = G.t and type node = G.Node.t and type edge = G.Edge.t and type parm = unit

      end
    
  end

module Make (D: DFST.Sig) = 
  struct

    open Array

    module T = D
    module G = T.G

    module Inner =
      struct      

    open Printf

    let build () = 
      let g          = T.graph                   in
      let g_size     = G.nnodes g                in
      let g_size1    = g_size + 1                in
      let ancestor   = create g_size1 0          in
      let label      = init g_size1 (fun x -> x) in
      let parent     = 
        let arr = create ((G.nnodes g) + 1) 0 in   
        List.iter 
          (fun el -> 
        if T.sort el = DFST.Tree 
        then arr.((T.Pre.number (G.dst el)) + 1) <- ((T.Pre.number (G.src el)) + 1)
          )
              (D.G.edges g);
        arr
      in
      let bucket     = create g_size1 []         in
      let dm         = create g_size1 0          in
      let semi       = init g_size1 (fun x -> x) in
      let size       = create g_size1 1          in
      let child      = create g_size1 0          in
      
      let rec compress v =
        let anc_v = ancestor.(v) in
        if ancestor.(anc_v) <> 0 
        then begin
              compress anc_v;
              if semi.(label.(anc_v)) < semi.(label.(v)) then 
                label.(v) <- (label.(anc_v));
              ancestor.(v) <- (ancestor.(anc_v))
        end
      in
      
      let link v w =               
        let rec iter s =
              let child_s = child.(s) in
              if semi.(label.(w)) < semi.(label.(child_s)) 
              then begin
        if size.(s) + size.(child.(child_s)) >= 2 * size.(child_s) 
        then (
          ancestor.(child_s) <- s;
          child.(s) <- (child.(child_s));
          iter s 
         ) 
        else (
          size.(child_s) <- size.(s); 
          ancestor.(s) <- child_s;
          iter ancestor.(s) 
         )
              end
              else s 
        in
        let s = iter w in
        label.(s) <- label.(w);
        size.(v) <- size.(v) + size.(w);
        let s = 
          if size.(v) < 2 * size.(w) 
          then ( 
        let tmp = child.(v) in
        child.(v) <- s;
        tmp
           ) 
          else s 
        in 
        let rec iter1 s = if s <> 0 then (
              ancestor.(s) <- v;
              iter1 child.(s)
            ) in
        iter1 s 
      in
      
      let eval v =                   
            if ancestor.(v) = 0 then label.(v)   
        else
            (
              compress v;
              let l_anc_v = label.(ancestor.(v)) in
              let label_v = label.(v) in
              if semi.(l_anc_v) >= semi.(label_v) then label_v else l_anc_v 
            )
      in
      
      for i = g_size downto 2 do 
            LOG (printf "implicit %i\n" i);
            let w = T.Pre.node (i-1) in
            let parent_i = parent.(i) in
            LOG (printf "    parent %i\n" parent_i);
            List.iter 
          (fun v_eg ->
        let u = eval (1 + T.Pre.number (G.src v_eg)) in
        let semi_u = semi.(u) in 
        if semi_u < semi.(i) then semi.(i) <- semi_u
              ) 
          (D.G.ins w); 
            let semi_i = semi.(i) in
            LOG (printf "    semi %i\n" semi_i);
            bucket.(semi_i) <- i :: bucket.(semi_i);
            link parent_i i;
            List.iter 
          (fun v -> 
        let u = eval v in
        dm.(v) <- if semi.(u) < semi.(v) 
        then (LOG (printf "  dm %i <- %i\n" v u); u) 
        else (LOG (printf "  dm %i <- %i\n" v parent_i); parent_i)
              ) 
          bucket.(parent_i);
            bucket.(parent_i) <- [] 
      done;
      
      for i = 2 to g_size do 
            let w = D.Pre.node (i-1) in
            let dom_i = dm.(i) in
            if  dom_i <> semi.(i) then dm.(i) <- dm.(dom_i)
      done;

      LOG (
        printf "Dominance vector {\n";
        iteri (fun i n -> printf "%d: %d\n" i n) dm;
        printf "}\n";
        flush stdout;
      );
      
      let index = create g_size (0, 0, []) in
      
      let update node dom = 
        let n, size, list = index.(dom) in
        index.(dom) <- (n, size, node :: list)
      in
      
      iteri (fun node dom -> if node > 1 then update (node-1) (dom-1)) dm;

      LOG (
        printf "Index vector (pass1) {\n";
        iteri 
          (fun i (n, size, list) -> 
        printf "%d: (%d, %d, %s)\n" i n size (let module M = View.List (View.Integer) in M.toString list)
          )
          index;
        printf "}\n";
        flush stdout;
      );
      
      let rec renum curr i =
        let _, _, list = index.(i) in
        let curr', size' = 
          List.fold_left 
        (fun (curr, size) node -> 
          let curr, size' = renum curr node in curr, size+size'
        ) 
        (curr+1, 0) 
        list
        in
        index.(i) <- curr, size', list;
        curr', size'+1
      in
      ignore (renum 0 0);
      dm, index
        
    let data = lazy (build ())
        
    type t = G.Node.t
          
    let root = T.start
        
    let parent node = 
      if D.Pre.number node = 0 
      then None
      else
        let dm = fst (Lazy.force data) in
        Some (T.Pre.node (dm.((T.Pre.number node) - 1) + 1))
          
    let children node =
      let index = snd (Lazy.force data) in
      let _, _, list = index.(T.Pre.number node) in
      List.map D.Pre.node list
        
    let dominates x y =
      let index = snd (Lazy.force data) in
      let nx, mx, _ = index.(T.Pre.number x) in
      let ny, my, _ = index.(T.Pre.number y) in
      ny >= nx && ny <= nx + mx 
        
    let rec dominators x =
      match parent x with
      | None -> []
      | Some y -> y :: (dominators y)
                
      end

    include Inner

    module Tree =
      struct

    include Tree.DOT.Printer (Inner) (D.DOT.Node)

      end

    module DOT =
      struct

    include D.DOT

    let toDOT () = 
      Printf.sprintf "%s%s%s%s%s" 
        (header T.graph) 
        (nodes (G.nodes T.graph)) 
        (edges (G.edges T.graph)) 
        (List.fold_left 
           (fun acc node ->
         acc ^ 
         (List.fold_left 
            (fun acc child -> 
              acc ^ (Printf.sprintf "%s -> %s [color=brown, style=bold];\n  " (Node.name node) (Node.name child))
            ) 
            "" 
            (children node)
         )
           )
           "\n  "
           (G.nodes T.graph)
        )
        (footer T.graph)
        
      end
      
  end





















