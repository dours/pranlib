(*
 * Loops: Loop finding algorithms.
 * Copyright (C) 2004-2006
 * Serjic Shkredov, St.Petersburg State University
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

open Printf
open List

module Make (D: Dominance.Sig) =
  struct

    module T = D.T
    module G = T.G

    let graph = T.graph
    let start = T.start

    module Region =
      struct

	module R = Region.Make (G) (T.Post)

	module SCS =
	  struct
	    
            let all = 
              lazy (
              Urray.init (G.nnodes graph) 
		(fun i -> 
		  if T.isValid i 
		  then lazy (snd (R.build (T.Post.node i)) )
		  else lazy R.NodeSet.empty
		)
             )
		
            let get node = Lazy.force (Urray.get (Lazy.force all) (T.Post.number node))
		
	  end
        
	module SCC =
	  struct

            let build () =
              let n = G.nnodes graph in
              let rec traverse i visited sccs =
		LOG (Printf.fprintf stderr "Traversing N=%d\n" i);
		if i = n 
		then sccs
		else 
		  let node = T.Post.node i in
		  if not (R.NodeSet.mem node visited) 
		  then begin
		    LOG (Printf.fprintf stderr "  Building region for %d\n" i);
		    let scc = snd (R.build node) in
		    traverse (i+1) (R.NodeSet.union visited scc) ((node, scc) :: sccs)
		  end
		  else traverse (i+1) visited sccs
              in
              traverse 0 R.NodeSet.empty []
		
            let data = lazy (build ())
		
            let get () = Lazy.force data
		
	  end

      end

(*
    module UFS = Unionfind.Make (G.Node) 
    module UFSI = Unionfind.Make 
     (
      struct 

    type t = int 

    let equal   x y = x = y 
    let compare x y = x - y 
    let hash        = Hashtbl.hash 

      end
     )

    module HSHND = Hashtbl.Make(G.Node)
    module HSHEG = Hashtbl.Make(G.Edge)
    
    let collapse uf li body header =
      LOG (printf "collapse: %s\n" (G.Node.toString header));
      HSHND.iter 
    (fun x y -> 
      LOG (printf "    with: %s\n" (G.Node.toString x)); 
      UFS.union header x uf; 
      HSHND.add li x header
    )
    body

    let find_loop uf li hdr = 
      let loop_body = HSHND.create 10 in
      let worklist = 
    (fun lst fi fu ->
          let t = HSHND.create 9 in
          List.iter 
        (fun curr -> 
              let fuh = fu curr in
              if fi fuh && not (HSHND.mem t fuh) then HSHND.add t fuh fuh
        ) 
        lst; 
      t 
    )
        (List.filter 
       (fun x -> T.sort x = DFST.Back) 
           (G.ins hdr)
    )
        (fun y -> not (G.Node.equal y hdr))
        (fun x -> UFS.find (G.src x) uf) 
      in 
      let rec iter_front wl =   
    if HSHND.length wl > 0 
    then (
          HSHND.iter (fun x y -> HSHND.add loop_body x y) wl; 
          let new_wl = HSHND.create 10 in
          let proceed_node nd =
            let in_edges = List.filter (fun x -> (T.sort x <> DFST.Back)) (G.ins nd) in
            let proceed_node x = 
              let nd = UFS.find (G.src x) uf in 
              if ((not (HSHND.mem loop_body nd)) && 
          (not (HSHND.mem new_wl    nd)) &&
          ((T.pre  nd) > (T.pre  hdr)) &&
          ((T.Post.number nd) > (T.Post.number hdr))
         ) 
          then HSHND.add new_wl nd nd
            in
            List.iter (proceed_node) in_edges
          in
          HSHND.iter (fun x y -> proceed_node x) wl;
          iter_front new_wl;
     ) 
      in
      iter_front worklist; 
      if HSHND.length loop_body > 0 then collapse uf li loop_body hdr

    module Havlak =
      struct

    let build () =
      let uf = UFS.init (G.nodes g) in
      let li = HSHND.create (G.nnodes g) in
      
      for i = (G.nnodes g) - 1 downto 0 
      do
        find_loop uf li (T.pre'1 i)
      done
        
    let root = T.pre'1 0 


      NODE_TREE_BUILDER.create  
        {
         NODE_TREE_BUILDER.iter = 
         ( fun f ->
               List.iter 
         (fun x ->
           LOG (printf "iter for node %s\n" (G.Node.toString x));
           if not (G.Node.equal x root_tree_node) then (
                     LOG (printf "started\n");
                     if HSHND.mem li x then f x (HSHND.find li x) else f x root_tree_node
            );
           LOG (printf "iter end\n");
         ) (G.nodes g)
          );  
         
         NODE_TREE_BUILDER.root = root_tree_node;
         NODE_TREE_BUILDER.size = (G.nnodes g);
       }
    


    module Improved =
      struct

        let build () = 
          let li = HSHND.create (G.nnodes g) in
          let add_to_mult_hash hsh key el =  
        if HSHND.mem hsh key 
        then 
          let lst = HSHND.find hsh key in 
          HSHND.replace hsh key (el::lst)
        else 
          HSHND.add hsh key [el] 
          in
          let lca =
        let uf = UFS.init (G.nodes g) in
        let ans = HSHND.create 99 in
        let pred =  
          let arr = HSHND.create (G.nnodes g) in
          HSHND.add arr T.start T.start;   
          List.iter 
            (fun el -> if T.sort el = DFST.Tree then HSHND.add arr (G.dst el) (G.src el)) 
            (G.edges g);
          arr 
        in

        let apply_fold from tooo f init = 
          let rec apply_n n = if n <= tooo then f (apply_n (n+1)) n else init in 
          apply_n from
        in

        (apply_fold 0 ((G.nnodes g) - 1)
           (fun gr i -> 
                     let nd = T.Post.node i in 
                     let pr = HSHND.find pred nd in
                     UFS.union pr nd uf; 
                     let proceed_out_edge grf e =
                       match (T.sort e) with
                       | DFST.Forward -> 
                           add_to_mult_hash ans nd (nd, G.dst e); 
                           G.deleteEdge grf e
                       | DFST.Cross -> 
                           let ca = UFS.find (G.dst e) uf in 
                           add_to_mult_hash ans ca (nd, G.dst e); 
                           G.deleteEdge grf e
                       | _ -> grf
                     in
                     List.fold_left (proceed_out_edge) gr (G.outs nd)
           ) 
           g, ans
        ) 
          in
          let gr, lc = lca in 
          let df = DFST.create (gr, (T.pre'1 0)) in
          let uf = UFS.init (G.nodes gr) in
          
          for i = (G.nnodes gr) - 1 downto 0 
          do
        find_loop uf li (T.pre'1 i);
          done;

          let root_tree_node = df.T.pre'1 0 in

          NODE_TREE_BUILDER.create  
          {
           NODE_TREE_BUILDER.iter = 
           (fun f ->
         List.iter 
                   (fun x ->
                     LOG (printf "iter for node %s\n" (G.Node.toString x));
                     if not (G.Node.equal x root_tree_node) then (
                       LOG (printf "started\n");
                       if HSHND.mem li x then f x (HSHND.find li x) else f x root_tree_node
              );
                     LOG (printf "iter end\n");
                   ) (G.nodes g)
               );  
           NODE_TREE_BUILDER.root = root_tree_node;
           NODE_TREE_BUILDER.size = (G.nnodes g);
          }

      end

      end

    module GaoLee =
      struct

    let build () = 
      let nnodes    = G.nnodes g in
      let rec init_list n = if n = 0 then [0] else n :: (init_list (n-1)) in 
      let uf        = UFSI.init (init_list (nnodes + nnodes/2 + 1)) in  
      let res_loops = Array.create (nnodes + nnodes/2 + 2) (nnodes + nnodes/2 + 2) in
      let processed = Array.create (nnodes + 1) false in
      let loops     = Array.create (nnodes + nnodes/2 + 1) [] in 
      let cur       = ref nnodes in
      let process   = Array.create ((nnodes*3)/2+1) false in
      let is_in_subtree hd nd =
            let hd_node = T.pre'1 hd in 
            let nd_node = T.pre'1 nd in
            let hd1 = T.Post.number hd_node in 
            let nd1 = T.Post.number nd_node in
            hd <= nd && hd1 <= nd1 
      in  
      let create_node nd = 
            LOG (
              printf "    create node rep for %i \n" nd;
              printf "    cur =  %i \n" !cur
            );
            loops.(!cur) <- List.map 
        (fun x -> 
          let x = UFSI.find (T.pre (G.src x)) uf in
          LOG (printf "      incoming from %i \n" x);
          x
        ) 
        (G.ins (T.pre'1 nd));
            incr (cur);
            !cur - 1 
      in
      let collapse body header =
            LOG (printf "Collapse for node %i with:\n" header);
            List.iter 
          (fun x -> 
        LOG (printf "    %i.\n" x);
        process.(x) <- false; 
        res_loops.(x) <- header; 
        UFSI.union header x uf
          ) 
          body;
        
            LOG (
              printf "    Incoming edges for new node come from: ";
              List.iter (fun x -> printf "%i," x) loops.(header);          
              printf ".\n"
            );
      in
      let find_loop hdr work_list =
            List.iter (fun x -> process.(x) <- true) work_list;
            if work_list <> []  
        then (
              LOG (printf "Work list not empty\n");
              let loop_rep = create_node hdr in
              let loop_body = [hdr] in 
              process.(hdr) <- true;
              let rec process_work_list lb = function
        | [] -> lb
        | hd :: tl_wl -> 
            let lb = hd :: lb in
            LOG (printf "    element %i " hd);
            if hd < nnodes 
            then (LOG (printf "processed.\n"); processed.(hd) <- true) 
            else (LOG (printf "\n"));
            let precedors =
                      if hd >= nnodes 
                      then List.map (fun x -> UFSI.find x uf) loops.(hd)   
                      else List.map (fun x -> UFSI.find (T.pre (G.src x)) uf) (G.ins (T.pre'1 hd)) 
            in
            let for_func wl in_node =
                      if in_node < nnodes && not (is_in_subtree hdr in_node) 
                      then (
            if List.for_all (fun x -> x <> in_node) loops.(loop_rep) 
            then loops.(loop_rep) <- in_node :: loops.(loop_rep);
            wl
                       ) 
                      else 
            if not process.(in_node) 
            then (
                          LOG (printf "        predecessor added to work list %i\n " in_node);
                          process.(in_node) <- true;
                          in_node :: wl 
             )
            else wl                        
            in
            let wl = List.fold_left for_func tl_wl precedors in
            process_work_list lb wl
              in
              let lp_body = process_work_list loop_body work_list in
              collapse lp_body loop_rep;
              LOG (printf "    collapse done\n");
             ) 
      in

      let f1 f_not nd = 
            if f_not true 
        then LOG (printf "red   find for node %i\n" nd) 
            else LOG (printf "irred find for node %i\n" nd);
        
            let w = 
          List.fold_left 
        (fun lst el_eg ->
          let el_nd = G.src el_eg in
                  let el_nd_n = T.pre el_nd in
                  LOG (printf "     incoming from %i\n" el_nd_n);
                  if T.sort el_eg = DFST.Back && f_not (D.dominates nd el_nd)  
                  then (UFSI.find el_nd_n uf) :: lst 
                  else lst 
        ) 
        [] 
        (G.ins (T.pre'1 nd)) 
            in      
            find_loop nd w 
      in
      
      let iter_levels f_1 f_2 = 
            let rec iter_levels_up_f lst =
              let new_lst = 
        List.fold_left 
          (fun ls el_ls -> ls @ (dm.DOMM.INT_TREE_BUILDER.get_childs el_ls)) 
          [] 
          lst 
          in
              match new_lst with
              | [] -> 
                  List.iter (fun nd -> f_1 nd) lst;
                  List.iter (fun nd -> f_2 nd) lst; 
        
              | _ -> 
                  iter_levels_up_f new_lst; 
                  List.iter (fun nd -> f_1 nd) lst;
                  List.iter (fun nd -> f_2 nd) lst  
            in
            iter_levels_up_f [0] 
      in
      
      iter_levels (f1 (fun x -> x)) (f1 (fun x -> not x));
      
      INT_TREE_BUILDER.create  
      {
       INT_TREE_BUILDER.iter = 
       (fun f ->
             for i = 0 to !cur - 1 do
               f i res_loops.(i)
             done
       );  

       INT_TREE_BUILDER.root = nnodes + nnodes/2 + 2;
       INT_TREE_BUILDER.size = !cur - 1;
     }

      end 
*)
  end































