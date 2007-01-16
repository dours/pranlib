(*
 * Hammocks: hammocks hierarchy construction.
 * Copyright (C) 2005
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

open List 
open Printf

module Make (T : DFST.Sig) =
  struct

    module T = T
    module G = T.G

    module K = 
      struct
  
        module G = G

        exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
        exception RangeError  of int

        type ('a, 'b) s =
        {
          number  : 'a;
          node : 'b;
        }
                    
        let build graph =
          let nnodes = G.nnodes graph in
    
          let renumK i (getKiNum, getKiNode) =
            LOG (printf "Running renumK for %d\n" i);

            let nextorder  = Urray.make (G.lastNode graph) (-1) in
            let nextorder' = Urray.make nnodes T.start in
      
            let set node num =
              Urray.set nextorder (G.Node.index node) num;
              Urray.set nextorder' num node
            in
      
            for j=0 to i-1 do set (getKiNode j) j done;
            
            let module R = Region.Make (G) 
                (struct 
                  exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
                  exception RangeError  of int
          
                  module G = G 
          
                  let node   = getKiNode 
                  let number = getKiNum 
                end)
            in

            LOG (printf "Building region for K...\n");
      
            let _, r = R.build (getKiNode i) in

            LOG (
              printf "Region for K built\n";
              printf "Region: ";
              R.NodeSet.iter (fun node -> printf "%d, " (R.F.number node)) r ;
              printf "\n";
              printf "K Renumbering...\n"
            );
            
            let module NodeSet = R.NodeSet in
      
            let rec inner j inn out =
              if j < nnodes then
                let u = getKiNode j in
                if (NodeSet.mem u r)
                then begin
                  set u inn;
                  inner (j+1) (inn+1) out
                end
                else begin
                  set u out;
                  inner (j+1) inn (out+1)
                end                    
            in

            inner i i (i+(NodeSet.cardinal r));

            LOG (
              printf "K Renumbered\n"
            );
        
            let isValid i = 
              (i >= 0) && (i < nnodes) && (i = 0 || not (G.Node.equal (Urray.get nextorder' i) T.start)) 
            in

            let getNum node = 
              let m = Urray.get nextorder (G.Node.index node) in
              if m = -1 then raise (Unreachable (`Node node)) else m
            in

            let getNode num = if isValid num then Urray.get nextorder' num else raise (RangeError num) in

            LOG (
              List.iter (fun node -> printf "node: %d; K: %d\n" (G.Node.index node) (getNum node)) (G.nodes T.graph)
            );
      
            getNum, getNode
          in
    
          let rec build getNum getNode n =
            if n < nnodes then
              let getNum, getNode = renumK n (getNum, getNode) in
              build getNum getNode (n+1)
            else begin
              LOG (printf "K order built\n"); 
              {number = getNum; node = getNode}
            end
          in
    
          build T.Post.number T.Post.node 0 
      
        let data = lazy (build T.graph)
      
        let number =  (fun node -> (Lazy.force data).number node)
        let node   =  (fun num  -> (Lazy.force data).node num)
      
      end

   let mem elm lst = List.exists (fun e -> e == elm) lst

    module L = 
      struct
  
        module G = G

        exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
        exception RangeError  of int

        type ('a, 'b) s =
        {
          number  : 'a;
          node : 'b;
        }

        let build graph =
          let nnodes = G.nnodes graph in
    
          let renumL i (getLiNum, getLiNode) =
            LOG (printf "Running renumL for %d\n" i);

            let nextorder  = Urray.make (G.lastNode graph) (-1) in
            let nextorder' = Urray.make nnodes T.start in
      
            let set node num =
              Urray.set nextorder (G.Node.index node) num;
              Urray.set nextorder' num node
            in

            List.iter (fun x -> set x (getLiNum x)) (G.nodes graph);

            let module R = Region.Make (G) 
                (struct 
                  exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
                  exception RangeError  of int
          
                  module G = G 
          
                  let node   = getLiNode 
                  let number = getLiNum 
                end)
            in

            LOG (printf "Building region for L...\n");

            (* The begining of the algorithm *)

            let _, r = R.build (getLiNode i) in

            LOG (
              printf "Region for L built\n";
              printf "Region: ";
              R.NodeSet.iter (fun node -> printf "%d, " (R.F.number node)) r ;
              printf "\n";
              printf "L Renumbering...\n"
            );

            let r = R.NodeSet.fold (fun x l -> x :: l) r [] in

            LOG (
              printf "Region: ";
              List.iter (fun x -> printf "%d, " (R.F.number x)) r;
              printf "\n";
            );

            (* Computes Exit(S) - exit vertex set for specified subgraph S. *)
            let exit s =
              List.fold_right (fun x exits -> 
                                    if (List.fold_left (fun res y -> res || not (mem y s) ) false (G.succ x))
                                    then
                                      x :: exits
                                    else
                                      exits
                                   )
                                  s
                                  []
            in

            (* Computes Line(F, S) - the line for specified numeration F and subgraph S. *)
            let line number s =
              
              LOG(
                printf "Calculating a line for set (Li numbers): ";
                List.iter (fun x -> printf "%d, " (getLiNum x)) s;
                printf "\n";
              );

              let rec fold frontier l =
                match frontier with
                | [] -> l
                | u :: t ->
              
                  let rec updateFrontier frontier preds =
                    match preds with
                    | [] -> frontier
                    | v :: t ->
                      if (mem v s) then
                        if (number v) < (number u) then updateFrontier (v :: frontier) t
                        else updateFrontier frontier t
                      else updateFrontier frontier t
                  in

                  let frontier = updateFrontier t (G.pred u) in
                  fold frontier (u :: l)
              in
              fold (exit s) []
             
            in

            let rank number i u s =

              LOG(
                (*printf "Calculating rank for i = %d, Li(u) = %d\n" i (getLiNum u);*)
                ();
              );
              
              let rec fold frontier r visited =
                match frontier with
                | [] -> r
                | v :: t ->

                  let visited = (v :: visited) in
              
                  let rec fld preds r frontier =
                    match preds with
                    | [] -> r, frontier
                    | w :: t ->
                      if not (mem w visited) then
                        if (mem w (line number s)) then fld t (max r (number w)) frontier
                        else fld t r (w :: frontier)
                      else fld t r frontier
                  in

                  let r, frontier = fld (G.pred v) r t in
                  fold frontier r visited
              in
              fold [u] i []
             
            in


            let rank w = rank getLiNum i w r in
            let sorted = List.sort (fun u v ->  
                                    let rankU = rank u in
                                    let rankV = rank v in
                                    if rankU < rankV || rankU = rankV && (getLiNum u) < (getLiNum v) then -1 else 1) r
            in

            LOG(
              printf "Sorted by (rank, Li): ";
              List.iter (fun x -> printf "(%d, %d) " (rank x) (getLiNum x)) sorted;
              printf "\n";
            );

            let rec fld n lst =
              match lst with
              | [] -> ()
              | u :: t -> 
                
                LOG(
                  printf "Assigning %d L number to node with graph index %d\n" n (G.Node.index u);
                );

                set u n; fld (n+1) t
            in

            fld i sorted;

            LOG(
              printf "nextorder: ";
              for i = 0 to (Urray.length nextorder)-1 do printf "%d, " (Urray.get nextorder i) done;
              printf "\n";
            );

            (* The end of the algorithm *)

            LOG (
              printf "L Renumbered\n"
            );
        
            let isValid i = 
              (i >= 0) && (i < nnodes) && (i = 0 || not (G.Node.equal (Urray.get nextorder' i) T.start)) 
            in

            let getNum node = 
              LOG(
                (* printf "getNum called with node with index = %d\n" (G.Node.index node); *)();
              );
              let m = Urray.get nextorder (G.Node.index node) in
              if m = -1 then raise (Unreachable (`Node node)) else m
            in

            let getNode num = if isValid num then Urray.get nextorder' num else raise (RangeError num) in

            LOG (
              List.iter (fun node -> printf "node: %d; L: %d\n" (G.Node.index node) (getNum node)) (G.nodes T.graph)
            );
      
            getNum, getNode
          in

          let rec build getNum getNode n =
            if n < nnodes then
              let getNum, getNode = renumL n (getNum, getNode) in
              build getNum getNode (n+1)
            else begin
              LOG (printf "L order built\n"); 
              {number = getNum; node = getNode}
            end
          in
    
          build K.number K.node 0 
      
        let data = lazy (build T.graph)
      
        let number =  (fun node -> (Lazy.force data).number node)
        let node   =  (fun num  -> (Lazy.force data).node num)
 
      end
  
      let hammocks () =
        let nnodes = G.nnodes T.graph in
        
        let verify p = function | None -> false | Some q -> p q in
        let getNum = function | None -> -1 | Some q -> K.number q in   
(*        let mem lst elm = List.exists (fun e -> e == elm) lst in*)
        
        let rec build nodes hamms =
          match nodes with
          | [] -> hamms
          | p :: t ->
              
            LOG(  
              printf "Assume K(begin) = %d\n" (K.number p)
            );

            let rec fold k q set1 max1 max2 hams =

              LOG ( printf " Running fold for k=%d\n" k );
              
              if k < nnodes then
                let w = K.node k in
                
                let rec foldset1 nodes q set1 max1 break =
                  LOG( 
                    printf "  Running foldset1 with params: SUCCS(w)= ";
                    List.iter (fun node -> printf "%d, " (K.number node)) nodes ;
                    printf " K(q)=%d, set1=%B, max1=%d, break=%B\n" (getNum q) set1 max1 break 
                  );
                  
                  if break then q, set1, max1, break
                  else begin
                    match nodes with
                    | [] -> q, set1, max1, break
                    | v :: t ->
                      let q, set1, max1, break =
                        if (T.Pre.number v) < (T.Pre.number p) then
                          if set1 then
                            if not (verify ((==) v) q) then
                              q, set1, max1, true
                            else
                              q, set1, max1, false
                          else
                            (Some v), true, max1, break 
                        else
                          (* if SET3 then break *)
                          if (K.number v) < (K.number p) then
                            q, set1, max1, true
                          else
                            q, set1, (if max1 > (K.number v) then max1 else (K.number v)), break
                      in
                      foldset1 t q set1 max1 break
                  end
                in

                let q, set1, max1, break = foldset1 (G.succ w) q set1 max1 false in
                if break then hams
                else
                  let max2, break = 
                    if not (w == p) then
                     
                      let rec foldset2 nodes max2 break =
                        
                        LOG( 
                          printf "  Running foldset2 with params: PREDS(w)= ";
                          List.iter (fun node -> printf "%d, " (K.number node)) nodes ;
                          printf " max2=%d, break=%B\n" max1 break
                        );
                        
                        if break then max2, break
                        else
                          match nodes with
                          | [] -> max2, break
                          | v :: t ->
                            let break = if (K.number v) < (K.number p) then true else false in
                            foldset2 t (if max2 > (K.number v) then max2 else (K.number v)) break
                      in

                      foldset2 (G.pred w) max2 false
                     
                    else
                      max2, break
                  in
                  if break then hams
                  else
                    if set1 then
                      (* bad piece : mem ... *)
                      if max1 <= k && max2 <= k && not (verify (fun x -> mem x (G.pred p)) q) then begin
                        
                        LOG(
                          printf " Found new hammock K'[begin .. k]: begin=%d, k=%d, end=%d\n" (K.number p) k (getNum q)
                        );
                        
                        fold (k+1) q set1 max1 max2 ((K.number p, k, getNum q) :: hams)
                      end
                      else
                        fold (k+1) q set1 max1 max2 hams
                    else
                      if k = (nnodes-1) then begin
                        
                        LOG(
                          printf " Found new hammock K'[begin .. k]: begin=%d, k=%d, end=NO\n" (K.number p) k
                        );
                        
                        fold (k+1) q set1 max1 max2 ((K.number p, k, -1) :: hams)
                      end
                      else
                        (* bad piece : mem ... *)
                        if (max1 = k + 1) && (max2 <= k) && not (mem (K.node (k+1)) (G.pred p)) then begin

                          LOG(
                            printf " Found new hammock K'[begin .. k]: begin=%d, k=%d, end=%d\n" (K.number p) k (k+1)
                          );
                          
                          fold (k+1) q set1 max1 max2 ((K.number p, k, k+1) :: hams)
                        end
                        else
                          fold (k+1) q set1 max1 max2 hams
              else
                hams
                
            in
            let hamms = fold (K.number p) None false (K.number p) (K.number p) hamms in
            build t hamms
        in

        build (G.nodes T.graph) []

    module DOT =
      struct

        module Edge =
          struct
  
            type t = G.Edge.t
          
            let attrs _ = []
          
            let label _ = ""

          end

        module Node =
          struct
        
            type t = G.Node.t
          
            let attrs node = ["shape", "ellipse"]
            let label node = sprintf "N=%d, K=%d, L=%d\n%s" (T.Post.number node) (K.number node) (L.number node) (G.Node.toString node)
            let name  node = sprintf "node%d" (G.Node.index node)

          end

(*        include Digraph.DOT.Printer (G) (Node) (Edge) *)

        module M = Digraph.DOT.Printer (G) (Node) (Edge)
    
        type graph = M.graph
        type node  = M.node
        type edge  = M.edge

        let header = M.header
        let footer = M.footer

        let attributes = M.attributes

        let node  = M.node
        let edge  = M.edge

        let edges = M.edges
        let nodes = M.nodes

        type parm = unit

        let toDOT () = M.toDOT T.graph
        
      end

    (* module L : Order.Sig with module G = T.G *)
 

  end
