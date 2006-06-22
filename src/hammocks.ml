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

            let nextorder  = Array.create (G.lastNode graph) (-1) in
            let nextorder' = Array.create nnodes T.start in
      
            let set node num =
              nextorder.(G.Node.index node) <- num;
              nextorder'.(num) <- node
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

            LOG (printf "Building region...\n");
      
            let _, r = R.build (getKiNode i) in

            LOG (
              printf "Region built\n";
              printf "Region: ";
              R.NodeSet.iter (fun node -> printf "%d, " (R.F.number node)) r ;
              printf "\n";
              printf "Renumbering...\n"
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
              printf "Renumbered\n"
            );
        
            let isValid i = 
              (i >= 0) && (i < nnodes) && (i = 0 || not (G.Node.equal nextorder'.(i) T.start)) 
            in

            let getNum node = 
              let m = nextorder.(G.Node.index node) in
              if m = -1 then raise (Unreachable (`Node node)) else m
            in

            let getNode num = if isValid num then nextorder'.(num) else raise (RangeError num) in

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

      
      let hammocks () =
        let nnodes = G.nnodes T.graph in
        
        let verify p = function | None -> false | Some q -> p q in
        let getNum = function | None -> -1 | Some q -> K.number q in   
        let mem lst elm = List.exists (fun e -> e == elm) lst in
        
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
                      if max1 <= k && max2 <= k && not (verify (mem (G.pred p)) q) then begin
                        
                        LOG(
                          printf " Found new hammock K'[begin .. k]: begin=%d, k=%d, end=%d\n" (K.number p) k (getNum q)
                        );
                        
                        fold (k+1) q set1 max1 max2 ((K.number p, k, getNum q) :: hams)
                      end
                      else
                        fold (k+1) q set1 max1 max2 hams
                    else
                      if k = nnodes then begin
                        
                        LOG(
                          printf " Found new hammock K'[begin .. k]: begin=%d, k=%d, end=NO\n" (K.number p) k
                        );
                        
                        fold (k+1) q set1 max1 max2 ((K.number p, k, -1) :: hams)
                      end
                      else
                        (* bad piece : mem ... *)
                        if (max1 = k + 1) && (max2 <= k) && not (mem (G.pred p) (K.node (k+1))) then begin

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
                

(*
    let get node =
      let num = K.number node in
      let nnodes = (G.nnodes T.graph) in
      
      LOG ( printf "Begin hammocks construction for node %d\n" num; );
      
      let getHam i j =
	
        LOG ( printf "Verifying hammock: (%d .. %d)\n" i j; );
	
        let rec build (bg, bglen) (en, enlen) ham frontier =
	  
	  let mem elm lst = List.exists (fun e -> e == elm) lst in
          
          let buildBg node =
	    
            let preds = G.pred node in
            if (List.exists (fun node -> let num = K.number node in num < i || num > j) preds) || (node == T.start)
            then (node :: bg), (bglen+1)
            else bg, bglen
          in
          
          let buildEn node frontier =
	    
	    LOG ( printf "	Building End...\n"; );
	    
	    (*TODO: this function will work very slow!*)
            let rec build en enlen frontier succs =
	      
	      LOG (
	        printf "		End: ";
	        List.iter (fun node -> printf "%d, " (K.number node)) en;
	        printf "\n";
	        printf "		frontier: ";
	        List.iter (fun node -> printf "%d, " (K.number node)) frontier;
	        printf "\n";
	        printf "		succs: ";
	        List.iter (fun node -> printf "%d, " (K.number node)) succs;
	        printf "\n";
	      );
	      
	      match succs with
              | [] -> en, enlen, frontier
              | h :: t ->
                  let num = K.number h in
                  if num < i || num > j then
		    (*TODO: probably, I can make a flag here*)
		    if not(mem h en) then
		      build (h :: en) (enlen+1) frontier t
		    else
		      build en enlen frontier t
                  else
		    if num >= i && num <= j && ((K.number h) > (K.number node)) && not (mem h frontier) then
	              build en enlen (h :: frontier) t
		    else
		      build en enlen frontier t
            in
            
            build en enlen frontier (G.succ node)
          in
          
          match frontier with
          | [] -> 
              if bglen = 1 && enlen <= 1 then begin
		
		LOG (							
		printf "Caught it! Begin: %d; Ham: " (match bg with | [] -> (-1) | h::t -> K.number h);
		  List.iter (fun node -> printf "%d, " (K.number node)) ham;
		  printf "\n";
	         );
		
		bg, ham, en
	      end
              else [], [], []
		  
          | h :: t ->
	      
	      LOG ( printf "	Matching frontier with %d :: t\n" (K.number h);	);
	      
              let bg, bglen = buildBg h in

              if bglen > 1 then [], [], []
              else begin
		
		LOG ( printf "	Begin: %d\n" (match bg with | [] -> -1 | h::t -> K.number h); );
		
		let en, enlen, frontier = buildEn h t in
		
		if enlen > 1 then [], [], []
		else begin
		  (*TODO: may be I can verify this in some another way?*)
                  if List.exists
                      (fun sucnode -> mem sucnode bg(*List.exists (fun begnode -> sucnode == begnode) bg*))
                      en
                  then begin 
		    
		    LOG ( printf "	Edge between Begin and End exists. Exit.\n"; );
		    
		    [], [], [] 
		  end
                  else build (bg, bglen) (en, enlen) (h :: ham) frontier
		end

	      end
        in
        
        build ([], 0) ([], 0) [] [(K.node i)]
      in
      
      let rec build i hams =
        if i < nnodes then
          let bg, ham, en = getHam num i in
	  match bg with
	  | [] -> build (i+1) hams
	  | h :: t ->
	      build (i+1) ( (h, ham) :: hams )
        else
          hams
      in
      
      build num []
*)

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
            let label node = sprintf "N=%d, K=%d\n%s" (T.Post.number node) (K.number node) (G.Node.toString node)
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
