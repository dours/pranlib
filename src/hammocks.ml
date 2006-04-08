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
    



    let get node =
      let num = K.number node in
      let nnodes = (G.nnodes T.graph) in

          LOG ( printf "Begin hammocks construction for node %d\n" num; );
          
      let getHam i j =
      
        LOG ( printf "Verifying hammock: (%d .. %d)\n" i j; );
      
        let rec build (bg, bglen) (en, enlen) ham frontier =

					let mem elm lst = List.exists (fun e -> e == elm) lst
					in
        
          let buildBg node =

            let preds = G.pred node in
            if (List.exists (fun node -> let num = K.number node in num < i || num > j) preds)
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
										(*TODO: it's not necessary to verify this all time (I can make a flag...)*)
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
								printf "	Catch it! Begin: %d; Ham: " (match bg with | [] -> (-1) | h::t -> K.number h);
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
            else

							LOG ( printf "	Begin: %d\n" (match bg with | [] -> -1 | h::t -> K.number h); );

              let en, enlen, frontier = buildEn h t in

              if enlen > 1 then [], [], []
              else
								(*TODO: may be I can verify this using another way?*)
                if List.exists
                        (fun sucnode -> mem sucnode bg(*List.exists (fun begnode -> sucnode == begnode) bg*))
                        en
                then begin 

									LOG ( printf "	Edge between Begin and End exists. Exit.\n"; );

									[], [], [] 
								end
                else build (bg, bglen) (en, enlen) (h :: ham) frontier
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