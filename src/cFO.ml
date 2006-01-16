(*
 * CFO: control flow optimizations.
 * Copyright (C) 2004-2006
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

open List

let fold_num func start num =
  let rec inner value i = if i = num then value else inner (func value i) (i+1) in
  inner start 0

module type Helper =
  sig

    type t

    val merge : t -> t -> t
    val empty : t -> bool
     
  end

module Make (D : DFST.Sig) (H : Helper with type t = D.G.Node.info) =
  struct

    let straightLines () =
      let graph  = D.graph             in
      let nnodes = D.G.nnodes graph    in
      let ends   = Array.make nnodes 0 in
      let markBegin i =
	ends.(i) <- ends.(i) lor 1;
	if i-1 >= 0 then ends.(i-1) <- ends.(i-1) lor 2
      in
      let markEnd i =
	ends.(i) <- ends.(i) lor 2;
	if i+1 < nnodes then ends.(i+1) <- ends.(i+1) lor 1
      in
      markBegin 0;
      for i=0 to nnodes-1 do
	let node   = D.pre'1 i in
	let outs   = D.G.outs node  in
	
	if (length (D.G.ins  node))  > 1 then markBegin i;
	if (length (D.G.outs node)) <> 1 
	then markEnd i
	else 
	  let [out] = outs in
	  if D.pre (D.G.dst out) <> i+1 then markEnd i;
      done;
      let rec convert i curr list =
	if i = nnodes then list
	else begin
	  let node = D.pre'1 i in
	  match ends.(i) with
	  | 0 -> convert (i+1) (node :: curr) list
	  | 1 -> convert (i+1)  [node]        list
	  | 2 -> convert (i+1)  []           ((rev (node :: curr)) :: list)
	  | 3 -> convert (i+1)  []           ( [node]              :: list)	      
	end
      in
      convert 0 [] [] 

    let collapseStraightLines () =
      let lines = straightLines () in
      let graph, edges, start =
	fold_left 
	  (fun (graph, edges, start) line ->
	    match line with
	    | [_]      -> graph, edges, start
	    | hd :: tl ->		
		let graph, edges, hd' =
		  fold_left 
		    (fun (graph, edges, hd) node ->  
		      let graph = fold_left 
			  (fun graph edge -> 
			    fst (D.G.insertEdge graph hd (D.G.dst edge) (D.G.Edge.info edge))
			  ) 
			  graph 
			  (D.G.outs node)
		      in
		      let info      = H.merge (D.G.Node.info hd) (D.G.Node.info node) in
		      let graph     = D.G.deleteNode graph node     in
		      let graph, hd = D.G.replaceNode graph hd info in
		      graph, edges+1, hd
		    ) 
		    (graph, edges, hd) 
		    tl
		in
		graph, edges, (if D.G.Node.equal hd D.start then hd' else start)
	  ) 
	  (D.graph, 0, D.start) 
	  lines
      in
      (graph, start), edges

    let removeUnreachableCode () = 
      let n = ref 0 in
      ((D.G.deleteNodes D.graph (fun node -> if not (D.reachedNode node) then (incr n; true) else false)), D.start), !n

    let removeEmptyNodes () =
      let graph, start = D.graph, D.start in
      let n            = D.G.nnodes graph in
      let removed      = ref 0 in
      let graph, start =
	fold_num 
	  (fun (g, s) i -> 
	    LOG (Printf.fprintf stderr "Checking node with N=%d\n" i);
	    let node = D.post'1 i in
	    let outs = D.G.outs node   in
	    let labl = D.G.Node.info node in
	    match outs with
	    | [edge] when H.empty labl ->
		LOG (Printf.fprintf stderr "Deleting node {%s}, N=%d\n" (D.G.Node.toString node) i);
		let dst = D.G.dst edge in
		if D.G.Node.equal dst node 
		then g, s
		else begin
		  LOG (Printf.fprintf stderr "Take 1: N(dst)=%d\n" (D.post dst));
		  let ins = D.G.ins node in
		  let g   = 
		    fold_left 
		      (fun g edge ->
			let src, info = D.G.src edge, D.G.Edge.info edge in
			fst (D.G.insertEdge g src dst info)
		      ) 
		      g ins 
		  in
		  LOG (Printf.fprintf stderr "Take 2: N(dst)=%d\n" (D.post dst));
		  incr removed;
		  let g, dst' = D.G.replaceNode g dst (H.merge labl (D.G.Node.info dst)) in
		  LOG (Printf.fprintf stderr "Before replace in DFST...\n");
		  LOG (Printf.fprintf stderr "After replace in DFST...\n");
		  if node == s then (D.G.deleteNode g node, dst')
		  else (D.G.deleteNode g node, s)
		end
		    
	    | _ -> 
		LOG (
		  Printf.fprintf stderr "Skipping node {%s}, N=%d\n" (D.G.Node.toString node) i;
		  List.iter 
		  (fun edge -> 
		    let dst = D.G.dst edge in
		    Printf.fprintf stderr "  Destination {%s}, N=%d\n" (D.G.Node.toString dst) (D.post dst);
		  ) outs
	        );
		(g, s)
	  ) (graph, start) n 
      in
      (graph, start), !removed 

  end    
