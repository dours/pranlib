(*
 * DFAEngine: implements forward, backward and bidirectional DFA engines.
 * Copyright (C) 2007
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

module Generic (P : ProgramView.Sig) (O : Order.Sig with module G = P.G) =
  struct

    let solve () =
      let upbe   = P.G.lastEdge P.G.graph in
      let upbn   = P.G.lastNode P.G.graph in
      let flows  = 
	let data = Urray.make upbn ((fun _ -> [], []) : P.Adapter.flow) in    
	List.iter 
	  (fun node -> Urray.set data (P.G.Node.index node) (P.flow (P.G.Node.info node))) 
	  (P.G.nodes P.G.graph);
	data
      in
      let infos  = 
	match P.G.edges P.G.graph with
	| [] -> Urray.empty ()
	| (hd :: _) as edges ->
	    let data = Urray.make upbe (P.Abstractor.edge (P.G.Edge.info hd)) in
	    List.iter 
	      (fun edge -> Urray.set data (P.G.Edge.index edge) (P.Abstractor.edge (P.G.Edge.info edge))) 
	      edges;
	    data
      in
      let labels = 
	let data = Urray.make upbe P.L.bottom in
	List.iter
	  (fun node ->	    
	    let ins, outs = 
	      P.init 
		(P.G.Node.info node) 
		(
		 List.map (fun e -> Urray.get infos (P.G.Edge.index e)) (P.G.ins  node),
		 List.map (fun e -> Urray.get infos (P.G.Edge.index e)) (P.G.outs node)
		)
	    in
	    if ins  <> [] then List.iter2 (fun e l -> Urray.set data (P.G.Edge.index e) l) (P.G.ins  node) ins;
	    if outs <> [] then List.iter2 (fun e l -> Urray.set data (P.G.Edge.index e) l) (P.G.outs node) outs
	  )
	  (P.G.nodes P.G.graph);
	data
      in
      let flowNode node = Urray.get flows (P.G.Node.index node) in
      let flowArgs node =
	let combine = List.map (fun edge -> let i = P.G.Edge.index edge in Urray.get infos i, Urray.get labels i) in
	let ins, outs = P.G.ins node, P.G.outs node in
	(combine ins, combine outs)
      in
      let update node (ins, outs) =
	let insOrig, outsOrig = P.G.ins node, P.G.outs node in
	let collectChanges acc labs edges =
	  if labs != []
	  then
	    List.fold_left2
	      (fun acc lab edge ->
		let i = P.G.Edge.index edge in
		if P.L.equal (Urray.get labels i) lab then acc
		else (
		  Urray.set labels i lab;
		  edge :: acc
		 )
	      )
	      acc
	      labs
	      edges
	  else acc
	in
	collectChanges (collectChanges [] ins insOrig) outs outsOrig
      in
      let continue = ref true in
      while !continue do
	continue := false;
	for i=O.first to O.last do
	  let node = O.node i in
	  match update node ((flowNode node) (flowArgs node)) with
	  | [] -> ()
	  | _  -> continue := true	  
	done
      done;
      (fun edge -> Urray.get labels (P.G.Edge.index edge))
	    
    let data = lazy (solve ())	  
    let get  = Lazy.force data	

  end

module Post (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) =
  struct

    include Generic (P) (T.Post)

  end

module RevPost (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) =
  struct

    include Generic (P) (Order.Rev (T.Post))

  end

module Concat (X : Order.Sig) (Y : Order.Sig with module G = X.G) =
  struct
    
    module G = X.G
	
    exception Unreachable of [ `Edge of G.Edge.t | `Node of G.Node.t ]
    exception RangeError of int
	
    let first       = 0
    let last        = X.last * 2
    let number node = X.number node
    let node   num  = if num > X.last then Y.node (num - X.last) else X.node num
    let valid  num  = num <= last && num >= first

  end

module PostRevPost (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) =
  struct

    include Generic (P) (Concat (T.Post) (Order.Rev (T.Post)))

  end

module RevPostPost (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) =
  struct

    include Generic (P) (Concat (Order.Rev (T.Post)) (T.Post))

  end

