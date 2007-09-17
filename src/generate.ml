(*
 * Generate: directed graph generation.
 * Copyright (C) 2005
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

module Digraph =
  struct

    let rec foldNum f i u x = if i = u then x else foldNum f (i+1) u (f x)
      
    module Nothing =
      struct
	
	type t = unit
	let toString _ = ""
	    
      end
	
    module G = Digraph.Make (Nothing)(Nothing)
	
    module Random =
      struct
	
	let insertEdges g n =
	  let nodes  = Urray.of_list (G.nodes g) in
	  let nnodes = Urray.length nodes in
	  Random.self_init ();
	  foldNum 
            (fun g -> 
	      let src, dst = Random.int nnodes, Random.int nnodes in
	      fst (G.insertEdge g (Urray.get nodes src) (Urray.get nodes dst) ())
	    ) 
	    0 n g 
	    
	let create nnodes nedges =
	  let g = foldNum (fun g -> fst (G.insertNode g ())) 0 nnodes (G.create ()) in
	  insertEdges g nedges 
	    
	module ControlFlow =
	  struct
	    
	    open Printf
	    open List
	      
	    let create nnodes nedges =
	      if (nnodes <= 0) || (nedges <= 0) || (nnodes > nedges+1)
	      then raise (Failure (sprintf "can not generate CFG with %d nodes and %d edges" nnodes nedges))
	      else begin
		
		let g, start = G.insertNode (G.create ()) () in
		let g = foldNum (fun x -> fst (G.insertNode x ())) 0 (nnodes-1) g in
		
		let module S = Set.Make (G.Node) in
		
		let available = 
		  fold_left 
		    (fun s node -> 
		      if G.Node.equal node start then s else S.add node s
		    )
		    S.empty 
		    (G.nodes g) 
		in
		
		let rec genLevel g rest frontier available =
		  LOG (printf "Genlevel: rest=%d\n" rest);
		  
		  if rest = 0
		  then g
		  else begin
		    
		    LOG (
		    printf "Before fold, sizeOf (frontier)=%d, sizeOf (available)=%d\n" 
		      (S.cardinal frontier) 
		      (S.cardinal available)
		   );
		    
		    let rest, g, frontier, available = 
		      S.fold 
			(fun node ((rest, g, frontier, available) as curr) -> 
			  
			  LOG (printf "Folding: rest=%d\n" rest);
			  
			  let rec foldN i n f s x =
			    if i = n 
			    then x
			    else 
			      let y = S.choose s in			      
			      foldN (i+1) n f (S.remove y s) (f y x)
			  in
			  
			  if rest = 0 
			  then curr
			  else
			    let n = Random.int (rest + 1) in
			    
			    LOG (printf "Random.int %d = %d\n" (rest+1) n);
			    
			    if n = 0 
			    then curr
			    else
			      let rest = rest - n in
			      
			      let g, choosen =
				foldN 
				  0 
				  n
				  (fun node' (g, choosen) -> 
				    (fst (G.insertEdge g node node' ())), S.add node' choosen
				  )
				  available 
				  (g, S.empty)
			      in
			      
			      LOG (printf "sizeOf (choosen) = %d\n" (S.cardinal choosen));
			      
			      rest, g, (S.union frontier choosen), (S.diff available choosen)		    
			)
			frontier 
			(rest, g, frontier, available)
		    in
		    
		    genLevel g rest frontier available		    
		      
		  end
		      
		in
		
		Random.self_init ();
		
		(insertEdges (genLevel g (nnodes-1) (S.add start S.empty) available) (nedges - nnodes + 1)), start
		  
	      end
		  
	  end
	    
      end
	
  end
    
