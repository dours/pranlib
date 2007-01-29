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
    module G : CFG.Sig

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

    open Urray
      
    module T = D
    module G = T.G
	
    module Inner =
      struct      
	
	open Printf
	  
	let build () = 
	  let g          = T.graph                   in
	  let g_size     = G.nnodes g                in
	  let g_size1    = g_size + 1                in
	  let ancestor   = make g_size1 0            in
	  let label      = init g_size1 (fun x -> x) in
	  let parent     = 
            let arr = make ((G.nnodes g) + 1) 0 in   
            List.iter 
              (fun el -> 
		if T.sort el = DFST.Tree 
		then Urray.set arr ((T.Pre.number (G.dst el)) + 1) ((T.Pre.number (G.src el)) + 1)
              )
              (D.G.edges g);
            arr
	  in
	  let bucket     = make g_size1 []           in
	  let dm         = make g_size1 0            in
	  let semi       = init g_size1 (fun x -> x) in
	  let size       = make g_size1 1            in
	  let child      = make g_size1 0            in
	  
	  let rec compress v =
            let anc_v = Urray.get ancestor v in
            if (Urray.get ancestor anc_v) <> 0 
            then begin
              compress anc_v;
              if Urray.get semi (Urray.get label anc_v) < Urray.get semi (Urray.get label v) 
	      then 
		Urray.set label    v (Urray.get label    anc_v);
                Urray.set ancestor v (Urray.get ancestor anc_v)
            end
	  in
	  
	  let link v w =               
            let rec iter s =
              let child_s = Urray.get child s in
              if Urray.get semi (Urray.get label w) < Urray.get semi (Urray.get label child_s) 
              then begin
		if (Urray.get size s) + (Urray.get size (Urray.get child child_s)) >= 2 * (Urray.get size child_s)
		then (
		  Urray.set ancestor child_s s;
		  Urray.set child s (Urray.get child child_s);
		  iter s 
		 ) 
		else (
		  Urray.set size child_s (Urray.get size s); 
		  Urray.set ancestor s child_s;
		  iter (Urray.get ancestor s) 
		 )
              end
              else s 
            in
            let s = iter w in
            (Urray.set label s (Urray.get label w));
            Urray.set size v ((Urray.get size v) + (Urray.get size w));
            let s = 
              if (Urray.get size v) < 2 * (Urray.get size w) 
              then ( 
		let tmp = Urray.get child v in
		Urray.set child v s;
		tmp
               ) 
              else s 
            in 
            let rec iter1 s = 
	      if s <> 0 then (
		Urray.set ancestor s v;
		iter1 (Urray.get child s)
               ) 
	    in
            iter1 s 
	  in
	  
	  let eval v =                   
            if Urray.get ancestor v = 0 
	    then Urray.get label v
            else
              (
               compress v;
               let l_anc_v = Urray.get label (Urray.get ancestor v) in
               let label_v = Urray.get label v in
               if (Urray.get semi l_anc_v) >= (Urray.get semi label_v) then label_v else l_anc_v 
              )
	  in
	  
	  for i = g_size downto 2 do 
            LOG (printf "implicit %i\n" i);
            let w = T.Pre.node (i-1) in
            let parent_i = Urray.get parent i in
            LOG (printf "    parent %i\n" parent_i);
            List.iter 
              (fun v_eg ->
		let u = eval (1 + T.Pre.number (G.src v_eg)) in
		let semi_u = (Urray.get semi u) in 
		if semi_u < (Urray.get semi i) then Urray.set semi i semi_u
              ) 
              (D.G.ins w); 
            let semi_i = Urray.get semi i in
            LOG (printf "    semi %i\n" semi_i);
            Urray.set bucket semi_i (i :: Urray.get bucket semi_i);
            link parent_i i;
            List.iter 
              (fun v -> 
		let u = eval v in
		Urray.set dm v 
		  (
		   if (Urray.get semi u) < (Urray.get semi v) 
		   then (LOG (printf "  dm %i <- %i\n" v u); u)
		   else (LOG (printf "  dm %i <- %i\n" v parent_i); parent_i)
		  )
	      )
	      (Urray.get bucket parent_i);
	    Urray.set bucket parent_i [] 
	  done;
	  
	  for i = 2 to g_size do 
            let w = D.Pre.node (i-1) in
            let dom_i = Urray.get dm i in
            if  dom_i <> Urray.get semi i then Urray.set dm i (Urray.get dm dom_i)
	  done;
	  
	  LOG (
            printf "Dominance vector {\n";
            iteri (fun i n -> printf "%d: %d\n" i n) dm;
            printf "}\n";
            flush stdout;
	  );
      
	  let index = make g_size (0, 0, []) in
	  
	  let update node dom = 
            let n, size, list = Urray.get index dom in
            Urray.set index dom (n, size, node :: list)
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
            let _, _, list = Urray.get index i in
            let curr', size' = 
              List.fold_left 
		(fun (curr, size) node -> 
		  let curr, size' = renum curr node in curr, size+size'
		) 
		(curr+1, 0) 
		list
            in
            Urray.set index i (curr, size', list);
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
            Some (T.Pre.node (Urray.get dm ((T.Pre.number node) - 1) + 1))
              
	let children node =
	  let index = snd (Lazy.force data) in
	  let _, _, list = Urray.get index (T.Pre.number node) in
	  List.map D.Pre.node list
            
	let dominates x y =
	  let index = snd (Lazy.force data) in
	  let nx, mx, _ = Urray.get index (T.Pre.number x) in
	  let ny, my, _ = Urray.get index (T.Pre.number y) in
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





















