(*
 * DFST: Depth-First Search Tree Construction.
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

type sort = Tree | Forward | Back | Cross

module type Sig =
  sig

    module G : CFG.Sig

    val reachedNode : G.Node.t -> bool
    val reachedEdge : G.Edge.t -> bool

    module Pre  : Order.Sig with module G = G
    module Post : Order.Sig with module G = G
  
    val sort    : G.Edge.t -> sort 
    val graph   : G.t          
    val start   : G.Node.t    

    type t = G.Node.t
	  
    val root     : t
    val parent   : t -> t option
    val children : t -> t list
	
    module DOT :
      sig

	module Node  : DOT.Node          with type t = G.Node.t
	module Edge  : Digraph.DOT.Edge  with type t = G.Edge.t
	      
	include Digraph.DOT.S with 
	  type graph = G.t      and 
	  type node  = G.Node.t and 
          type edge  = G.Edge.t and 
	  type parm  = unit

      end
	
  end
 
module MakeOrdered (G : CFG.Sig) (O : sig val order : G.Edge.t list -> G.Edge.t list end) =
  struct

    open List
    open Printf

    module G = G

    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
    exception RangeError  of int

    type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k) s =
	{
	 pre         : 'a;
	 post        : 'b;
	 pre'1       : 'c;
	 post'1      : 'd;
	 sort        : 'e;
	 parent      : 'f;
	 children    : 'g;
	 reachedNode : 'h;
	 reachedEdge : 'i;
	 last        : 'j;
         n           : 'k;
        }

    let build graph =
      let start = G.start             in
      let n     = G.nnodes graph      in
      let pre'  = Urray.make n start  in
      let post' = Urray.make n start  in
      let pre   = Urray.make (G.lastNode graph) (-1) in
      let post  = Urray.make (G.lastNode graph) (-1) in
      let sort  = Urray.make (G.lastEdge graph) Tree in
      let state = Urray.make (G.lastNode graph) `Init in
      
      let set p p' node num =
	Urray.set p  (G.Node.index node) num;
	Urray.set p' (num) node
      in
      
      let setPost node num = set post post' node num in
      let setPre  node num = set pre  pre'  node num in
      let setSort edge s   = Urray.set sort (G.Edge.index edge) s in
      
      let reachedNode node = Urray.get pre (G.Node.index node) <> -1 in
      let reachedEdge edge = reachedNode (G.src edge) in

      let getPre node = 
	let m = Urray.get pre (G.Node.index node) in
	if m = -1 then raise (Unreachable (`Node node)) else m
      in

      let getPost node = 
	let n = Urray.get post (G.Node.index node) in
	if n = -1 then raise (Unreachable (`Node node)) else n
      in

      let getSort edge = 
	if reachedEdge edge then Urray.get sort (G.Edge.index edge) else raise (Unreachable (`Edge edge))
      in

      let getPre'  num = try Urray.get pre'  (num) with Invalid_argument "index out of bounds" ->  raise (RangeError num) in
      let getPost' num = try Urray.get post' (num) with Invalid_argument "index out of bounds" -> raise (RangeError num) in
      
      let rec visit (currM, currN) = function
	| [] -> currM, currN
	| (node, []) :: tl ->
            setPost node currN;
            Urray.set state (G.Node.index node) `Done;
            visit (currM, currN-1) tl
          
	| (node, nextEdge :: rest) :: tl ->
            let nextNode = G.dst nextEdge in
            begin match Urray.get state (G.Node.index nextNode) with
            | `Init ->
		setPre nextNode currM;
		Urray.set state (G.Node.index nextNode) `InProcess;
		visit 
		  (currM+1, currN) 
		  ((nextNode, O.order (G.outs nextNode)) :: (node, rest) :: tl)
		  
            | `Done -> 
		setSort 
		  nextEdge 
		  (if getPre node > getPre nextNode then Cross else Forward);
		visit (currM, currN) ((node, rest) :: tl)
                  
            | `InProcess -> 
		setSort nextEdge Back;
		visit (currM, currN) ((node, rest) :: tl)
		  
            end
      in
      Urray.set state (G.Node.index start) `InProcess;
      setPre start 0;
      let lastM, firstN = visit (1, n-1) [start, G.outs start] in
      {
       pre      = getPre;
       post     = getPost;
       pre'1    = getPre';
       post'1   = getPost';
       sort     = getSort;
       parent   = 
       (fun node -> 
         match filter (fun e -> getSort e = Tree) (G.ins node) with 
         | [] -> None 
         | [e] -> Some (G.src e)
       );
       children    = (fun node -> map G.dst (filter (fun e -> getSort e = Tree) (G.outs node)));
       reachedNode = reachedNode;
       reachedEdge = reachedEdge;
       last        = lastM - 1;
       n           = n;
      }
	
    let data = lazy (build G.graph) 
	      
    let reachedNode = (fun node -> (Lazy.force data).reachedNode node)
    let reachedEdge = (fun edge -> (Lazy.force data).reachedEdge edge)
    let pre         = (fun node -> (Lazy.force data).pre         node)
    let post        = (fun node -> (Lazy.force data).post        node)
    let pre'1       = (fun node -> (Lazy.force data).pre'1       node)
    let post'1      = (fun node -> (Lazy.force data).post'1      node)
    let sort        = (fun edge -> (Lazy.force data).sort        edge)
    let parent      = (fun node -> (Lazy.force data).parent      node)
    let children    = (fun node -> (Lazy.force data).children    node)
    let last        = (fun ()   -> (Lazy.force data).last            )
    let n           = (fun ()   -> (Lazy.force data).n               )

    let graph = G.graph
    let start = G.start
    let root  = G.start
	
    module Pre = 
      struct
	
        module G = G
	    
        exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
        exception RangeError  of int
	    
        let number node  = pre node
        let node num     = pre'1 num
	let first        = 0
	let last         = last ()
	let valid num    = num <= last && num >= first
	    
      end
	
    module Post = 
      Order.Normalize 
	(
	 struct
	   
           module G = G
	       
           exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
           exception RangeError  of int
	       
           let number node  = post node
           let node num     = post'1 num
           let first        = (n ()) - (last ()) - 1 
	   let last         = (n ()) - 1
	   let valid num    = num <= last && num >= first
		 
	 end
	)
	
    type t = G.Node.t
	  
    module DOT =
      struct
	
	module Edge =
	  struct
	    
            type t = G.Edge.t
		  
            let attrs edge = 
	      try
		match sort edge with
		| Tree    -> ["color", "black"; "style", "bold"]
		| Cross   -> ["color", "blue"]
		| Forward -> ["color", "green"]
		| Back    -> ["color", "red"]
	      with Unreachable _ -> []
		    
            let label edge = 
	      try
		sprintf "%s\n%s" 
		  (match sort edge with Tree -> "Tree" | Back -> "Back" | Cross -> "Cross" | Forward -> "Forward") 
		  (G.Edge.toString edge)
	      with Unreachable _ -> sprintf "*** unreached ***\n%s" (G.Edge.toString edge)
		
	  end
	    
	module Node =
	  struct
            
            type t = G.Node.t
		  
            let attrs node = 
	      try ["shape", if pre node = 0 then "ellipse" else "box"]
	      with Unreachable _ -> []

            let label node = 
	      try sprintf "M=%d, N=%d\n%s" (pre node) (post node) (G.Node.toString node)
	      with Unreachable _ -> sprintf "*** unreachable ***\n%s" (G.Node.toString node)

            let name node = sprintf "node%d" (G.Node.index node)
		
	  end
	    
	module M = Digraph.Printer (G) (Node) (Edge)
	    
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

	let toDOT () = M.toDOT graph

	module Clusters = M.Clusters

	module Clustered =
	  struct
	    
	    let toDOT () tree = M.Clustered.toDOT graph tree

	  end
            
      end
	
  end

module Make (G : CFG.Sig) = MakeOrdered (G) (struct let order x = x end)
    
