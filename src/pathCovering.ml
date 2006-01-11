(*
 * PathCovering: minimal path covering construction.
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

module MakeSimple (D : DFST.Sig) =
  struct

    open List

    module Aux = Digraph.Make
      (struct 

	type t = D.G.Node.t option 
	let toString = function Some node -> D.G.Node.toString node | _ -> "<none>"

       end
      )
      (
       struct 

	 type t = D.G.Edge.t option * int
	 let toString = function (Some edge), _ -> D.G.Edge.toString edge | _ -> "<none>"
	     
       end
      )

    module MaxFlow = Graph.Flow.Ford_Fulkerson 
      (
       struct 
	   
	 type t = Aux.t

	 module V = Aux.Node
	 module E = 
	   struct

	     type t     = Aux.Edge.t
	     type label = Aux.Edge.info

	     let src   = Aux.src
	     let dst   = Aux.dst
	     let label = Aux.Edge.info
		 
	   end

	 let iter_succ_e func graph node = iter func (Aux.outs node)
	 let iter_pred_e func graph node = iter func (Aux.ins  node)

       end
      ) 
      (
       struct

	 type label = Aux.Edge.info
	 type t = int
	       
	 let max_capacity l = snd l
	 let min_capacity l = 0
	     
	 let flow l = 0
	 let add = (+)
	 let sub = (-)
	 let zero = 0
	 let compare = compare
	     
       end
      )
	
    let build () =
      let module NodeHash = Hashtbl.Make (D.G.Node) in

      let graph       = D.graph in
      let start       = D.start in
      let aux         = Aux.create () in
      let aux, source = Aux.insertNode aux None in
      let aux, sink   = Aux.insertNode aux None in
      let hash        = NodeHash.create (D.G.nnodes graph) in
      let aux         = 
	fold_left 
	  (fun aux node -> 
	    let aux, n  = Aux.insertNode aux (Some node) in
	    let aux, n' = Aux.insertNode aux (Some node) in
	    NodeHash.add hash node (n, n');
	    let aux, _ = Aux.insertEdge aux source n    (None, 1) in
	    let aux, _ = Aux.insertEdge aux n'     sink (None, 1) in
	    aux
	  ) 
	  aux 
	  (D.G.nodes graph) 
      in
      let module P = Set.Make (Compare.Pair (Aux.Node) (Aux.Node)) in
      let aux, p = 
	fold_left
	  (fun (aux, p) edge ->
	    match D.sort edge with
	    | DFST.Back -> aux, p
	    | _ ->
		let n, _ = NodeHash.find hash (D.G.src edge) in
		let _, k = NodeHash.find hash (D.G.dst edge) in
		let aux = 
		  if P.mem (n, k) p 
		  then aux
		  else fst (Aux.insertEdge aux n k (Some edge, 1))
		in
		aux, P.add (n, k) p
	  )
	  (aux, P.empty)
	  (D.G.edges graph)
      in

      LOG (Printf.fprintf stderr "Flow Graph:\n%s\n" (Aux.toDOT aux));

      let flows, _ = MaxFlow.maxflow aux source sink in
      let module NodeSet = Set.Make (D.G.Node) in
      let edges, nodes   = fold_left 
	  (fun (list, nodes) edge -> 
	    if flows edge > 0 then 
	      begin
		LOG (Printf.fprintf stderr " Adding edge to maximal flow: %s\n" (Aux.Edge.toString edge));
		match Aux.Edge.info edge with
		| None, _   -> list, nodes
		| Some e, _ -> (e :: list), (NodeSet.add (D.G.dst e) (NodeSet.add (D.G.src e) nodes))
	      end
	    else list, nodes
	  ) ([], NodeSet.empty) (Aux.edges aux)
      in

      LOG (
        Printf.fprintf stderr "Covered node set:\n";
        NodeSet.iter (fun node -> Printf.fprintf stderr "%s; " (D.G.Node.toString node)) nodes;
        Printf.fprintf stderr "\n";
      );

      let nodes = List.filter 
	  (fun node -> 
	    LOG (Printf.fprintf stderr "Checking node %s\n" (D.G.Node.toString node)); 
	    not (NodeSet.mem node nodes)
	  ) 
	  (D.G.nodes graph) 
      in

      LOG (
        Printf.fprintf stderr "Single node list:\n";
        List.iter (fun node -> Printf.fprintf stderr "%s; " (D.G.Node.toString node)) nodes;
        Printf.fprintf stderr "\n";
      );

      edges, nodes
	
    let data = lazy (build ()) 

    let edges = (fun () -> fst (Lazy.force data))
    let nodes = (fun () -> snd (Lazy.force data))
	
    let toDOT () =

      let module EdgeSet = Set.Make (D.G.Edge) in
      let module NodeSet = Set.Make (D.G.Node) in

      let edges = fold_left (fun set edge -> EdgeSet.add edge set) EdgeSet.empty (edges ()) in
      let nodes = fold_left (fun set node -> NodeSet.add node set) NodeSet.empty (nodes ()) in

      let module P = Digraph.DOT.Printer (D.G) 
	  (struct

	    include D.DOT.Node

	    let attrs node = 
	      if NodeSet.mem node nodes 
	      then ("color", "magenta") :: (attrs node)
	      else attrs node

	   end
	  )
	  (struct

	    include D.DOT.Edge

	    let attrs edge = 
	      if EdgeSet.mem edge edges 
	      then map (function ("color", _) -> "color", "magenta" | x -> x ) (attrs edge)
	      else attrs edge

	   end
	  )
      in
      P.toDOT D.graph


    type path = Single of D.G.Node.t | Path of (D.G.Node.t * D.G.Edge.t) list

    let paths () =

      let module NodeSet = Set.Make (D.G.Node) in
      let module NodeMap = Map.Make (D.G.Node) in

      let edges = edges () in
      let check set elt num = if NodeSet.mem elt set then num else 0 in
      let heads, tails, path = 
	fold_left 
	  (fun (heads, tails, path) edge ->
	    let src, dst = D.G.src edge, D.G.dst edge in
	    match (check tails src 1) + (check heads dst 2) with
	    | 0 (* not tail, not head *) -> NodeSet.add src heads, NodeSet.add dst tails, NodeMap.add src edge path
	    | 1 (* src is tail *)        -> heads, NodeSet.add dst (NodeSet.remove src tails), NodeMap.add src edge path
	    | 2 (* dst is head *)        -> NodeSet.add dst (NodeSet.remove src heads), tails, NodeMap.add src edge path
	    | 3 (* both        *)        -> NodeSet.remove dst heads, NodeSet.remove src tails, NodeMap.add src edge path
	  )	  
	  (NodeSet.empty, NodeSet.empty, NodeMap.empty)
          edges
      in
      (NodeSet.fold 
	(fun head fragments -> 
	  let rec build node list =
	    if NodeSet.mem node tails then list
	    else 
	      let edge = NodeMap.find node path in
	      build (D.G.dst edge) ((node, edge) :: list)
	  in
	  (Path (rev (build head []))) :: fragments
	) 
	heads 
	[]) @ (List.map (fun node -> Single node) (nodes ()))
	
  end
