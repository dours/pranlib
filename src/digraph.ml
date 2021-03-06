(*
 * Digraph: basic directed graph implementation.
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

module type Item =
  sig

    type t
    type info

    val toString : t -> string
    val hash     : t -> int
    val equal    : t -> t -> bool
    val info     : t -> info
    val compare  : t -> t -> int
    val index    : t -> int

  end

module type Info =
  sig
    
    type t 
	  
    val toString : t -> string
	
  end


module type Base =
  sig

    module Node : Item
    module Edge : Item
	
    type t
	
    val src : Edge.t -> Node.t
    val dst : Edge.t -> Node.t

    val ins  : Node.t -> Edge.t list
    val outs : Node.t -> Edge.t list

    val pred : Node.t -> Node.t list
    val succ : Node.t -> Node.t list
	
    val nnodes : t -> int
    val nedges : t -> int
	
    val nodes  : t -> Node.t list 
    val edges  : t -> Edge.t list

    val lastEdge : t -> int
    val lastNode : t -> int
	  
    val create : unit -> t
	
    val insertNode  : t -> Node.info -> t * Node.t
    val insertEdge  : t -> Node.t -> Node.t -> Edge.info -> t * Edge.t

    val deleteEdges : t -> (Edge.t -> bool) -> t
    val deleteEdge  : t -> Edge.t -> t
      
    val deleteNodes : t -> (Node.t -> bool) -> t
    val deleteNode  : t -> Node.t -> t

    val replaceNode : t -> Node.t -> Node.info -> t * Node.t
    val replaceEdge : t -> Edge.t -> Edge.info -> t * Edge.t

    val copy  : t -> t
	
  end

module type Sig =
  sig

    include Base

    module DOT :
      sig
        module Info : DOT.Graph with type t = t and
                                     type Node.t = Node.t and
                                     type Edge.t = Edge.t

        include DOT.Sig with type parm = t
      end
	
  end

module DotInfo (G : Base)
               (N : DOT.ExtInfo with type t = G.Node.t) 
               (E : DOT.Info with type t = G.Edge.t) = 
  struct
    module Node = N
    module Edge = 
    struct
      include E
      let nodes edge = (G.src edge, G.dst edge)
    end
    type t = G.t
    let kind _  = `Digraph
    let name _ = "X"
    let label _ = ""
    let attrs _ = []
    let nodes = G.nodes
    let edges = G.edges
  end

    
module Make (NodeInfo : Info) (EdgeInfo : Info) =
  struct

    module Inner =
      struct

	module ID =
	  struct
	    
	    type t = int * int list
    
	    let start = 0, []

	    let provide (next, available) =
	      match available with
	      | hd :: tl -> (next, tl), hd
	      | [] -> (next+1, available), next
		    
	    let free (next, available) i = (next, i :: available)

	    let last (next, _) = next
					    
	  end

	type edgeBase = Edge of node * node
	and  nodeBase = Node of edge list ref * edge list ref
	    
	and  edge     = edgeBase * EdgeInfo.t * int * t ref
	and  node     = nodeBase * NodeInfo.t * int * t ref

	and  t = 
	     {
	      mutable nodes  : node list; 
	      mutable edges  : edge list; 
	      mutable nodeID : ID.t; 
	      mutable edgeID : ID.t; 
	      mutable nnodes : int; 
	      mutable nedges : int
	     }
	      
	let mid (_, x, _, _) = x
	let id  (_, _, x, _) = x
	let gr  (_, _, _, x) = !x
    
	let null = {nodes=[]; edges=[]; nodeID=ID.start; edgeID=ID.start; nnodes=0; nedges=0}

	module Node =
	  struct
	    		
	    type t    = node
	    type info = NodeInfo.t

	    let toString node = NodeInfo.toString (mid node)
	    let hash     node = Hashtbl.hash (id node)
	    let equal    x y  = (id x) = (id y)
	    let info          = mid
	    let compare x y   = (id x) - (id y)
	    let index         = id

	  end

	module Edge =
	  struct
		    
	    type t    = edge
	    type info = EdgeInfo.t

	    let toString edge = EdgeInfo.toString (mid edge)
	    let hash     edge = Hashtbl.hash (id edge)
	    let equal    x y  = (id x) =  (id y)
	    let info          = mid
	    let compare x y   = (id x) - (id y)
	    let index         = id

	  end
	      
	let src (Edge (x, _), _, _, _) = x
	let dst (Edge (_, x), _, _, _) = x
	    
	let ins  (Node (x, _), _, _, _) = !x
	let outs (Node (_, x), _, _, _) = !x
	    
	let collect list getOf =
	  let module S = Set.Make (Node) in
	  snd (
            fold_left 
	      (fun (s, l) e -> 
		let n = getOf e in
		if S.mem n s then s, l
		else S.add n s, n :: l
	      )
	      (S.empty, [])
	      list
	  )

	let pred node = collect (ins node) src
	let succ node = collect (outs node) dst
	    
	let nnodes g = g.nnodes
	let nedges g = g.nedges
	    
	let nodes g = g.nodes
	let edges g = g.edges
	    
	let create () = {nodes=[]; edges=[]; nodeID=ID.start; edgeID=ID.start; nnodes=0; nedges=0}

	let lastEdge t = ID.last t.edgeID
	let lastNode t = ID.last t.nodeID

	let insertNode (g : t) (info : Node.info) = 
	  let nodeID, id = ID.provide g.nodeID in
	  let node = Node (ref [], ref []), info, id, ref g in
	  g.nodes  <- node :: g.nodes; 
	  g.nnodes <- g.nnodes+1; 
	  g.nodeID <- nodeID;
	  g, node
	
	let insertEdge (g : t) beg edn (info : Edge.info) =
	  let edgeID, id = ID.provide g.edgeID in
	  let edge = Edge (beg, edn), info, id, ref g in
	  let Node (_, outs), _, _, _ = beg in
	  let Node (ins,  _), _, _, _ = edn in
	  outs := edge :: !outs;
	  ins  := edge :: !ins;
	  g.edges  <- edge :: g.edges; 
	  g.nedges <- g.nedges+1; 
	  g.edgeID <- edgeID;
	  g, edge
	    
	let insertNodeID id (g : t) (info : Node.info) = 
	  let node = Node (ref [], ref []), info, id, ref g in
	  g.nodes <- node :: g.nodes; 
	  g.nnodes <- g.nnodes+1;
	  g, node
	
	let insertEdgeID id (g : t) beg edn (info : Edge.info) =
	  let edge = Edge (beg, edn), info, id, ref g in
	  let Node (_, outs), _, _, _ = beg in
	  let Node (ins,  _), _, _, _ = edn in
	  outs := edge :: !outs;
	  ins  := edge :: !ins;
	  g.edges  <- edge :: g.edges; 
	  g.nedges <- g.nedges+1;
	  g, edge

	let deleteEdgesID flag (g : t) predicate =
	  let victims, edges = partition predicate g.edges in
	  let edgeID, num =
	    fold_left
	      (fun (edgeID, num) ((Edge ((Node (_, outs), _, _, _), (Node (ins, _), _, _, _)), _, _, graph) as edge) -> 
		let indicator e = (e == edge) in
		outs  := snd (partition indicator !outs);
		ins   := snd (partition indicator !ins);
		graph := null;
		(if flag then ID.free edgeID (id edge) else edgeID), num+1
	      ) 
	      (g.edgeID, 0)
	      victims
	  in
	  g.edges  <- edges; 
	  g.nedges <- g.nedges-num; 
	  g.edgeID <- edgeID;
	  g

	let deleteEdgeID flag graph edge = deleteEdgesID flag graph (fun x -> x == edge) 
	    
	let deleteNodesID flag (g : t) predicate =
	  let victims, nodes = partition predicate g.nodes in
	  let g, num =
	    fold_left 
	      (fun (g, num) ((Node (ins, outs), _, _, graph) as node) ->
		g.nodeID <- if flag then ID.free g.nodeID (id node) else g.nodeID;
		let r = (fold_left (deleteEdgeID flag) (fold_left (deleteEdgeID flag) g !outs) !ins), num+1 in
		graph := null;
		r
	      )
	      (g, 0)
	      victims
	  in
	  g.nodes <- nodes; 
	  g.nnodes <- g.nnodes-num;
	  g
	    
	let deleteNodeID flag graph node = deleteNodesID flag graph (fun x -> x == node)	    
	let deleteNodes = deleteNodesID true
	let deleteNode  = deleteNodeID true
	    
	let deleteEdges = deleteEdgesID true
	let deleteEdge  = deleteEdgeID true
	    
	let replaceNode graph node info =
	  if not (graph == (gr node)) then raise (Failure "node does not belong to the graph");
	  let graph, node' = insertNodeID (id node) graph info in
	  let graph = 
	    fold_left (fun g edge -> fst (insertEdgeID (id edge) g (src edge) node' (Edge.info edge))) graph (ins node) 
	  in
	  let graph = 
	    fold_left (fun g edge -> fst (insertEdgeID (id edge) g node' (dst edge) (Edge.info edge))) graph (outs node) 
	  in
	  deleteNodeID false graph node, node'
	    
	let replaceEdge graph edge info =
	  if not (graph == (gr edge)) then raise (Failure "edge does not belong to the graph");
	  let s, d = src edge, dst edge in
	  insertEdgeID (id edge) (deleteEdgeID false graph edge) s d info

	let copy g =
	  if nnodes g = 0 
	  then create ()
	  else
	    let index = Urray.make (lastNode g) (let n :: _ = nodes g in n) in
	    let g' = 
	      fold_left 
		(fun g' node -> 
		  let g', node' = insertNode g' (Node.info node) in
		  Urray.set index (Node.index node) node';
		  g'
		) 
		(create ()) 
		(nodes g) 
	    in
	    fold_left 
	      (fun g' edge -> 
		fst 
		  (
		   insertEdge g' 
		     (Urray.get index (Node.index (src edge))) 
		     (Urray.get index (Node.index (dst edge))) 
		     (Edge.info edge)
		  )
	      ) 
	      g' 
	      (edges g)

      end
    
    include Inner

    open Printf

    module DOT =
      struct

        module Info =
          DotInfo (Inner)
                  (
                   struct
                     
                     type t = Node.t
                           
                     let attrs _ = []
                     let label node = sprintf "index=%d, info=%s" (Node.index node) (Node.toString node)
                     let name  node = sprintf "node%d" (Node.index node)
                         
                   end
                  )
                  (
                   struct
                     
                     type t = Inner.Edge.t
                           
                     let attrs _ = []
                     let label edge = sprintf "info=%s" (Edge.toString edge)
                         
                   end
                  )

        include DOT.Printer (Info) 
      end


  end
    
