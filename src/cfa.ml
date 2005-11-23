(*
 * Cfa: control flow analysis library.
 * Copyright (C) 2004
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
 * (enclosed in the file LGPL).
 *)

open List

let fold_num func start num =
  let rec inner value i =
    if i=num then value
    else inner (func value i) (i+1)
  in
  inner start 0

module type G_DFST =
    sig

      module Node :
	  sig

	    type t

	    val hash     : t -> int
	    val equal    : t -> t -> bool
	    val toString : t -> string

	  end

      module Edge :
	  sig

	    type t

	    val hash     : t -> int
	    val equal    : t -> t -> bool
	    val toString : t -> string

	  end

      type t 

      val nnodes : t -> int
      val nedges : t -> int

      val dst    : Edge.t -> Node.t
      val outs   : Node.t -> Edge.t list

      val print  : t -> (Node.t -> string) * (Edge.t -> string) -> string

    end

module DFST (G : G_DFST) =
  struct
    
    type sort = Tree | Forward | Back | Cross
    type info =
       {
	pre        : G.Node.t -> int;
        post       : G.Node.t -> int;

        pre'1      : int -> G.Node.t;
	post'1     : int -> G.Node.t;

	sort       : G.Edge.t -> sort;

	graph      : G.t;
	start      : G.Node.t;

	nodeString : G.Node.t -> string;
	edgeString : G.Edge.t -> string;

        toString   : unit -> string;

        replaceNode : G.Node.t -> G.Node.t -> unit;
        replaceEdge : G.Edge.t -> G.Edge.t -> unit;
       }

    type state = InProcess | Done

    module NodeHash = Hashtbl.Make (G.Node)
    module EdgeHash = Hashtbl.Make (G.Edge)

    let create (graph, start) =
      let build (graph, start) =
	let n     = G.nnodes graph      in
	let pre'  = Array.make n start  in
	let post' = Array.make n start  in
	let pre   = NodeHash.create n   in
	let post  = NodeHash.create n   in
	let sort  = EdgeHash.create (G.nedges graph) in
	let state = NodeHash.create n   in
	
	let set p p' node num =
	  NodeHash.remove p node;
	  NodeHash.add p node num;
	  p'.(num) <- node
	in
	
	let setPost node num = set post post' node num in
	let setPre  node num = set pre  pre'  node num in
	let setSort edge s   = EdgeHash.remove sort edge; EdgeHash.add sort edge s in
	
	let getPre   node = NodeHash.find pre  node in
	let getPost  node = NodeHash.find post node in
	let getSort  edge = EdgeHash.find sort edge in
	let getPre'  num  = pre' .(num) in
	let getPost' num  = post'.(num) in

	let replaceNode node node' =
	  let pre, post = getPre node, getPost node in
	  setPre node' pre;
	  setPost node' post
	in
	let replaceEdge edge edge' =
	  setSort edge' (getSort edge)
	in
	
	let rec visit (currM, currN) = function
	  | [] -> ()
	  | (node, []) :: tl ->
	      setPost node currN;
	      NodeHash.add state node Done;
	      visit (currM, currN-1) tl
		
	  | (node, nextEdge :: rest) :: tl ->
	      let nextNode = G.dst nextEdge in
	      try
		begin match NodeHash.find state nextNode with
		| Done -> 
		    setSort 
		      nextEdge 
		      (if getPre node > getPre nextNode then Cross else Forward)
		      
		| InProcess -> setSort nextEdge Back
		end;
		visit (currM, currN) ((node, rest) :: tl)
	      with
	      | Not_found -> 
		  LOG (Printf.fprintf stderr "Fresh node: {%s}\n" (G.Node.toString nextNode));
		  setPre  nextNode currM;
		  setSort nextEdge Tree;
		  NodeHash.add state nextNode InProcess;
		  visit 
		    (currM+1, currN) 
		    ((nextNode, G.outs nextNode) :: (node, rest)::tl)
	in
	let edgeAttrs edge = 
	  try
	    (match EdgeHash.find sort edge with
	    | Tree    -> "[color=black, style=bold, label=\""
	    | Cross   -> "[color=blue, label=\""
	    | Forward -> "[color=green, label=\""
	    | Back    -> "[color=red, label=\"") ^ (G.Edge.toString edge) ^ "\"]"
	  with
	    Not_found -> "[color=magenta, style=bold, label=\"**unreached**\"]"
	in 
	let nodeAttrs node =
	  "[" ^ 
	  (if node == start then "shape=ellipse, " else "shape=box, ") ^
	  ("label=\"") ^ 
	  (
	   (
	    try Printf.sprintf "M=%d,N=%d\\n%s" (getPre node) (getPost node)
	    with Not_found -> Printf.sprintf "**unreachable**\\n%s"
	   ) 
	     (G.Node.toString node)
	  ) ^
	  "\"]"	    
	in	
	NodeHash.add state start InProcess;
	setPre start 0;
	visit (1, n-1) [start, G.outs start];
	{
	 pre        = getPre;
	 post       = getPost;
	 pre'1      = getPre';
	 post'1     = getPost';
	 sort       = getSort;       
	 graph      = graph;
	 start      = start;

	 nodeString = nodeAttrs;
	 edgeString = edgeAttrs;

	 toString   = (fun () -> G.print graph (nodeAttrs, edgeAttrs));

	 replaceNode = replaceNode;
	 replaceEdge = replaceEdge;
        }
      in
      let data = lazy (build (graph, start)) in
      {
       pre        = (fun node -> (Lazy.force data).pre    node);
       post       = (fun node -> (Lazy.force data).post   node);
       pre'1      = (fun node -> (Lazy.force data).pre'1  node);
       post'1     = (fun node -> (Lazy.force data).post'1 node);
       sort       = (fun edge -> (Lazy.force data).sort   edge);

       graph      = graph;
       start      = start;

       nodeString = (fun n    -> (Lazy.force data).nodeString n);
       edgeString = (fun e    -> (Lazy.force data).edgeString e);

       toString   = (fun ()   -> (Lazy.force data).toString ());

       replaceNode = (fun x y -> (Lazy.force data).replaceNode x y);
       replaceEdge = (fun x y -> (Lazy.force data).replaceEdge x y);
      }

  end

module type G_CFO =
    sig

      module Node :
	  sig

	    type t
	    type info

	    val info : t -> info
	    val hash : t -> int

	    val equal : t -> t -> bool

	    val toString : t -> string

	  end

      module Edge :
	  sig

	    type t
	    type info

	    val info : t -> info
	    val hash : t -> int

	    val equal : t -> t -> bool

	    val toString : t -> string

	  end

      type t 

      val nnodes : t -> int
      val nedges : t -> int

      val src  : Edge.t -> Node.t
      val dst  : Edge.t -> Node.t
      val ins  : Node.t -> Edge.t list
      val outs : Node.t -> Edge.t list

      val print : t -> (Node.t -> string) * (Edge.t -> string) -> string

      val insertEdge  : t -> Node.t -> Node.t -> Edge.info -> t * Edge.t
      val deleteNode  : t -> Node.t -> t
      val deleteNodes : t -> (Node.t -> bool) -> t
      val replaceNode : t -> Node.t -> Node.info -> t * Node.t

    end

module type IS =
  sig

    type t

    val merge : t -> t -> t
    val empty : t -> bool
     
  end

module Optimize (G : G_CFO) (S : IS with type t = G.Node.info) =
  struct

    module DFST = DFST(G)

    type info = DFST.info

    let detectStraightLines info =
      let graph  = info.DFST.graph          in
      let nnodes = G.nnodes graph      in
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
	let node   = info.DFST.pre'1 i in
	let outs   = G.outs node  in
	
	if (length (G.ins  node))  > 1 then markBegin i;
	if (length (G.outs node)) <> 1 
	then markEnd i
	else 
	  let [out] = outs in
	  if info.DFST.pre (G.dst out) <> i+1 then markEnd i;
      done;
      let rec convert i curr list =
	if i = nnodes then list
	else begin
	  let node = info.DFST.pre'1 i in
	  match ends.(i) with
	  | 0 -> convert (i+1) (node :: curr) list
	  | 1 -> convert (i+1)  [node]        list
	  | 2 -> convert (i+1)  []           ((rev (node :: curr)) :: list)
	  | 3 -> convert (i+1)  []           ( [node]              :: list)	      
	end
      in
      convert 0 [] [] 

    let collapseStraightLines info lines =
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
			    fst (G.insertEdge graph hd (G.dst edge) (G.Edge.info edge))
			  ) 
			  graph 
			  (G.outs node)
		      in
		      let info      = S.merge (G.Node.info hd) (G.Node.info node) in
		      let graph     = G.deleteNode graph node     in
		      let graph, hd = G.replaceNode graph hd info in
		      graph, edges+1, hd
		    ) 
		    (graph, edges, hd) 
		    tl
		in
		graph, edges, (if hd == info.DFST.start then hd' else start)
	  ) 
	  (info.DFST.graph, 0, info.DFST.start) 
	  lines
      in
      (DFST.create (graph, start)), edges

    let removeUnreachableCode info = 
      let n = ref 0 in
      DFST.create 
	(
	 (G.deleteNodes info.DFST.graph) (fun node -> try ignore (info.DFST.pre node); false with Not_found -> incr n; true), 
	  info.DFST.start
	), 
      !n

    let removeEmptyNodes info =
      let graph, start = info.DFST.graph, info.DFST.start in
      let n            = G.nnodes graph in
      let removed      = ref 0 in
      let graph, start =
	fold_num 
	  (fun (g, s) i -> 
	    LOG (Printf.fprintf stderr "Checking node with N=%d\n" i);
	    let node = info.DFST.post'1 i in
	    let outs = G.outs node   in
	    let labl = G.Node.info node in
	    match outs with
	    | [edge] when S.empty labl ->
		LOG (Printf.fprintf stderr "Deleting node {%s}, N=%d\n" (G.Node.toString node) i);
		let dst = G.dst edge in
		if G.Node.equal dst node 
		then g, s
		else begin
		  LOG (Printf.fprintf stderr "Take 1: N(dst)=%d\n" (info.DFST.post dst));
		  let ins = G.ins node in
		  let g   = 
		    fold_left 
		      (fun g edge ->
			let src, info = G.src edge, G.Edge.info edge in
			fst (G.insertEdge g src dst info)
		      ) 
		      g ins 
		  in
		  LOG (Printf.fprintf stderr "Take 2: N(dst)=%d\n" (info.DFST.post dst));
		  incr removed;
		  let g, dst' = G.replaceNode g dst (S.merge labl (G.Node.info dst)) in
		  LOG (Printf.fprintf stderr "Before replace in DFST...\n");
		  info.DFST.replaceNode dst dst';
		  LOG (Printf.fprintf stderr "After replace in DFST...\n");
		  if node == s then (G.deleteNode g node, dst')
		  else (G.deleteNode g node, s)
		end
		    
	    | _ -> 
		LOG (
		  Printf.fprintf stderr "Skipping node {%s}, N=%d\n" (G.Node.toString node) i;
		  List.iter 
		  (fun edge -> 
		    let dst = G.dst edge in
		    Printf.fprintf stderr "  Destination {%s}, N=%d\n" (G.Node.toString dst) (info.DFST.post dst);
		  ) outs
	        );
		(g, s)
	  ) (graph, start) n 
      in
      DFST.create (graph, start), !removed 

  end    

module SCC (G : Digraph.Sig) =
  struct

    module DFST    = DFST(G)
    module NodeSet = Set.Make (G.Node)

    type dfst = DFST.info
    type info = {sccs : unit -> (G.Node.t * G.Node.t list) list; dfst : DFST.info; toString : unit -> string}

    let rec region dfst visited binumber scc front =
      match front with
      | []       -> visited, scc
      | hd :: tl ->
	  LOG (Printf.fprintf stderr "    Visiting %d\n" (dfst.DFST.post hd));
	  let scc, visited, frontier = 
	    fold_left 
	      (fun (scc, visited, frontier) edge ->
		let candidate = G.src edge in
		LOG (Printf.fprintf stderr "    Considering candidate %d..." (dfst.DFST.post candidate));
		if not (NodeSet.mem candidate visited) && (dfst.DFST.post candidate > binumber)
		then begin
		  LOG (Printf.fprintf stderr "    added frontier.\n");
		  scc, (NodeSet.add candidate visited), candidate :: frontier		 
		end
		else begin
		  LOG (Printf.fprintf stderr "    skipped.\n");
		  scc, visited, frontier 
		end
	      ) 
	      (hd :: scc, visited, tl) 
	      (G.ins hd) 
	  in
	  region dfst visited binumber scc frontier	  

    let detectSCS dfst node =
      snd (region dfst NodeSet.empty (dfst.DFST.post node) [] [node])

    let detectSCCs dfst =
      let n = G.nnodes (dfst.DFST.graph) in
      let rec traverse i visited sccs =
	LOG (Printf.fprintf stderr "Traversing N=%d\n" i);
	if i = n 
	then sccs
	else 
	  let node = dfst.DFST.post'1 i in
	  if not (NodeSet.mem node visited) 
	  then begin
	    LOG (Printf.fprintf stderr "  Building region for %d\n" i);
	    let visited, scc = region dfst visited i [] [node] in
	    traverse (i+1) visited ((node, scc) :: sccs)
	  end
	  else traverse (i+1) visited sccs
      in
      let sccs = lazy (traverse 0 NodeSet.empty []) in
      {
       sccs     = (fun () -> Lazy.force sccs);
       dfst     = dfst;
       toString = (fun () -> G.printClustered (dfst.DFST.graph) (snd (split (Lazy.force sccs))) (dfst.DFST.nodeString, dfst.DFST.edgeString));
      }
      
  end

module PathCovering (G : Digraph.Sig) =
  struct

    module DFST = DFST(G)
    module Aux  = Digraph.Make
	(
	 struct 
	   type t = G.Node.t option 
	   let toString = function Some node -> G.Node.toString node | _ -> "<none>"
	 end
	)
	(
	 struct 
	   type t = G.Edge.t option * int
	   let toString = function (Some edge), _ -> G.Edge.toString edge | _ -> "<none>"
	 end
	)

    module MaxFlow = Flow.Ford_Fulkerson 
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
	
    module NodeHash = Hashtbl.Make(G.Node)

    type dfst = DFST.info
    type info = {dfst : DFST.info; edges : unit -> G.Edge.t list; nodes : unit -> G.Node.t list; toString : unit -> string}

    let create info =
      let build info =
	let graph       = info.DFST.graph in
	let start       = info.DFST.start in
	let aux         = Aux.create () in
	let aux, source = Aux.insertNode aux None in
	let aux, sink   = Aux.insertNode aux None in
	let hash        = NodeHash.create (G.nnodes graph) in
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
	    (G.nodes graph) 
	in
	let aux = 
	  fold_left
	    (fun aux edge ->
	      match info.DFST.sort edge with
	      | DFST.Back -> aux
	      | _ ->
		  let n, _ = NodeHash.find hash (G.src edge) in
		  let _, k = NodeHash.find hash (G.dst edge) in
		  let aux, _ = Aux.insertEdge aux n k (Some edge, 1) in
		  aux
	    )
	    aux
	    (G.edges graph)
	in
	LOG (Printf.fprintf stderr "Flow Graph:\n%s\n" (Aux.toString aux));
	let flows, _ = MaxFlow.maxflow aux source sink in
	let module NodeSet = Set.Make (G.Node) in
	let edges, nodes   = fold_left 
	  (fun (list, nodes) edge -> 
	    if flows edge > 0 then 
	      begin
		LOG (Printf.fprintf stderr " Adding edge to maximal flow: %s\n" (Aux.Edge.toString edge));
		match Aux.Edge.info edge with
		| None, _   -> list, nodes
		| Some e, _ -> 
		    (e :: list), (NodeSet.add (G.dst e) (NodeSet.add (G.src e) nodes))
	      end
	    else list, nodes
	  ) ([], NodeSet.empty) (Aux.edges aux)
	in
	LOG (
	  Printf.fprintf stderr "Covered node set:\n";
	  NodeSet.iter (fun node -> Printf.fprintf stderr "%s; " (G.Node.toString node)) nodes;
	  Printf.fprintf stderr "\n";
        );
	let nodes = List.filter (fun node -> 
	  LOG (Printf.fprintf stderr "Checking node %s\n" (G.Node.toString node)); 
	  not (NodeSet.mem node nodes)) 
	    (G.nodes graph) 
	in
	LOG (
	  Printf.fprintf stderr "Single node list:\n";
	  List.iter (fun node -> Printf.fprintf stderr "%s; " (G.Node.toString node)) nodes;
	  Printf.fprintf stderr "\n";
        );
	edges, nodes
      in
      let data = lazy (build info) in
      {
       dfst     = info;
       edges    = (fun () -> fst (Lazy.force data));
       nodes    = (fun () -> snd (Lazy.force data));
       toString = 
         (fun () -> 
	   let module EdgeSet = Set.Make (G.Edge) in
	   let marker = fold_left (fun set edge -> EdgeSet.add edge set) EdgeSet.empty (fst (Lazy.force data)) in
	   let edgeAttrs edge =
	     Printf.sprintf "[label=\"%s\", color=%s]" 
	       (G.Edge.toString edge) 
	       (if EdgeSet.mem edge marker then "green" else "black")
	   in
	   G.print info.DFST.graph (info.DFST.nodeString, edgeAttrs)
	 )
      } 

    type path = Single of G.Node.t | Path of (G.Node.t * G.Edge.t) list

    let linearize info =
      let edges = info.edges ()              in
      let module NodeSet = Set.Make (G.Node) in
      let module NodeMap = Map.Make (G.Node) in
      let check set elt num = if NodeSet.mem elt set then num else 0 in
      let heads, tails, path = 
	fold_left 
	  (fun (heads, tails, path) edge ->
	    let src, dst = G.src edge, G.dst edge in
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
	      build (G.dst edge) ((node, edge) :: list)
	  in
	  (Path (rev (build head []))) :: fragments
	) 
	heads 
	[]) @ (List.map (fun node -> Single node) (info.nodes ()))
	
  end
