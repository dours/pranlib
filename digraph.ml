open List

module type Sig =
  sig

    module Node :
      sig

	type t    
	type info 

	val toString : t -> string
	val hash     : t -> int
	val equal    : t -> t -> bool
	val info     : t -> info
	val compare  : t -> t -> int

      end

    module Edge :
      sig

	type t    
	type info 

	val toString : t -> string
	val hash     : t -> int
	val equal    : t -> t -> bool
	val info     : t -> info
	val compare  : t -> t -> int

      end
	
    type t
	
    val src : Edge.t -> Node.t
    val dst : Edge.t -> Node.t

    val ins  : Node.t -> Edge.t list
    val outs : Node.t -> Edge.t list
	
    val nnodes : t -> int
    val nedges : t -> int
	
    val nodes  : t -> Node.t list 
    val edges  : t -> Edge.t list
	  
    val create : unit -> t
	
    val insertNode  : t -> Node.info -> t * Node.t
    val insertEdge  : t -> Node.t -> Node.t -> Edge.info -> t * Edge.t

    val deleteEdges : t -> (Edge.t -> bool) -> t
    val deleteEdge  : t -> Edge.t -> t
      
    val deleteNodes : t -> (Node.t -> bool) -> t
    val deleteNode  : t -> Node.t -> t

    val replaceNode : t -> Node.t -> Node.info -> t * Node.t
    val replaceEdge : t -> Edge.t -> Edge.info -> t * Edge.t

    val print          : t -> ((Node.t -> string) * (Edge.t -> string)) -> string	
    val printClustered : t -> Node.t list list -> ((Node.t -> string) * (Edge.t -> string)) -> string
    val toString       : t -> string
	
  end

module type Info =
  sig
    
    type t 
	  
    val toString : t -> string
	
  end

module Id =
  struct

    let i = ref 0
    let provide () =
      let i' = !i in
      incr i;
      i'

  end

module Make (NodeInfo : Info) (EdgeInfo : Info) =
  struct

    type edgeBase = Edge of node * node
    and  nodeBase = Node of edge list ref * edge list ref

    and  edge     = edgeBase * EdgeInfo.t * int
    and  node     = nodeBase * NodeInfo.t * int

    let mid (_, x, _) = x
    let id  (_, _, x) = x

    module Node =
      struct

	type t    = node
	type info = NodeInfo.t

	let toString node = NodeInfo.toString (mid node)
	let hash     node = Hashtbl.hash (id node)
	let equal    x y  = (id x) = (id y)
	let info          = mid
	let compare x y   = (id x) - (id y)

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

      end
	
    type t = node list * edge list
	
    let src (Edge (x, _), _, _) = x
    let dst (Edge (_, x), _, _) = x

    let ins  (Node (x, _), _, _) = !x
    let outs (Node (_, x), _, _) = !x

    let nnodes (nodes, _) = length nodes
    let nedges (_, edges) = length edges
	
    let nodes (nodes, _) = nodes
    let edges (_, edges) = edges
 
    let create () = [], []
	
    let insertNode ((nodes, edges) : t) (info : Node.info) = 
      let node = Node (ref [], ref []), info, (Id.provide ()) in
      (node :: nodes, edges), node
	
    let insertEdge ((nodes, edges) : t) beg edn (info : Edge.info) =
      let edge = Edge (beg, edn), info, (Id.provide ()) in
      let Node (_, outs), _, _ = beg in
      let Node (ins,  _), _, _ = edn in
      outs := edge :: !outs;
      ins  := edge :: !ins;
      (nodes, edge :: edges), edge

    let deleteEdges ((nodes, edges) : t) predicate =
      let victims, edges = partition predicate edges in
      iter 
	(fun ((Edge ((Node (_, outs), _, _), (Node (ins, _), _, _)), _, _) as edge) -> 
	  let indicator e = (e == edge) in
	  outs := snd (partition indicator !outs);
	  ins  := snd (partition indicator !ins)
	) 
	victims;
      (nodes, edges)

    let deleteEdge graph edge =	deleteEdges graph (fun x -> x == edge) 
      
    let deleteNodes ((nodes, edges) as graph : t) predicate =
      let victims, nodes = partition predicate nodes in
      let _, edges =
	fold_left 
	  (fun graph (Node (ins, outs), _, _) ->
	    fold_left deleteEdge (fold_left deleteEdge graph !outs) !ins
	  )
	  graph
	  victims
      in
      nodes, edges

    let deleteNode graph node = deleteNodes graph (fun x -> x == node)

    let replaceNode graph node info =
      let graph, node' = insertNode graph info in
      let graph        = fold_left (fun g edge -> fst (insertEdge g (src edge) node' (Edge.info edge))) graph (ins  node) in
      let graph        = fold_left (fun g edge -> fst (insertEdge g node' (dst edge) (Edge.info edge))) graph (outs node) in
      deleteNode graph node, node'      

    let replaceEdge graph edge info =
      let s, d = src edge, dst edge in
      insertEdge (deleteEdge graph edge) s d info
	
    module NodeHash = Hashtbl.Make (Node)

    let buildIndex (nodes, edges) =
      let index = NodeHash.create 1024 in
      ignore (fold_left (fun curr node -> NodeHash.add index node curr; curr+1) 0 nodes);
      index

    let printEdges buffer edges edgeString hash =
      iter 
	(fun ((Edge (beg, edn), _, _) as edge) -> 
	  Buffer.add_string 
	    buffer 
	    (Printf.sprintf " node%d -> node%d %s;\n" 
	       (NodeHash.find hash beg) 
	       (NodeHash.find hash edn) 
	       (edgeString edge)
	    )
	) 
	edges

    let printNodes buffer nodes nodeString hash =
      iter 
	(fun node -> 
	  Buffer.add_string 
	    buffer 
	    (Printf.sprintf " node%d %s;\n" 
	       (NodeHash.find hash node) 
	       (nodeString node)
	    )
	) 
	nodes

    let printGraph graph func =
      let index  = buildIndex graph in      
      let buffer = Buffer.create 1024 in
      Buffer.add_string buffer "digraph G{\n";
      func buffer index;
      Buffer.add_string buffer "}\n";
      Buffer.contents buffer

    let print ((nodes, edges) as graph) (nodeString, edgeString) =
      printGraph graph
	(fun buffer index ->
	  printNodes buffer nodes nodeString index;
	  printEdges buffer edges edgeString index
	)
	
    let printClustered ((nodes, edges) as graph) clusters (nodeString, edgeString) =
      printGraph graph
	(fun buffer index ->
	  iter 
	    (fun cluster -> 
	      match cluster with
	      | [node]  -> printNodes buffer cluster nodeString index
	      | hd :: _ ->
		  Buffer.add_string buffer (Printf.sprintf "subgraph cluster_%d {\n" (NodeHash.find index hd));
		  printNodes buffer cluster nodeString index;
		  Buffer.add_string buffer "}\n"
	    )
	    clusters;
	  printEdges buffer edges edgeString index
	)
	
    let toString (nodes, edges) = 
      print (nodes, edges) 
	((fun node -> Printf.sprintf "[shape=box, label=\"%s\"]" (NodeInfo.toString (Node.info node))),
	 (fun edge -> Printf.sprintf "[label=\"%s\"]" (EdgeInfo.toString (Edge.info edge))))
	
  end
    
