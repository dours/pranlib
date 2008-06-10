(** This module is to find cycles in a graph that is NOT a CFG. DFST can't be used for such graphs
    because it makes some assertions about what a CFG should look like *)
    
module type Sig = sig
type graph 
type edge
val cycleBreakers : graph -> edge list 
end

module Make(G : Digraph.Sig) : Sig with 
    type graph = G.t and
    type edge = G.Edge.t 
= struct 

type graph = G.t
type edge = G.Edge.t
    
(* it actually returns all back edges relatively to several random nodes *)
let cycleBreakers graph = 
  let module NodeHash = Hashtbl.Make(G.Node) in 
  let started = NodeHash.create (G.nnodes graph) in 
  let finished = NodeHash.create (G.nnodes graph) in 
  let answer = ref [] in 
  let rec process node = 
    NodeHash.add started node ();
    List.iter (fun edge -> 
      let dst = G.dst edge in 
      if NodeHash.mem started dst then begin
        if not (NodeHash.mem finished dst) then answer := edge :: !answer
      end 
      else process dst
    ) (G.outs node);
    NodeHash.add finished node () 
  in 
  List.iter (fun node -> if not (NodeHash.mem finished node) then process node) (G.nodes graph);
  !answer
  
end
        