(** This module is to find cycles in a graph that is NOT a CFG. DFST can't be used for such graphs
    because it makes some assertions about what a CFG should look like *)
    
module type Sig = sig

type graph 
type edge

(** if you remove all the edges returned by [cycleBreakers g] from [g], the graph will become a DAG *)
val cycleBreakers : graph -> edge list 

end

module Make(G : Digraph.Sig) : Sig with 
  type graph = G.t and 
  type edge = G.Edge.t 
