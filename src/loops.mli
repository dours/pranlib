(*
 * Loops: Loop finding algorithms.
 * Copyright (C) 2004-2006
 * Serjic Shkredov, St.Petersburg State University
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

(** {1 Loop finding algorithms} *) 

(** General functor to instantiate all loop finding algorithms *)
module Make (D: Dominance.Sig) :
  sig

    (** Depth-first search tree for given graph *)
    module T : DFST.Sig with module G = D.T.G

    (** The graph module *)
    module G : CFG.Sig with type t = T.G.t and module Node = T.G.Node and module Edge = T.G.Edge

    (** The graph data *)
    val graph : G.t

    (** The starting node *)
    val start : G.Node.t

    (** Region-based algorithms *)
    module Region :
      sig

	module R : Region.Sig with module G = G and module F = T.Post
		
        (** Strongly connected subgraphs formed by region by postorder *)
	module SCS :
	  sig

	    (** [get node] returns the list of nodes which form a region for the [node]. 
	        This set of nodes is strongly connected 
	     *)
	    val get : G.Node.t -> R.NodeSet.t
	
	  end
	    
        (** Strongly connected components *)
	module SCC :
	  sig
	  			
            (** Gets all strongly connected components and their bivertices.
		Bivertex is the vertex whose region is a strongly connected component 
	     *)
	    val get : unit -> (G.Node.t * R.NodeSet.t) list
		
	  end
	    
      end

  end


module NestedLoops (T : DFST.Sig) : 
sig 

  module LOOPS : Forest.Sig
      
  val buildLoops : unit -> LOOPS.t

end
 





























