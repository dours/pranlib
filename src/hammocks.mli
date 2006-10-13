(*
 * Hammocks: hammock hierarchy construction.
 * Copyright (C) 2005
 * Gennadiy Sych, St.Petersburg State University
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

(** {1 Hammock hierarchy construction } *)

(** General constructor *)

module Make (T : DFST.Sig) :
    sig

      (** A DFST module with digraph hierarchy is built for *)
      module T : DFST.Sig with module G = T.G

      (** K numeration module *)
      module K : Order.Sig with module G = T.G

      module G : Digraph.Sig with type t = T.G.t and module Node = T.G.Node


(*		module L : Order.Sig with module G = T.G

		module Hierarchy :
			sig

				type t

				val build : T.t -> t

			end
*)
	  (** [get node] constructs and returns a list of pairs (bg, ham) where
	     ham is a hammock with bg (=node) vertext of Begin set *)
		(*val get : T.G.Node.t -> (T.G.Node.t * T.G.Node.t list) list*)

      (** [hammocks ()] constructs a list of triples (begin, k, q) where
	  K'[begin..k] is a hammock with q end vertex 
      *)
      val hammocks : unit -> (int * int * int) list

      (** DOT visualizer *)
      module DOT :
	sig

	  (** Node wrapper *)
	  module Node : DOT.Node with type t = T.G.Node.t

	  (** Edge wrapper *)
	  module Edge : Digraph.DOT.Edge with type t = T.G.Edge.t
				      
	  include Digraph.DOT.S with 
	     type graph = T.G.t      and 
	     type node  = T.G.Node.t and 
	     type edge  = T.G.Edge.t and 
	     type parm  = unit

	end
      
    end
