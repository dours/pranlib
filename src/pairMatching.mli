(**************************************************************************
 *  Copyright (C) 2005
 *  Oleg Medvedev (dours@mail.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

(** 
    Finding maximal pairmatching in a weighted bipartite graph by the hungarian method.
    It uses a priority queue with O(log n) time for addition and minimum removal.
    The algorithm works in O(E * log E), where E is a number of edges.
*)

(** This module describes a bipartite graph *)
module type BiGraph =
  sig
  
    (** The type of the graph *)
    type t
    
    (** Number of boys in our bipartite graph.
	Boys are numbered consecutively from [0] to [nBoys-1] *)
    val nBoys : t -> int
    
    (** Number of girls in our bipartite graph.
	Girls are numbered consecutively from [0] to [nBoys-1] *)
    val nGirls : t -> int
    
    (** Array of edges of the form [((b, g), w)] (the edge connects a boy number [b] 
        with a girl number [g] and has a weight [w].  
    *)
    val edges : t -> ((int * int) * int) Urray.t

  end

(** Instantiates a function to find the pairmatching *)
module Make (G : BiGraph) :
    sig
    
      (** Returns a list of edges included in the maximal pairmatching *)
      val search : G.t -> ((int * int) * int) list

    end

(** Implementation of a full search in exponential time. 
    Used for debugging the fast algorithm *)
module MakeExhaustiveSearch (G : BiGraph) :
    sig 
    
      (** Returns a weight of some maximal pairmatching *)
      val search : G.t -> int 

    end
