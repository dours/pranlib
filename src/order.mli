(*
 * Order: Ordering For Control Flow Graphs.
 * Copyright (C) 2004-2006
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

(** {1 Vertecies order for control flow graphs} *)

(** General signature for Order *)
module type Sig =
  sig

    (** A graph module the numeration is built for *)
    module G : CFG.Sig
        
    (** Exception raised when the Order properties are queried for the 
	unreachable node/edge 
    *)
    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
 
    (** Exception raised when parameter for the [inverse] goes out of
	the valid range 
    *)
    exception RangeError of int

    (** Last number. All numbers has to be within the range [first..last] *)	
    val first : int

    (** Last number. All numbers has to be within the range [first..last] *)	
    val last : int

    (** Checks number to be withing the range [first..last] *)
    val valid : int -> bool
     
    (** The direct mapping of the numeration. [number node] returns number 
	of the [node] in the numeration
    *)
    val number : G.Node.t -> int
	
    (** The inverse mapping of the numeration. [node num] returns node
	whose number is [num] in the numeration. 
    *)
    val node : int -> G.Node.t

  end
      
(** Reversing the order *)
module Rev (X : Sig) : Sig with module G = X.G

(** Normalizing the order (transform it to range [0..n]) *)
module Normalize (X : Sig) : Sig with module G = X.G


