(*
 * Order: Numeration For Directed Graph.
 * Copyright (C) 2004-2006
 * Gennadiy A. Sych, St.Petersburg State University
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

(** {1 Vertecies order for directed graphs} *)

(** General signature for Order *)
module type Sig =
    sig
        (** A graph module the numeration is built for *)
        module G : Digraph.Sig
        
        (** Exception raised when the Order properties are queried for the 
            unreachable node/edge *)
        exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
 
        (** Exception raised when parameter for the [inverse] goes out of
            the valid range *)
        exception RangeError of int


        (** The direct mapping of the numeration. [direct node] returns number 
            of the [node] in the order. *)
        val direct  : G.Node.t -> int

        (** The inverse mapping of the numeration. [inverse num] returns node
            whose number is [num] in the numeration. *)
        val inverse : int -> G.Node.t
    end
