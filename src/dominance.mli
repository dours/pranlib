(*
 * Dominance: Dominance tree contruction.
 * Copyright (C) 2005
 * Serjic Shkredov, St.Petersburg State University
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

(** {1 Dominance relation and dominance tree construction} *)

(** General signature for dominance relation *)
module type Sig =
  sig

    (** Depth-first search tree *)
    module T : DFST.Sig

    (** Graph module *)
    module G : CFG.Sig

    (** Type of the tree node *)
    type t = G.Node.t

    (** Root of the immediate dominators tree *)
    val root : t

    (** Parent of the given node *)
    val parent : t -> t option

    (** List of sons of given nodes *)
    val children : t -> t list

    (** [dominates x y] checks whether [x] dominates [y] *)
    val dominates  : t -> t -> bool
	
    (** [dominators x] gets list of all strict dominators for [x]. The list is
        ordered according to the dominance relation (e.g. immediate dominator 
        comes first) 
    *)
    val dominators : t -> t list

    (** Dominance tree DOT vizualiser *)
    module Tree : DOT.Sig with type parm = unit

    (** DOT visualizer: shows graph + its dominance tree *)
    module DOT  : DOT.Sig with type parm = unit
	
  end

(** Build dominance relation by the Lengauer-Tarjan algorithm *)
module Make (D: DFST.Sig) : Sig with module T = D and module G = D.G
