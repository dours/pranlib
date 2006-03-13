(*
 * Region: Region Construction.
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

(** {1 Region construction} *)

(** General signature for Region *)
module type Sig =
    sig
        (** A graph module the region is built for *)
        module G : Digraph.Sig

        (** A numeration of the graph *)
        module F : Order.Sig
        
        (** [build node] returns a region in F numeration for [node] and [node] *)
        val build : G.Node.t -> G.Node.t * G.Node.t list
    end

(** Region constructor *)
module Make (G : Digraph.Sig) (F : Order.Sig with module G = G) : Sig with module G = G and module F = F
