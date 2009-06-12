(*
 * Tree: tree interface and implementation.
 * Copyright (C) 2006
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

(** {1 Tree representation and manipulation module} *)

(** Read-only tree interface. *)
module type Read =
  sig

    (** Type of the node. *)
    type t

    (** List of sons. *)
    val children : t -> t list

  end

(** Read-only instance. *)
module type Instance =
  sig
   
    include Read

    (** Root of the tree. *)
    val root : t  

  end

(** Full-fledged tree interface. *)
module type Sig =
  sig
  
    include Read
 
    (** Type of the label in the node. *)
    type label

    (** [label x] gets the label of node [x]. *)
    val label : t -> label

    (** [make l c] makes intermediate node with label [l] and
        children [c].
     *)
    val make : label -> t list -> t 
   
  end

(** Tree constructor. *)
module Make (L : sig type t end) : Sig with type label = L.t

(** DOT tree printer. *)
module DOT :
  sig

    (** Printer instantiator. *)
    module Printer (T : Instance) (N : DOT.ExtInfo with type t = T.t) :
      sig

        include DOT.Sig with type parm = unit 

      end

  end
