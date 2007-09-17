(*
 * Tree: tree manipulation functions.
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

(** Generic tree interface *)
module type Sig =
  sig

    (** Type of the node *)
    type t

    (** Root of the tree *)
    val root : t

    (** Parent of the node *)
    val parent : t -> t option

    (** List of sons *)
    val children : t -> t list

  end

(** DOT tree printer *)
module DOT :
  sig

    (** Printer instantiator *)
    module Printer (T : Sig) (N : DOT.Node with type t = T.t) :
      sig

	include DOT.Sig with type graph = unit and type node = N.t

        (** Printing function *)
	val toDOT : unit -> string

      end

  end
