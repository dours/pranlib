(*
 * Treebuilder: common tree construction libriary.
 * Copyright (C) 2004
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
 * (enclosed in the file LGPL).
 *)

(** Tree construction module *)

(** {1 Tree builder signature} *)

module type SIG =
sig

  type t
   (** Type of tree node *)

   (** Hashtalbe with type of key [t] *)
  module HSHT:Hashtbl.S with type key = t

  type info_out = 
  {

    get_childs : t -> t list;
   (** Gets list of childs of node specified *)

    get_parent : t -> t option;
   (** Gets parent of node specified. [None] is returned for root *)

    is_child : t -> t -> bool;
   (** Returns [true] if [second node] is ansettor of [fist node] in tree *)

    get_root : t
   (** Returns root of tree *)

  }
   (** Type of tree constructed *)

  type info_in = 
  {

    iter : (t -> t -> unit) -> unit;
   (** Function iterates [nodes] of tree and applies it's first argument (of type [t -> t -> ()] to the iterated node and it's parent.) *)

    root : t;
   (** Root of tree *)

    size : int;
   (** Size of tree. Exact value is not nessary *)

  }
   (** Type of information that should be provided for tree construction *)

  val create : info_in -> info_out
   (** Creates tree using [info_in] *)

end
(** Tree builder module signature *)

(** {1 Tree builder instantiation} *)

module type TYPE =
sig 

  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val toString : t -> string 

end
(** Type of tree node *)

module TYPE_INT : TYPE with type t = int
(** Type of tree node [int] *)

module Make (T:TYPE):SIG with type t = T.t
(** Tree builder instance *)


