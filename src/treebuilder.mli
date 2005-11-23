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
 * (enclosed in the file COPYING).
 *)

(** Tree construction module *)

(** {1 Tree builder signature} *)

module type SIG =
sig

  type t
  (** Type of tree node *)

  (** Hashtalbe with type of key [t] *)
  module HSHT:Hashtbl.S with type key = t

  (** Type of constructed tree *)
  type info_out = 
  {
    get_childs : t -> t list;
    (** Gets list of childs of the specified node *)

    get_parent : t -> t option;
    (** Gets parent of node specified. [None] is returned for root *)

    is_child : t -> t -> bool;
    (** Returns [true] if the [second node] is ancestor of the [fist node] in tree *)

    get_root : t
    (** Returns root of tree *)
  }

  (** Type of information that has to be provided for tree construction *)
  type info_in = 
  {
    iter : (t -> t -> unit) -> unit;
   (** Function iterates [nodes] of tree and applies it's first argument (of type [t -> t -> ()]) 
       to the iterated node and it's parent.) 
   *)

    root : t;
    (** Root of tree *)

    size : int;
    (** Size of tree. Exact value is not nessary *)
  }

  val create : info_in -> info_out
  (** Creates tree using [info_in] *)

end
(** Tree builder module signature *)

(** {1 Tree builder instantiation} *)

(** Type of the tree node *)
module type TYPE =
sig 

  type t

  val equal    : t -> t -> bool
  val hash     : t -> int
  val toString : t -> string 

end

(** Wrapper for int tree node *)
module TYPE_INT : TYPE with type t = int

(** Tree builder constructor *)
module Make (T : TYPE) : SIG with type t = T.t


