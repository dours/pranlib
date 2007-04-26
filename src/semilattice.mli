(*
 * Semilattice: semilattice abstraction.
 * Copyright (C) 2007
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

(** General semilattice representation *)

(** Basic signature for semilattice *)
module type Base =
  sig

    (** Type of semilattice element *)
    type t

    (** Intersection operation *)
    val cap : t -> t -> t

    (** The top element of the semilattice *)
    val top : t

    (** The bottom element of the semilattice *)
    val bottom : t

    (** Equality function *)
    val equal : t -> t -> bool

  end

(** Main semilattice signature *)
module type Sig =
  sig
    
    include Base

    (** Partial compare function; raises [Invalid_argument "Semilattice.compare"]
        when called for uncomparable elements
     *)
    val compare : t -> t -> int

    (** Tests comparability of elements *)
    val comparable : t -> t -> bool

    (** {3 Comparison synonyms} *)

    (** Greater *)
    val gt : t -> t -> bool
 
    (** Greater or equal *)
    val ge : t -> t -> bool

    (** Less *)
    val lt : t -> t -> bool

    (** Less or equal *)
    val le : t -> t -> bool    

  end

(** Instantiation functor *)
module Make (X : Base) : Sig with type t = X.t



