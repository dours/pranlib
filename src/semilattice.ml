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

module type Base =
  sig

    type t

    val cap : t -> t -> t

    val top : t

    val bottom : t

    val equal : t -> t -> bool

    val toString : t -> string

  end

module type Sig =
  sig

    include Base

    val compare : t -> t -> int
    val comparable : t -> t -> bool

    val gt : t -> t -> bool
    val ge : t -> t -> bool
    val lt : t -> t -> bool
    val le : t -> t -> bool    

  end

module Make (X : Base) =
  struct

    include X

    let compare x y =
      if equal x y then 0
      else if equal (cap x y) x then -1
      else if equal (cap y x) y then  1
      else invalid_arg "Semilattice.compare"

    let comparable x y =
      try ignore (compare x y); true with Invalid_argument "Semilattice.compare" -> false

    let gt x y = compare x y >  0
    let ge x y = compare x y >= 0
    let lt x y = gt y x
    let le x y = ge y x

  end
