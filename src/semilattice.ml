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

module type Sig =
  sig

    type t
    val cap : t -> t -> t
    val top : t
    val bottom : t
    val equal : t -> t -> bool
  end

module TestMake = 
  struct
    type t = string
    let top = ""
    let bottom = ""
    let cap x y = ""
    let equal x y = true
  end