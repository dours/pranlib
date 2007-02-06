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

(** {1 General semilattice representation} *)

(** General signature for semilattice *)
module type Sig =
  sig

    (** Type of semilattice element *)
    type t

    (** Intrsection operation *)
    val cap : t -> t -> t

    (** The top element of the semilattice *)
    val top : t

    (** The bottom element of the semilattice *)
    val bottom : t

  end