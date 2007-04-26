(*
 * ProgramView: program abstraction module for DFA.
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

(** Algorithm view defines data-flow environment: semilattice, flow functions and
    initial graph labeling
  *)

(** AlgView signature *)
module type Sig =
  sig

    (** Type of control flow graph node *)
    type t

    (** Semilattice module *)
    module L : Semilattice.Sig
 
    (** Node flow function *)    
    val flow : t -> (L.t -> L.t)

    (** Initial value for node *)
    val init : t -> L.t    

  end





