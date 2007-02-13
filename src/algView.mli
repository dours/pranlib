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

(** Representation of DFA flow function *)
module type Sig =
  sig
     
    (** type of node *)
    type t

    type sl_t

    module L : Semilattice.Sig with type t = sl_t
 
    val flow : t -> (sl_t -> sl_t)

    val init : t -> sl_t

  end

type rd_sl = Bitv.t

type rd_node = {def : Bitv.t; kill : Bitv.t}

module CilToDefUseAdapter : ViewAdapter.Sig 
         with type gnt = string and
              type nt = rd_node

module BitvRDSemilattice : Semilattice.Sig 
         with type t = rd_sl
           
module RDMake : Sig 
         with type t = rd_node and
              type sl_t = rd_sl
                       









