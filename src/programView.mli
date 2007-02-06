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


(** {1 Program abstraction interface for DFA} *)

(** General signature for program abstraction *)
module Make (AV : AlgView.Sig) :
  sig

    (** Control-flow graph of the program *)

    (** Specific analysis semilattice *)

    (** [flow node] returns the flow function associated with the given node *)
    val flow : AV.VA.G.Node.t -> (AV.L.t -> AV.L.t)

    (** [init node] returns initial semilattice element associated with the given node *)
    val init : AV.VA.G.Node.t -> AV.L.t

  end