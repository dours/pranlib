(*
 * CFG: Control Flow Graph Interface.
 * Copyright (C) 2004-2007
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

(** {1 Control-Flow Graph (CFG) interface} *)

(** General signature for control-flow graph *)
module type Sig =
  sig

    (** Underlying directed graph *)
    include Digraph.Sig

    (** The graph data *)
    val graph : t

    (** The starting node *)
    val start : Node.t

  end

(** Functor to instantiate CFG *)
module Make (S : Digraph.Sig) (G: sig val graph : S.t val start : S.Node.t end) : Sig with
   module Node = S.Node and module Edge = S.Edge and type t = S.t
    
