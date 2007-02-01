(*
 * RegularCFG: Regular Control Flow Graph (a CFG with the unique end node) Interface.
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

(** {1 Regular Control-Flow Graph (a CFG with the unique end node) interface} *)

(** General signature for regular control-flow graph *)
module type Sig =
  sig

    (** Underlying CFG *)
    include CFG.Sig

    (** End node *)
    val stop : Node.t

  end

(** Functor to instantiate regular CFG *)
module Make (S : CFG.Sig) : Sig with
   module Node = S.Node and module Edge = S.Edge and type t = S.t
