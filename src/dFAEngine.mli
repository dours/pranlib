(*
 * DFAEngine: implements forward, backward and bidirectional DFA engines.
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

(** {1 DFAEngine : variaous DFA solvers} *)

(** Generic solver: takes program view and order to process nodes *)
module Generic (P : ProgramView.Sig) (O : Order.Sig with module G = P.G) :
  sig

    (** Function to get DFA solution for edge *)
    val get : P.G.Edge.t -> P.L.t

  end

(** Solver which processes nodes in postorder *)
module Post (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) :
  sig

    (** Function to get DFA solution for edge *)
    val get : P.G.Edge.t -> P.L.t

  end

(** Solver which processes nodes in reversed postorder *)
module RevPost (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) :
  sig

    (** Function to get DFA solution for edge *)
    val get : P.G.Edge.t -> P.L.t

  end

(** Solver which processes nodes first in postorder then in reversed postorder 
    on each iteration (for bidirectional analyses)
  *)
module PostRevPost (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) :
  sig

    (** Function to get DFA solution for edge *)
    val get : P.G.Edge.t -> P.L.t

  end

(** Solver which processes nodes first in reversed postorder then in postorder 
    on each iteration (for bidirectional analyses)
  *)
module RevPostPost (P : ProgramView.Sig) (T : DFST.Sig with module G = P.G) :
  sig

    (** Function to get DFA solution for edge *)
    val get : P.G.Edge.t -> P.L.t

  end

