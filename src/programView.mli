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
module type Sig =
  sig

    module AV : AlgView.Sig 

    module VA : ViewAdapter.Sig with type nt = AV.t 

    module G : CFG.Sig with type Node.info = VA.gnt

    (** [flow node] returns the flow function associated with the given node *)
    val flow : G.Node.t -> (AV.L.t -> AV.L.t)

    (** [init node] returns initial semilattice element associated with the given node *)
    val init : G.Node.t -> AV.L.t

  end  
    
module Make (AV : AlgView.Sig) 
            (VA : ViewAdapter.Sig with type nt = AV.t) 
            (G : CFG.Sig with type Node.info = VA.gnt) : Sig with
       module AV = AV and
       module VA = VA and
       module G = G
    