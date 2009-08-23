(*
 * LV: implements 'Live variables' data analysis problems
 * Copyright (C) 2008
 * Andrey Serebryansky, St.Petersburg State University
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
open DFACommon

(** Live variable analysis semilattice. *)
module LVSemilattice
				(DFAC : Sig)
                (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
                (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge) :
  sig

    include Semilattice.Base
  
    val make : bool -> DFAC.BitVector.t
  
    val make_top : unit -> DFAC.BitVector.t
  
    val make_bottom : unit -> DFAC.BitVector.t

end

(** Live variable analysis results creation functor. *)
module LVResults 
               (DFAC : Sig) 
               (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
               (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge) :
  sig

    (** Type of live variable analysis information. *)
    type lvInfo = DFAC.BitVector.t
	
    (** [before n] Returns live variable informaton on entrance into node [n]. *)
    val before : G.Node.t -> lvInfo 
	
    (** [after n] Returns live variables information on exit of node [n]. *)
    val after : G.Node.t -> lvInfo

  end