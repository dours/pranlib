(*
 * RD: implements 'Reaching definitions' data analysis problems
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

(** Semilattice. *)
  module RDSemilattice
                    (DFAC : DFACommon.Sig)
                    (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
                    (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge) :
  sig
	
    include Semilattice.Base
  
    val make_empty : bool -> DFAC.RBitVector.t
  
    val make_empty_top : unit -> DFAC.RBitVector.t
  
    val make_empty_bottom : unit -> DFAC.RBitVector.t

  end
    
  (** Reaching definitions analysis results module. *)
  module RDResults
                (DFAC : DFACommon.Sig)
                (A: ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and type Abstract.edge = DFAC.EdgeInfo.t)
                (G : CFG.Sig with type Node.t = A.Concrete.node and type Edge.t = A.Concrete.edge) :
  sig

    (** Reaching definitions information (Basically a bit-vector). *)
    type rdInfo = DFAC.RBitVector.t
	
    (** [before n] returns reaching definitions information before executing node statements. *)
    val before : G.Node.t -> rdInfo
	
    (** [after n] returns reaching definitions information after executing node statements. *) 
    val after : G.Node.t -> rdInfo 

  end