(*
 * DFAEngine: implements forward, backward and bidirectional DFA engines.
 * Copyright (C) 2004-2007
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


(**  *)
module type Sig =
  sig

    module G : CFG.Sig

    module L : Semilattice.Sig

    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 

    exception RangeError  of int

    val analyse : G.t -> G.Node.t -> L.t

  end

module Forward(AV : AlgView.Sig) : Sig with 
  module G = AV.VA.G and module L = AV.L