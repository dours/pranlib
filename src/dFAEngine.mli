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

    module PV : ProgramView.Sig

    exception Unreachable of [ `Node of PV.G.Node.t | `Edge of PV.G.Edge.t ] 

    exception RangeError  of int

    val analyse : PV.G.t -> PV.G.Node.t -> PV.AV.L.t

  end

module Forward(PV : ProgramView.Sig) : Sig with module PV = PV
