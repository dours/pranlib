(*
 * Order: Ordering For Control Flow Graphs.
 * Copyright (C) 2004-2006
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

module type Sig =
  sig

    module G : CFG.Sig
        
    exception Unreachable of [ `Node of G.Node.t | `Edge of G.Edge.t ] 
    exception RangeError of int

    val last : int
    val number : G.Node.t -> int
    val node : int -> G.Node.t

  end

module Rev (X : Sig) =
  struct

    module G = X.G
	
    exception Unreachable of [ `Edge of G.Edge.t | `Node of G.Node.t ]
    exception RangeError of int
	
    let last        = X.last
    let number node = last - (X.number node)
    let node   num  = X.node (last - num)

  end
      
