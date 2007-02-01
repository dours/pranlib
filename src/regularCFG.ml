(*
 * RegularCFG: Regular Control Flow Graph Implementation.
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

module type Sig =
  sig

    include CFG.Sig

    val stop : Node.t

  end

module Make (S : CFG.Sig) =
  struct

    include S

    let stop = start

  end
