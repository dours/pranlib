(*
 * Test003: testing dominance tree construction.
 * Copyright (C) 2006
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
let _ =  
  let x = Bitv.create 12 true in
  let module X = struct type t = string let toString x = x end in
  let module GG = Digraph.Make (X) (X) in
  let g = GG.create () in
  let g, a = GG.insertNode g "a" in
  let module CFG = CFG.Make (GG) (struct let graph = g let start = a end) in
  let module RDMD = ProgramView.Make (AlgView.RDMake) (AlgView.CilToDefUseAdapter) (CFG) in
  ()