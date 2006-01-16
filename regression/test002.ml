(*
 * Test002: testing dominance tree construction.
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
  let module X = struct type t = string let toString x = x end in
  let module G = Digraph.Make (X)(X) in
  let node = G.insertNode in
  let edge = G.insertEdge in
  let g = G.create () in
  let g, ns = node g "start" in
  let g, na = node g "a" in
  let g, nb = node g "b" in
  let g, nc = node g "c" in
  let g, nd = node g "d" in
  let g, ne = node g "e" in
  let g, nf = node g "f" in
  let g, ng = node g "g" in
  let g, nh = node g "h" in
  let g, ni = node g "i" in
  let g, nj = node g "j" in
  let g, nk = node g "k" in
  let g, nl = node g "l" in
  let g, nm = node g "m" in
  let g, _  = edge g ns na "" in
  let g, _  = edge g na nb "" in
  let g, _  = edge g na nl "" in
  let g, _  = edge g nb nc "" in
  let g, _  = edge g nb ne "" in
  let g, _  = edge g nc nd "" in
  let g, _  = edge g ne nf "" in
  let g, _  = edge g nf ne "" in
  let g, _  = edge g nf nc "" in
  let g, _  = edge g nd ng "" in
  let g, _  = edge g nf ng "" in
  let g, _  = edge g ng nh "" in
  let g, _  = edge g ng ni "" in
  let g, _  = edge g ni nh "" in
  let g, _  = edge g ni nk "" in
  let g, _  = edge g nk nj "" in
  let g, _  = edge g nj ni "" in
  let g, _  = edge g nl nk "" in
  let g, _  = edge g nj nl "" in
  let g, _  = edge g nh nm "" in
  let module D = Dominance.Make (DFST.Make (G) (struct let graph = g let start = ns end)) in
  Printf.printf "%s" (D.Tree.toDOT ())
