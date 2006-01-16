(*
 * Test007: testing path covering construction.
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
  let (g, n0) = node g "0" in
  let (g, n1) = node g "1" in
  let (g, n2) = node g "2" in
  let (g, n3) = node g "3" in
  let (g, n3') = node g "3'" in
  let (g, n4) = node g "4" in
  let (g, n4') = node g "4'" in
  let (g, n5) = node g "5" in
  let (g, n6) = node g "6" in
  let (g, n6') = node g "6'" in
  let (g, n7) = node g "7" in
  let (g, n7') = node g "7'" in
  let (g, n8) = node g "8" in
  let (g, n8') = node g "8'" in
  let (g, n9) = node g "9" in
  let (g, n9') = node g "9'" in
  let (g, _) = edge g n0 n2 "" in
  let (g, _) = edge g n2 n5 "" in
  let (g, _) = edge g n2 n6 "" in
  let (g, _) = edge g n5 n7 "" in
  let (g, _) = edge g n5 n6 "" in
  let (g, _) = edge g n5 n5 "" in
  let (g, _) = edge g n2 n7 "" in
  let (g, _) = edge g n6 n6' "" in
  let (g, _) = edge g n7 n7' "" in
  let (g, _) = edge g n6' n8 "" in
  let (g, _) = edge g n8 n8' "" in
  let (g, _) = edge g n8' n8 "" in
  let (g, _) = edge g n6' n3 "" in
  let (g, _) = edge g n8' n3 "" in
  let (g, _) = edge g n3 n3' "" in
  let (g, _) = edge g n7' n4 "" in
  let (g, _) = edge g n7' n9 "" in
  let (g, _) = edge g n4 n4' "" in
  let (g, _) = edge g n9 n9' "" in
  let (g, _) = edge g n9' n9 "" in
  let (g, _) = edge g n9' n4 "" in
  let (g, _) = edge g n3' n1 "" in
  let (g, _) = edge g n4' n1 "" in
  let module PC = PathCovering.MakeSimple (DFST.Make (G) (struct let graph = g let start = n0 end)) in
  Printf.printf "%s" (PC.toDOT ())
