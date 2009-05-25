(*
 * Test013: Testing live variable analysis in a simple case
 * Copyright (C) 2009
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
open Printf
open DFACommon
open DFATestCommon
open Lv

let _ =
let g = G.create () in
let a1 = [StatementConstructor.construct "a" ["b";"c"]] in
let a2 = [StatementConstructor.construct "d" ["e";"f"]] in
let g, n0 = G.insertNode g a1 in
let g, n1 = G.insertNode g a2 in
let g, e0 = G.insertEdge g n1 n0 EdgeInfo.Empty in
let module MYG = CFG.Make (G)(struct let graph=g let start=n1 end) in
let module LVR = Lv.LVResults(PVA)(MYG) in
printf "Started logging...\n";
printf "Graph:\n\n%s\n" (G.DOT.toDOT g);
let lvrResults = LVR.before n0 in
printf "LV results:\n\n%s\n" (BitVector.toString lvrResults)