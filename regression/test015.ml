(*
 * Test015: Testing live variables analysis in a complex case
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
let module DFATC = DFATestCommon.Make in
let module DC = DFATC.DFAC in
let module V = DFATC.DFAC.V in
let module EI = DC.EdgeInfo in
let module G = DFATC.G in
let module PVA = DFATC.PVA in
let g = DFATC.G.create () in
(* b1 *)
let a1 = [DFATC.StatementConstructor.construct "a" []] in let g, n1 = G.insertNode g a1 in
let a2 = [DFATC.StatementConstructor.construct "b" []] in let g, n2 = G.insertNode g a2 in
let a3 = [DFATC.StatementConstructor.construct "d" []] in let g, n3 = G.insertNode g a3 in
  
(* b2 *)
let a4 = [DFATC.StatementConstructor.construct "c" ["a";"b"]] in let g, n4 = G.insertNode g a4 in
let a5 = [DFATC.StatementConstructor.construct "d" []] in let g, n5 = G.insertNode g a5 in

(* b3 *)
let a6 = [DFATC.StatementConstructor.construct "c" []] in let g, n6 = G.insertNode g a6 in
let g, _ = G.insertEdge g n1 n2 DC.EdgeInfo.Empty in
let g, _ = G.insertEdge g n2 n3 DC.EdgeInfo.Empty in
let g, _ = G.insertEdge g n3 n4 DC.EdgeInfo.Empty in
let g, _ = G.insertEdge g n4 n5 DC.EdgeInfo.Empty in
let g, _ = G.insertEdge g n5 n6 DC.EdgeInfo.Empty in
let g, _ = G.insertEdge g n3 n6 DC.EdgeInfo.Empty in
let module MYG = CFG.Make (G)(struct let graph=g let start=n1 end) in
let module LVR = Lv.LVResults(DC)(PVA)(MYG) in
printf "Started logging...\n";
printf "Graph:\n\n%s\n" (G.DOT.toDOT g);
let lvrResults = LVR.after n3 in
printf "LV results:\n\n%s\n" (DC.BitVector.toString lvrResults)