(*
 * Test001: testing basic graph operations.
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

open Printf

let _ =
  let module X = struct type t = string let toString x = x end in
  let module G = Digraph.Make (X) (X) in
  let g    = G.create () in
  let g, x = G.insertNode g "x" in
  let g, y = G.insertNode g "y" in
  let g, z = G.insertNode g "z" in
  let g, t = G.insertNode g "t" in

  printf "Graph:\n\n%s\n" (G.toDOT g);
  printf "Number of edges: %d\n" (G.nedges g);
  printf "Number of nodes: %d\n" (G.nnodes g);
  printf "lastNode: %d\n" (G.lastNode g);
  printf "lastEdge: %d\n" (G.lastEdge g);
  printf "Copy:\n\n%s\n" (G.toDOT (G.copy g));

  let g, a = G.insertEdge g x y "a" in
  let g, b = G.insertEdge g y z "b" in
  let g, c = G.insertEdge g z t "c" in
  let g, d = G.insertEdge g t x "d" in

  printf "Graph:\n\n%s\n" (G.toDOT g);
  printf "Number of edges: %d\n" (G.nedges g);
  printf "Number of nodes: %d\n" (G.nnodes g);
  printf "lastNode: %d\n" (G.lastNode g);
  printf "lastEdge: %d\n" (G.lastEdge g);

  let g'  = G.copy g  in
  let g'' = G.copy g' in

  printf "Copy:\n\n%s\n" (G.toDOT g');

  let g = G.deleteEdge g a in

  printf "Deleted edge \"a\":\n\n%s\n" (G.toDOT g);

  let g = G.deleteEdge g a in

  printf "Deleted edge \"a\" again:\n\n%s\n" (G.toDOT g);

  let x :: _ = G.nodes g' in
  let g' = G.deleteNode g' x in

  printf "Deleted node %s:\n\n%s\n" (G.Node.toString x) (G.toDOT g');

  let g' = G.deleteNode g' x in

  printf "Deleted node %s again:\n\n%s\n" (G.Node.toString x) (G.toDOT g');

  let g = g'' in
  let x :: _ = G.nodes g in

  let g, _ = G.replaceNode g x "replaced" in
  
  printf "Replaced node %s:\n\n%s\n" (G.Node.toString x) (G.toDOT g);

  begin try
    let g, _ = G.replaceNode g x "replaced again" in
    printf "Oops, replaced node %s replaced again:\n\n%s\n" (G.Node.toString x) (G.toDOT g)
  with
  | Failure "node does not belong to the graph" -> printf "Exception raised on trying to replace detached node.\n"
  end;

  let e :: _ = G.edges g in

  let g, _ = G.replaceEdge g e "replaced" in
  
  printf "Replaced edge %s:\n\n%s\n" (G.Edge.toString e) (G.toDOT g);

  begin try
    let g, _ = G.replaceEdge g e "replaced again" in
    printf "Oops, replaced edge %s replaced again:\n\n%s\n" (G.Edge.toString e) (G.toDOT g)
  with
  | Failure "edge does not belong to the graph" -> printf "Exception raised on trying to replace detached edge.\n"
  end

  


