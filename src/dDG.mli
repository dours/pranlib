(*
 * DDG: Data-Dependency Graph interface
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

(* DDG interface *)
module type Sig =
sig

	(* Underlying graph *)
  include Digraph.Sig

	(* Graph itself *)
  val graph : t

	(* Start node *)
	val start : Node.t

end

module Make (S : Digraph.Sig) (G: sig val graph : S.t val start : S.Node.t end) : Sig with
   module Node = S.Node and module Edge = S.Edge and type t = S.t