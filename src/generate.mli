(*
 * Generate: directed graph generation.
 * Copyright (C) 2005
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

(** {1 Graph generation module} *)

(** Directed graph generation *)
module Digraph :
	sig

		(** A graph with simple nodes and edges *)
		module G : Digraph.Sig with type Node.info = unit and type Edge.info = unit

		(** Generate random graphs *)
		module Random :
			sig

				(** [create n m] creates random directed graph with [n] nodes and [m] edges *)
				val create : int -> int -> G.t

				(** Generate random control flow graph *)
				module ControlFlow :
					sig

						(** [create n m] creates random control flow graph with [n] nodes and [m] edges 
								and returns the graph and its source node *)
						val create : int -> int -> G.t * G.Node.t

					end

			end

	end
