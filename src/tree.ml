(*
 * Tree: tree manipulation functions.
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

module type Tree =
  sig

    type t

    val root     : t
    val parent   : t -> t option
    val children : t -> t list

  end

module DOT =
  struct

    module Printer (T : Tree) (N : DOT.Node with type t = T.t) =
      struct

	include DOT.Printer (struct type t = unit let keyword _ = "digraph" let name _ = "X" let attrs _ = [] end) (N)
	
	open List
	open Printf

	let toDOT () =
	  sprintf "%s %s %s" 
	    (header ()) 
	    (
	     let rec inner acc n =
	       fold_left
		 inner
		 (fold_left 
		    (fun acc child -> 
		      acc ^ (sprintf "%s -> %s;\n  " (N.name n) (N.name child))
		    ) 
		    (acc ^ "\n  " ^ (node n) ^ "\n  ") 
		    (T.children n)
		 )		 
		 (T.children n)
	     in
	     inner "" T.root
	    )
	    (footer ())
	
      end

  end
