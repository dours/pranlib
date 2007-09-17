(*
 * DOT: basic graph printing interface.
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

module type Graph =
  sig
    
    type t

    val keyword : t -> string
    val name    : t -> string          
    val attrs   : t -> (string * string) list
        
  end
      
module type Node =
  sig
    
    type t

    val attrs : t -> (string * string) list        
    val label : t -> string        
    val name  : t -> string
        
  end

module type Sig =
  sig

    type graph
    type node

    val header     : graph -> string
    val footer     : graph -> string
    val node       : node -> string
    val nodes      : node list -> string
    val attributes : string -> (string * string) list -> string

    module Clusters :
      sig

	type t = Node of node list * t list | Leaf of node list

      end

  end

module Printer (G : Graph) (N : Node) =
  struct

    open Printf

    type graph = G.t
    type node  = N.t

    let attributes label attrs =
      let module M = View.List (
	struct 
	      
	  type t = string * string 
		
	  let toString (x, y) = sprintf "%s=%S" x y
	      
	end
       ) 
      in
      M.toString (match label with "" -> attrs | _ -> ("label", label) :: attrs)

    module Concat = struct let concat = View.concatWithDelimiter "\n  " end 

    let header g = sprintf "%s %s {  %s\n" (G.keyword g) (G.name g) (attributes "" (G.attrs g))
    let footer g = sprintf "\n}\n"

    let node n = sprintf "%s [%s];" (N.name n) (attributes (N.label n) (N.attrs n))
    let nodes list = 
      let module M = View.ListC (Concat) (struct type t = N.t let toString = node end) in
      M.toString list

    module Clusters =
      struct

	type t = Node of node list * t list | Leaf of node list

      end

  end
