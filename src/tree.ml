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

module type Sig =
  sig

    type t

    val root     : t
    val parent   : t -> t option
    val children : t -> t list

  end

module DOT =
  struct

    module Printer (T : Sig) (N : DOT.ExtInfo with type t = T.t) =
      struct

        module G = 
          struct
            module Node = N

            module Edge = 
              struct
                type t = N.t * N.t
                let label _ = ""
                let attrs _ = []
                let nodes x = x
              end

           include DOT.Empty

           let name () = "Tree"
           let kind () = `Digraph

           let nodes () =
             let rec aux acc node = 
               List.fold_left aux (node :: acc) (T.children node)
             in aux [] T.root

           let edges () = 
             let rec aux acc node = 
               List.fold_left aux
                              (List.fold_left (fun acc dst -> (node, dst) :: acc) acc (T.children node))
                              (T.children node)
             in aux [] T.root

          end

	module DOTPrinter = DOT.Printer (G)

        type parm = unit
        let toDOT () = DOTPrinter.toDOT ()
	
      end

  end
