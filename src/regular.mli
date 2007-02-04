(*
 * Region: Region Construction.
 * Copyright (C) 2004-2006
 * Gennadiy Sych, St.Petersburg State University
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

(*
 * Source is under development.
 *
 * Provides interfaces for structural program representation and invariant 
 * transformations.
 *)

type 'a regularProgram =  
                      Compound of 'a regularProgram* 'a regularProgram
                    | Loop of 'a regularProgram*'a regularProgram
                    | If of 'a regularProgram*'a regularProgram*'a regularProgram*'a regularProgram
                    | Operator of 'a
                    | Condition of 'a regularProgram
                    | Or of 'a regularProgram * 'a regularProgram 
                    | And of 'a regularProgram * 'a regularProgram
                    | Not of 'a regularProgram
                    | BoolTrue
                    | BoolFalse  
                    | TempVariableSet of int
                    | TempVariableReset of int
                    | TempVariableCheck of int
                    | Empty

module type Sig =
sig

   module G_OLD : Digraph.Sig 

   module G : Digraph.Sig
  
   val reduce_all : unit -> G.t
   
end


module Make (G_OLD : Digraph.Sig) 
            (X : sig val graph : G_OLD.t val start : G_OLD.Node.t end) : Sig 
