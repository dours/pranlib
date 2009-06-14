(*
 * Forest: forest manipulation functions.
 * Copyright (C) 2006
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

(** Abstract forest signature. *)
module type Sig = 
  sig

    (** Forest type. *)
    type t
    
    (** Type of elements stores in the forest. *)
    type node

    (** creates empty forest. *)
    val create : unit -> t
     
    (** joins it's first parameter of node to the second. *)
    val join : t -> node -> node -> unit

    (** Poot element of the hierarchy. *)
    val root : t -> node -> node
    
    (** Parent of given element. *)
    val parent : t -> node -> node option  

    (** Simple string representation of the forest. *)
    val toString : t -> string
    
    (** This module provides top-to-down tree view. *)
    module Tree :
    sig
      
      (** Type of element of this view of the forest. *)
      type treeEl = Node of node 
                   | Head of node * (treeEl list)
      
      (** Creates top-to-down view of the forset. *)
      val create : t -> treeEl list

      (** dotty clusterded representation. *)
      val toDOT : treeEl list -> string 
            
      (** Traces forest. *)
      val print : treeEl list -> unit 
       
    end
	
  end 

module Make (G : CFG.Sig )(O : Order.Sig with module G = G): Sig with type node = G.Node.t

           
