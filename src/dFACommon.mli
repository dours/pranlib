(*
 * DFACommon: contains common modules for data flow analyses: 
 * reaching definitions and live variables.
 *
 * Copyright (C) 2008
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

(** Represents a variable. *)
module Variable : 
  sig
	
    (** Variable is represented by a name by default. *)
    type t = string
	
    (** [create s] creates a variable named [s]. *)
    val create : string -> t
	
    (** [equals a b] returns true if a and b are the same variable. *)
    val equals : t -> t -> bool
	
    (** [toString s] prints string representation. *)
    val toString : t -> string

  end

(** Represents a point where multiple variables are available. 
    It is used to support alias analysis results.
 *)
module MultiVariablePoint : 
  sig
	
   (** An MVP is a list of variables which may appear at this point 
       (used to support alias analysis. 
    *)
   type t = Variable.t list

   (** [create_empty ()] creates an empty MVP. *)
   val create_empty : unit -> t
	
   (** [toString mvp] prints MVP information. *)
   val toString : t -> string

  end

(** Represents a statement in reaching definitions model. *)
module Statement: 
  sig

    type t = Assignment of (MultiVariablePoint.t * MultiVariablePoint.t list) | Other
	
    val assignment : MultiVariablePoint.t -> MultiVariablePoint.t list -> t
	
    val other : t
	
    val equals : t -> t -> bool
	
    val toString : t -> string

  end

(** Element of a bit-vector. *)
module BitVectorElement : 
  sig

    type t = Variable.t * bool
  
    val construct : Variable.t -> bool -> t
	
    val and_op : t -> t -> t
	
    val or_op : t -> t -> t
	
    val equal : t -> t -> bool
	
    val var : t -> Variable.t
	
    val state : t -> bool
	
    val toString : t -> string 

end

(** Bit-vector. *)
module BitVector : 
  sig

    type t = BitVectorElement.t list
	
    val empty : t
	
    val cap : t -> t -> t
	
    val equal : t -> t -> bool
	
    val lookup_element : Variable.t -> t -> BitVectorElement.t option
	
    val toString : t -> string  

  end

(** DFA helper routines. *)
module DFAHelper:
  sig

    val gen : Statement.t list -> BitVector.t
	
    val kill : Statement.t list -> BitVector.t

  end
