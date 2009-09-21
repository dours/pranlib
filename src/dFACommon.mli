(*
 * DFACommon: contains common modules for data flow analyses: reaching definitions and live variables
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
module type Variable=
sig
  type t
  
  val equals : t -> t -> bool
  
  val toString : t -> string
    
  val make : string -> t
end

module type Sig=
sig
  module V : Variable
  
  module Statement:
  sig
      type t = V.t list * V.t list
    
      val makeAssign : V.t -> V.t list -> t
    
      val makeUse : V.t -> t
    
      val makeOther : unit -> t
      
      val contains_variable_in_lp : t -> V.t -> bool
      
      val contains_variable_in_rp : t -> V.t -> bool
        
      val lp_match : t -> t -> bool
    
      val equals : t -> t -> bool
    
      val toString : t -> string
  end

  module NodeInfo:
  sig
    type t = Statement.t list
  
    val toString : t -> string
  end

  module EdgeInfo : 
  sig
    type t = Empty
  
    val toString : t -> string
  end
  
	(** Element of a bit-vector *)
	module BitVectorElement : 
	sig
		type t = V.t * bool
	  
	  val construct : V.t -> bool -> t
		
		val and_op : t -> t -> t
		
		val or_op : t -> t -> t
		
		val equal : t -> t -> bool
		
		val var : t -> V.t
		
		val state : t -> bool
		
		val toString : t -> string 
	end

	(** Bit-vector *)
	module BitVector : 
	sig
		type t = BitVectorElement.t list
		
		val empty : t
		
		val cap : t -> t -> t
		
		val equal : t -> t -> bool
	  
	  val lookup_element : V.t -> t -> BitVectorElement.t option
	  
	  val print_result : V.t list -> string
		
		val toString : t -> string  
	end
    
  (** Element of a bit-vector. *)
  module RBitVectorElement : 
    sig
  
      (** Type of bit vector element. Reaching definitions analysis works on bit 
          vectors consisting of statements.
       *)
      type t = Statement.t * bool
    
      val construct : Statement.t -> bool -> t
  	
      val and_op : t -> t -> t
  	
      val or_op : t -> t -> t
  	
      val equal : t -> t -> bool
  	
      val statement : t -> Statement.t
  	
      val state : t -> bool
  	
      val toString : t -> string 
  
  end
  
  (** Bit-vector. *)
  module RBitVector: 
  sig
  
      type t = RBitVectorElement.t list
  	
      val empty : t
    
      val cap : t -> t -> t
  	
      val equal : t -> t -> bool
  	
      val lookup_element : Statement.t -> t -> RBitVectorElement.t option
      
      val print_result : Statement.t list -> string
  	
      val toString : t -> string  
  end
  
  (** DFA helper routines *)
  module DFAHelper:
  sig
	  val gen : Statement.t list -> BitVector.t -> BitVector.t
	
	  val kill : Statement.t list -> BitVector.t -> BitVector.t
  end
end

module Make(V : Variable) :
sig
  include Sig
end