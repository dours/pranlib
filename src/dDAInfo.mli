(*
 * DDA: Data-Dependency Analysis model
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

(** Variable module type *)
module type VariableSig = 
sig
	type t
	
	(* [make s] creates a variable with name s *)
	val make : string -> t
	
	(* [equals v1 v2] returns true if variables v1 and v2 are equal *)
	val equals : t -> t -> bool
	
	val equal : t -> t -> bool
	
	(* [hash v] calculates the hash for the variable *)
	val hash : t -> int
	
	val toString : t -> string
end

module type ValueSig = 
sig
	type t
	
	val make : int -> t
	
	val add : t -> t -> t
	val subtract : t -> t -> t
	val mult : t -> t -> t
	val div : t -> t -> t
	val _mod : t -> t -> t
	val gt : t -> t -> bool
	val lt : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val equal : t -> t -> bool
	
	val get_value : t -> int
	
	val toString : t -> string
end

(** Represents a coefficient in a subscript specification component. E.g. it's 'a1' in 'a1*i1' *)
module type CoefficientSig = 
sig
	module Val : ValueSig
  type t
	
	(* [make_int] creates an integer coefficient *)
	val make_int : int -> t
	
	(* [make_string] creates a named coefficient *)
	val make_string : string -> t
	
	(* [get_value c] returns the value representation of the coefficient in case it exists *)
	val get_value : t -> Val.t option
	
	val toString : t -> string
end

module type IterationVariableValueSig = 
sig
	module V : VariableSig
	module Val : ValueSig
	
	type t
	
	(* [variable ivv] returns the corresponding loop variable *)
	val variable : t -> V.t
	
	(* [value ivv] returns the value of the variable *)
	val value : t -> Val.t
	
	(* [make v val] creates a new iteration variable value *)
	val make : V.t -> Val.t -> t
end

module type CoefficientValueSig = 
sig
	module C : CoefficientSig
	module Val : ValueSig
	
	type t
	
	(* [coefficient cv] returns the corresponding coefficient *)
	val coefficient : t -> C.t
	
	(* [value cv] returns the value of the coefficient *)
	val value : t -> Val.t
	
	(* [make c val] creates a new coefficient value *)
	val make : C.t -> Val.t -> t
end

(** Represents a subscript specification component. E.g. 'a1 * i1' *)
module type SubscriptSpecComponentSig = 
sig
  module V : VariableSig
  module C : CoefficientSig
    
  type t
    
	(* [variable ssc] returns the variable part of ssc or None if the coefficient is not bound to any variable*)
  val variable : t -> V.t option
	
	(* [coefficient ssc] returns the coefficient part of ssc *)
  val coefficient : t -> C.t
	
	(* [make v c] creates a subscript spec component *)
	val make : V.t -> C.t -> t
	
	(* [make_free c] creates a component not bound to any variable (free coefficient) *)
	val make_free : C.t -> t
	
	(* [is_free ssc] returns true in case a component is not bound to any variable *)
	val is_free : t -> bool
	
	val toString : t -> string
end

(** Subscript spectification. Usually represented through a1*i1+a2*i2+...+an*in *)
module type SubscriptSpecSig = 
sig
	module V : VariableSig
  module SSC : SubscriptSpecComponentSig with module V = V
  
  type t
    
	(* [components ss] returns the list of ssc's comprising given ss *)
  val components : t -> SSC.t list
	
	(* [make sscs] creates s subscript spec out of component list *)
	val make : SSC.t list -> t
	
  val toString : t -> string
end

module type ReferenceSig =                                                              
sig                                                                                                 
  module V : VariableSig                                                                            
  module SS : SubscriptSpecSig with module V = V                                                                      
	                                                                                                  
  type t                                                                                            
	                                                                                                  
	(* [variable aer] returns array variable which is referenced *)                                   
	val variable : t -> V.t                                                                           
	                                                                                                  
	(* [subscript aer] returns subscript specification for array element reference or nothing if this is a simple variable reference*)                 
	val subscript : t -> SS.t option
	
  (* [make v ss] creates a reference to a variable with specified subscript *)
	val make : V.t -> SS.t option -> t                                                                        
	                                                                                                  
  val toString : t -> string                                                                        
end

(** Statement module signaure *)
module type StatementSig = 
sig
	module V : VariableSig
	module R : ReferenceSig with module V = V
	module Val : ValueSig
	module IVV : IterationVariableValueSig with module V = V and module Val = Val
	
	type t
	
	(* [reads s] returns variable references which are read by given statement *)
	val reads : t -> R.t list
	
	(* [writes s] returns variable reference which is written by given statement *)
	val writes : t -> R.t
	
	(* [eval s] evaluates the statement on given values *)
	val eval : t -> IVV.t list -> Val.t
	
	(* [make w r] creates a new statements which writes w and reads r *)
	val make : R.t -> R.t list -> t
	
	(* [make_evaluatable f] creates a new evaluatable statement *)
	val make_evaluatable : (IVV.t list -> Val.t) -> t 
	
	val toString : t -> string
 end

(** Module type for loops *)
module type LoopSig =
sig
	module V : VariableSig
	module S : StatementSig with module V = V
	module IVV : IterationVariableValueSig with module V = V
	
	type t
	
	(* [iteration_variable l] returns iteration variable for given loop *)
	val iteration_variable : t -> V.t
	
	(* [innerLoops l] returns all inner loops of the l loop *)
	val inner_loops : t -> t option (* TODO *)
	
	(* [statements l] returns all statements of loop l *)
	val statements : t -> S.t list
	
	(* [gather_statements l] returns all statements of loop l on any nesting level *)
	val gather_statements : t -> S.t list
	
	(* [gather_iteration_variables l] returns all iteration variables of loop l on any nesting level *)
	val gather_iteration_variables : t -> V.t list
	
	(* [iteration_statement l] returns the statement which changes iteration variable for given loop *)
	val iteration_statement : t -> S.t
	
	(* [lower_bound l] return loop's lower bound *)
	val lower_bound : t -> IVV.t option
	
	(* [upper_bound l] returns loop's upper bound *)
	val upper_bound : t -> IVV.t option
	
	(* [make_simple iteration_variable iteration_statement lower_bound upper_bound statements] creates a simple loop *)
	val make_simple : V.t -> S.t -> IVV.t option -> IVV.t option -> S.t list -> t
	
	(* [wrap_with_nest loop nesting_level iteration_variable iteration_statement lower_bound upper_bound] wraps a loop with an enclosing loop (e.g. creates a loop nest) *)
	val wrap_with_nest : t -> int -> V.t -> S.t -> IVV.t option -> IVV.t option -> t
	
	val toString : t -> string
end
  
(** Node information module type signature *)
module type NodeInfoSig=
sig
	module V : VariableSig
  module S : StatementSig with module V = V
	module L : LoopSig with module V = V and module S = S
    
  type t
  
	(* [statement ni] returns the statement represented by node or nothing if node represents a loop *)
  val statement : t -> S.t option
	
	(* [loop ni] returns loop represented by node or nothing if node represents a statement *)
	val loop : t -> L.t option
	
	(* [make_loop l] creates a new loop node *)
	val make_loop : L.t -> t
	
	(* [make_statement s] creates a new statement node *)
	val make_statement : S.t -> t
	
	val toString : t -> string
end

(** Edge information module type signature *)
module type EdgeInfoSig=
sig
  type t
	
	val toString : t -> string
end

(* Dependency analysis general info *)
module type Sig = 
sig
	module Variable : VariableSig
	module Value : ValueSig
	module Coefficient : CoefficientSig with module Val = Value
	module SubscriptSpecComponent : SubscriptSpecComponentSig with module V = Variable and module C = Coefficient
	module SubscriptSpec : SubscriptSpecSig with module V=Variable and module SSC = SubscriptSpecComponent
	module Reference : ReferenceSig with module V = Variable and module SS = SubscriptSpec
	module IterationVariableValue : IterationVariableValueSig with module V = Variable and module Val = Value
	module CoefficientValue : CoefficientValueSig with module C = Coefficient and module Val = Value
	module Statement : StatementSig with module V = Variable and module R = Reference and module Val = Value and module IVV = IterationVariableValue
	module Loop : LoopSig with module V = Variable and module S = Statement and module IVV = IterationVariableValue
	module NodeInfo : NodeInfoSig with module V = Variable and module S = Statement and module L = Loop
	module EdgeInfo : EdgeInfoSig
end

(* Test DDA object model implementation *)
module DDAInfoTest : Sig

