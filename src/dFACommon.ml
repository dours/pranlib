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
end

module type Sig=
sig
	val print_time : unit -> string
	
  module V : Variable
  
  module Statement:
  sig
      type t = V.t list * V.t list
			
			val make : V.t list -> V.t list -> t
    
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

module Make(Var : Variable)=
struct
	let print_time _ = 
		let now = Unix.localtime (Unix.time ()) in
		Printf.sprintf "%s\n" ((string_of_int now.Unix.tm_hour)^":"^(string_of_int now.Unix.tm_min)^":"^(string_of_int now.Unix.tm_sec));
	
  module V = Var
  
  module Statement=
  struct
    type t = V.t list * V.t list
		
		let make lp rp = (rp, lp)
    
    let makeAssign lp rp = ([lp], rp)
    
    let makeUse u = ([], [u])
    
    let makeOther _ = ([], [])
    
    let contains_variable_in_lp s v = match s with 
      | (lp, _) -> List.exists (fun x -> V.equals x v) lp
    
    let contains_variable_in_rp s v = match s with
      | (_, rp) -> List.exists (fun x -> V.equals x v) rp
    
    let lp_match s1 s2 = match (s1, s2) with 
      | ((lp1, _), (lp2, _)) -> 
        let rec do_match list1 list2 = match (list1, list2) with
      | ((hd1::tl1), (hd2::tl2)) -> if not (V.equals hd1 hd2) then false else do_match tl1 tl2
          | _ -> false in
        do_match lp1 lp2
      | _ -> false
    
    let equals s1 s2 =
      let rec compare_variable_lists a b = match (a, b) with
        | ([], []) -> true
        | (hda::tla, hdb::tlb) -> if V.equals hda hdb then compare_variable_lists tla tlb else false
        | _ -> false 
        in  
      match (s1, s2) with
      | ((s1w, s1r), (s2w, s2r)) -> (compare_variable_lists s1w s2w) && (compare_variable_lists s1r s2r)
      | _ -> false

    let toString (s : t) =
      let print_variable_list vars = 
        List.fold_right 
        (fun x y -> (V.toString x)^";"^y)
         vars 
        "" in
      match s with  
      | (reads, writes) -> Printf.sprintf "[Reads=%s; Writes=%s]" (print_variable_list reads) (print_variable_list writes) 
  end
  
  module NodeInfo=
  struct
    type t = Statement.t list
    
    let toString n =
      let print_statement_list statements = 
        List.fold_right (fun x y -> (Statement.toString x)^";"^y) statements "" in 
      Printf.sprintf "[Node=[StatementCount=%s][Statements=%s]" (string_of_int (List.length n)) (print_statement_list n)
  end
  
  module EdgeInfo=
  struct
    type t = Empty
    
    let toString _ = "Empty"
  end
  
	module BitVectorElement = 
	struct
		type t = V.t * bool
		
	  let construct var b = (var, b)
	  
		let and_op x y = 
			match (x, y) with
				| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit && y_bit)
		
		let or_op x y =
			match (x, y) with
				| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit || y_bit)
	
		let equal x y = 
			match (x, y) with
				| ((x_var, _), (y_var, _)) -> V.equals x_var y_var
	
		let var x = match x with 
			| (v, _) -> v
			| _ -> failwith "Invalid argument"
	
		let state x = match x with
			| (_, b) -> b
			| _ -> failwith "Invalid argument"
		
		let toString (v, b) = "E["^(V.toString v)^","^(string_of_bool b)^"]"
	end
	
	module BitVector = 
	struct
		type t = BitVectorElement.t list
		
		let empty = []
		
		let cap bv1 bv2 =
			let rec append_or_cap list1 list2 = 
				match list1 with
					| [] -> list2
					| hd::tl -> if List.exists (fun x -> BitVectorElement.equal x hd) list2 then 
						let rec lookup_and_change elem lst =
							match lst with
								| [] -> []
								| hd1::tl1 ->  if BitVectorElement.equal hd1 elem then (BitVectorElement.and_op hd1 elem) :: tl else hd1::(lookup_and_change elem tl1)
						in
						lookup_and_change hd list2
						else hd::(append_or_cap tl list2)
			in
			append_or_cap bv1 bv2
			
			let equal x y =
				let rec equal' x y = match (x, y) with
					| ([], []) -> true
					| ([], _) -> false
					| (_, []) -> false
					| (hd_x::tl_x, hd_y::tl_y) -> 
						if BitVectorElement.equal hd_x hd_y then equal' tl_x tl_y 
						else false
				in
				equal' x y
	      
	    let rec lookup_element var bit_vector = 
	      let compare v bit_vector_element = V.equals v (BitVectorElement.var bit_vector_element) in
	        match bit_vector with
	          | [] -> None
	          | hd::tl -> if compare var hd then Some(hd) else lookup_element var tl
	          
	          let print_result vl = List.fold_right (fun x y -> (V.toString x)^";"^y) vl "" 
				
			let toString bv = 
				let strRepr = List.map (fun x -> BitVectorElement.toString x) bv in
				"BV["^(List.fold_right (fun x y -> 
					let delimiter = if x == "" || y == "" then "" else ";" in
					x^delimiter^y) strRepr "")^"]"
				
	end
    
    module RBitVectorElement = 
      struct
        type t = Statement.t * bool
      	
        let construct var b = (var, b)
        
      	let and_op x y = 
      		match (x, y) with
      			| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit && y_bit)
      	
      	let or_op x y =
      		match (x, y) with
      			| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit || y_bit)
      
      	let equal x y = 
      		match (x, y) with
              | ((x_var, _), (y_var, _)) -> Statement.equals x_var y_var
      
      	let statement x = match x with 
      		| (v, _) -> v
      		| _ -> failwith "Invalid argument"
      
      	let state x = match x with
      		| (_, b) -> b
      		| _ -> failwith "Invalid argument"
      	
      	let toString (v, b) = "E["^(Statement.toString v)^","^(string_of_bool b)^"]"
      end
      
      module RBitVector= 
      struct
      	type t = RBitVectorElement.t list
      	
      	let empty = []
      	
      	let cap bv1 bv2 =
      		let rec append_or_cap list1 list2 = 
      			match list1 with
      				| [] -> list2
      				| hd::tl -> if List.exists (fun x -> RBitVectorElement.equal x hd) list2 then 
      					let rec lookup_and_change elem lst =
      						match lst with
      							| [] -> []
      							| hd1::tl1 ->  if RBitVectorElement.equal hd1 elem then (RBitVectorElement.and_op hd1 elem) :: tl else hd1::(lookup_and_change elem tl1)
      					in
      					lookup_and_change hd list2
      					else hd::(append_or_cap tl list2)
      		in
      		append_or_cap bv1 bv2
      		
      		let equal x y =
      			let rec equal' x y = match (x, y) with
      				| ([], []) -> true
      				| ([], _) -> false
      				| (_, []) -> false
      				| (hd_x::tl_x, hd_y::tl_y) -> 
      					if RBitVectorElement.equal hd_x hd_y then equal' tl_x tl_y 
      					else false
      			in
      			equal' x y
            
          let rec lookup_element var bit_vector = 
            let compare v bit_vector_element = Statement.equals v (RBitVectorElement.statement bit_vector_element) in
              match bit_vector with
                | [] -> None
                | hd::tl -> if compare var hd then Some(hd) else lookup_element var tl
                
          let print_result sl = List.fold_right (fun x y -> (Statement.toString x)^";"^y) sl "" 
      			
  		let toString bv = 
  			let strRepr = List.map (fun x -> RBitVectorElement.toString x) bv in
  			"BV["^(List.fold_right (fun x y -> 
  				let delimiter = if x == "" || y == "" then "" else ";" in
  				x^delimiter^y) strRepr "")^"]"
      			
      end
  
  module DFAHelper=
	struct
		let gen statements bottom = List.map (fun e ->
	    let v = BitVectorElement.var e in
	    let r = (List.exists (fun s -> Statement.contains_variable_in_rp s v) statements) && not (List.exists (fun s -> Statement.contains_variable_in_lp s v) statements) in 
	    BitVectorElement.construct v r) bottom 
	    
		let kill statements bottom = List.map (fun e ->
	    let v = BitVectorElement.var e in
	    let r = List.exists (fun s -> Statement.contains_variable_in_lp s v) statements in 
	    BitVectorElement.construct v r) bottom 
	end
end