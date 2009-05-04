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
open Printf

module Variable=
struct
	type t = string (* Variable is identified by a string. Not sure how to do better *)
	
	let create name = name
	
	let equals x y = (String.compare x y) == 0
	
	let toString s = s
end

module MultiVariablePoint=
struct
	type t = Variable.t list
	
	let create_empty _ = []
	
	let toString mvp = List.fold_right (fun x y -> (Variable.toString x)^(Variable.toString y)) mvp ""
end

module Statement=
struct
	type t = Assignment of (MultiVariablePoint.t * MultiVariablePoint.t list) | Other
	
	let assignment lp rp = Assignment (lp, rp)
	
	let other = Other
	
	let equals x y = true (* TODO: implement *)
	
	let toString statement =
		(* Prints expression (list of MultiVariablePoints *)
		let rec printExpression expr acc =  
			match expr with
				| [] -> acc
				| hd::tl ->
					let delimiter = if List.length tl > 0 then "+" else "" in 
					printExpression tl (acc^(MultiVariablePoint.toString hd)^delimiter) in
		match statement with
			| Other -> "Other"
			| Assignment (lp, rp) -> Printf.sprintf "Assignment [%s=%s]" (MultiVariablePoint.toString lp) (printExpression rp "")
end

module BitVectorElement = 
struct
	type t = Variable.t * bool
	
  let construct var b = (var, b)
  
	let and_op x y = 
		match (x, y) with
			| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit && y_bit)
	
	let or_op x y =
		match (x, y) with
			| ((x_var, x_bit), (y_var, y_bit)) -> (x_var, x_bit || y_bit)

	let equal x y = 
		match (x, y) with
			| ((x_var, _), (y_var, _)) -> Variable.equals x_var y_var

	let var x = match x with 
		| (v, _) -> v
		| _ -> failwith "Invalid argument"

	let state x = match x with
		| (_, b) -> b
		| _ -> failwith "Invalid argument"
	
	let toString (v, b) = "E["^(Variable.toString v)^","^(string_of_bool b)^"]"
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
      let compare v bit_vector_element = Variable.equals v (BitVectorElement.var bit_vector_element) in
        match bit_vector with
          | [] -> None
          | hd::tl -> if compare var hd then Some(hd) else lookup_element var tl 
			
		let toString bv = 
			let strRepr = List.map (fun x -> BitVectorElement.toString x) bv in
			"BV["^(List.fold_right (fun x y -> 
				let delimiter = if x == "" || y == "" then "" else ";" in
				x^delimiter^y) strRepr "")^"]"
			
end

module DFAHelper=
struct
	let gen (statements:Statement.t list) = 
    let gen_statement statement = match statement with 
      | Statement.Other -> BitVector.empty
      | Statement.Assignment(lp, rp) -> 
      let gen_multi_var mvp = List.map (fun x -> (x, true)) mvp in
      let gen_multi_var_list mvpl = List.map (fun x -> gen_multi_var x) mvpl in
       List.concat (gen_multi_var_list rp) in
    let gen_statements = List.map (fun x -> gen_statement x) statements in
    List.concat gen_statements
    
	let kill statements = List.concat (List.map (fun s ->
    match s with
      | Statement.Other -> BitVector.empty
      | Statement.Assignment (lp, _) -> List.map (fun x -> (x, true)) lp) statements) 
end