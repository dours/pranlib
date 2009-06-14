
module type Element = 
  sig

    type t

    val compare  : t -> t -> int
    val toString : t -> string

  end

module Make (X : Element) = 
  struct

    type tree = Empty | Root of X.t * heap
    and  heap = tree list

(* Tree means binomial tree here.*)
   
    let rec merge = function
      | [], [], Empty -> []
      | [], [],     z -> [z]
      | [],  h, Empty 
      |  h, [], Empty -> h
      | [],  h,     z
      |  h, [],     z -> merge ([z], h, Empty)
      |  x,  y,     z -> 
	  let value, forward = join (List.hd x) (List.hd y) z in
	  value :: (merge ((List.tl x), (List.tl y), forward))

    and	join t1 t2 t3 = 
      let join2 t1 t2 =
        match t1, t2 with
        | Empty, Empty -> Empty, Empty
	| Empty, Root _ -> t2, Empty
	| Root (x, h1), Empty -> t1, Empty
	| Root (x, h1), Root (y, h2) -> 
	    Empty, 
	    if X.compare x y < 0 then 
	       Root (x, List.append h1 [ Root (y, h2) ]) else 
	       Root (y, List.append h2 [ Root (x, h1) ]) 
      in
(*      ( snd (join2 t3 value), forward ) BUG, 0b01 + 0b01 = 0b10 *)
      match join2 t1 t2 with
      	| Empty, t -> t3, t
	| t, Empty -> join2 t t3

    let rec fold f x0 = List.fold_left 
	(fun x t -> match t with
	| Empty -> x
	| Root (u, h) -> f (fold f x h) u
	) x0

    let findMin h = 
	match List.fold_left 
	    (fun x t -> match x, t with
		| _, Empty -> x
		| None, Root (y, _) -> Some y
		| Some x, Root (y, _) -> 
		    if X.compare x y < 0 then Some x else Some y
	    ) None h
	with 
	    | None -> raise (Failure "empty heap in findMin")
	    | Some x -> x

    let removeMin = function
      | [] -> raise (Failure "empty heap during removeMin")
      | h ->
	  let u = findMin h in
	  let rec inner (tree :: rest) =
	    match tree with 
	    | Empty -> 
		let a, b, c = inner rest in
		Empty :: a, b, c
	    | Root (v, h1) ->
		if v == u then [Empty], h1, rest
		else
		  let a, b, c = inner rest in
		  tree :: a, b, c 
	  in 
	  let h1, h2, rest = inner h in
	  (List.append (merge (h1, h2, Empty)) rest), u
	  
    let add h x = merge ([], h, (Root (x, [])))

    let empty = []

    let isEmpty h = List.for_all (fun x -> x = Empty) h

    let rec iter f = List.iter 
	(fun t -> match t with 
	| Empty -> ()
	| Root (x, h) -> begin f x; iter f h end
	)
	
    let toString = fold (fun s i -> Printf.sprintf "%s, %s" (X.toString i) s) ""
    
end
