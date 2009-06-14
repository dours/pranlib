(**************************************************************************
 *  Copyright (C) 2005
 *  Oleg Medvedev (dours@mail.ru), St.Petersburg State University
 *  Universitetskii pr., 28, St.Petersburg, 198504, RUSSIA    
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA
 *
 *  See the GNU Lesser General Public License version 2.1 for more details
 *  (enclosed in the file COPYING).
 **************************************************************************)

module type BiGraph =
  sig

    type t

    val nBoys  : t -> int
    val nGirls : t -> int

    val edges : t -> ((int * int) * int) Urray.t
    
  end

open Printf 

module Util =
  struct
    
    let unsome (Some x) = x 
    
    let rec foldAndFind f exhausted accum = 
	if exhausted accum then accum, None else
	match f accum with
	| accum, (Some x) -> accum, (Some x)
	| accum, None -> foldAndFind f exhausted accum
    
    let rec foldAndFindList f accum = function 
    | [] -> accum, None
    | h :: t ->  
	 (match f accum h with
	  | accum, (Some x) -> accum, (Some x)
	  | accum, None -> foldAndFindList f accum t
	 ) 

    let rec foldNum f start = function
    | 0 -> start
    | n -> foldNum f (f start (n - 1)) (n - 1)
	    
    let iterNum f = foldNum (fun _ x -> f x; ()) ()
	    
    let rec forAllNum p = function
    | 0 -> true
    | i -> (p (i - 1)) && (forAllNum p (i - 1))

  end

open Util

module Make (G : BiGraph) =
  struct
  
    type updateWeightsStructure = Add of int | Increment of int 

    let search graph =
      let module Graph =
          struct
 
            let boys, _, pm = Urray.fold_left 
		(fun (boys, index, vector) (((boy, girl), weight) as e) ->
                  Urray.set boys boy (((boy, girl), weight, index) :: (Urray.get boys boy));
                  Urray.set vector index (false, e);
                  (boys, index+1, vector)
		) 
		(
                 Urray.init (G.nBoys graph) (fun _ -> []), 
                 0, 
                 Urray.make (Urray.length (G.edges graph)) (false, ((0, 0), 0))
		) 
		(G.edges graph)
		
            let boyIsFree = Urray.make (G.nBoys graph) true
            let husband = Urray.make (G.nGirls graph) None 
		
            module Edge =
              struct

                type t = ((int * int) * int * int)

                let boy ((x, _), _, _) = x
                let girl ((_, x), _, _) = x

                let weight ((_, _), x, _) = x
                let index  ((_, _), _, x) = x

                let inMatching (_, _, x) = fst (Urray.get pm x)

                let includeIn  (((boy, girl), _, x) as e) = 
		  Urray.set pm x (true, snd (Urray.get pm x));
		  Urray.set boyIsFree boy false;
		  Urray.set husband girl (Some e)
		      
                let excludeOut ((boy, girl), _, x) = 
		  Urray.set pm x (false, snd (Urray.get pm x));
		  Urray.set boyIsFree boy true;
		  Urray.set husband girl None
		      
              end

            module P (S : sig val a : ((int * int) * int * int) list Urray.t end) =
              struct

                type t = int
                let out n = try Urray.get S.a n with _ -> raise (Failure "getting out")

              end

            module Boy  = P (struct let a = boys end)
	    
	    let checkPM () = 

		let useBoyCounter = Urray.make (G.nBoys graph) false in
		let useGirlCounter = Urray.make (G.nGirls graph) false in

		Urray.iter 
		  (fun (inp, ((boy, girl), _)) ->
		    if inp then
		      if (Urray.get useBoyCounter boy || Urray.get useGirlCounter girl) 
		      then raise (Failure "Pairmatching CAPUT")
		    else begin
		      Urray.set useBoyCounter boy true;
		      Urray.set useGirlCounter girl true
		    end
		  ) pm
		  
          end      
      in

      LOG (
        let buf    = Buffer.create 1024 in
        let append = Buffer.add_string buf in

        for i=0 to (G.nBoys graph) - 1 do
            append (sprintf "boy %d {\n" i);            
            List.iter (
                 fun e -> append (sprintf " girl: %d, weight: %d, index: %d\n" 
                            (Graph.Edge.girl e) 
                            (Graph.Edge.weight e)
                            (Graph.Edge.index e)
                          )
               ) 
               (Graph.Boy.out i);
            append "}\n"
        done;

      );

      let u, v = 
          Urray.make (G.nBoys graph) (Urray.fold_left (fun m e -> max m (snd e)) 0 (G.edges graph)), 
          Urray.make (G.nGirls graph) 0 
      in

      LOG (
        fprintf stderr "U {\n";
        Urray.iteri (fprintf stderr " [%d]=%d\n") u;
        fprintf stderr "}\n";

        fprintf stderr "V {\n";
        Urray.iteri (fprintf stderr " [%d]=%d\n") v;
        fprintf stderr "}\n";

        fprintf stderr "boyIsFree {\n"; 
        Urray.iteri (fprintf stderr " [%d]=%b\n") Graph.boyIsFree;
        fprintf stderr "}\n";

        fprintf stderr "V {\n";
        Urray.iteri 
	     (fun i h -> fprintf stderr " [%d]=%d\n" i 
	          (match h with | None -> -1 | Some ((i, _), _, _) -> i)
             ) Graph.husband;
        fprintf stderr "}\n";
      );
      
      let module H = BinomialHeap.Make(struct 
        type t = Graph.Edge.t * int
	let compare (_, x) (_, y) = x - y 
	let toString (((boy, girl), weight, index), slack) = Printf.sprintf "%i -> %i (%i) [%i], slack = %i"
	    boy girl weight index slack
      end)
      in

      let rec bigStage () =

	LOG (fprintf stderr "%s" "bigStage ()\n";);
	
	let h = H.empty in
	 
	if not (forAllNum (fun i -> (not (Urray.get Graph.boyIsFree i)) || ((Urray.get u i) = 0)) (G.nBoys graph))
	then            
	  
          let boyIsUsed, girlIsUsed =
            Urray.make (G.nBoys graph) false,
            Urray.make (G.nGirls graph) false
          in
	  
          let rec searchPath (cu, cv, (shift, h)) boy =
	    
            LOG (fprintf stderr "searchPath %i\n" boy); 
	    
	    if Urray.get boyIsUsed boy then ((cu, cv, (shift, h)), None)
            else begin
	      Urray.set boyIsUsed boy true;
	      
	      let (h, l) = List.fold_left (fun (h, l) (((boy, girl), weight, _) as e) ->
	            if (Urray.get u boy) + (Urray.get v girl) = weight then h, e :: l else 
		    (H.add h (e, (Urray.get u boy) + (Urray.get v girl) - weight + shift), l)) (h, []) (Graph.Boy.out boy)
	      in
	      LOG (fprintf stderr "Heap AFTER ADDITION = %s\n" (H.toString h));
	      foldAndFindList searchPathForEdge ((Add boy) :: cu, cv, (shift, h)) l

	    end
	  and
    	  searchPathForEdge (cu, cv, sh) (((boy, girl), weight, index) as e) = 
		if Urray.get girlIsUsed girl then (cu, cv, sh), None 
		else begin
		  let state = (cu, (Add girl) :: cv, sh) in
		  Urray.set girlIsUsed girl true;
		  if ((Urray.get Graph.husband girl) = None) then state, Some [e] 
		  else if not (Graph.Edge.inMatching e) then 
		    let nextBoy = Graph.Edge.boy (unsome (Urray.get Graph.husband girl)) in
		    match searchPath state nextBoy with
		    | state, None -> state, None
		    | state, (Some l) -> state, (Some (e :: (unsome (Urray.get Graph.husband girl)) :: l))
		  else state, None
		end 
	  in
	  
	  let rec updateMatch = function
            | [lastEdge] -> Graph.Edge.includeIn lastEdge
	    | fromBoy :: fromGirl :: tail -> begin
		Graph.Edge.excludeOut fromGirl;  (* Order is important :) *)
		Graph.Edge.includeIn fromBoy;
		updateMatch tail;
	    end
	    | [] -> raise (Failure "problems in updateMatch")
	  in
	  
	  let rec foundPath ((changeU, changeV, (shift, h)) as state) startBoys = 

            LOG (
	      fprintf stderr "%s" "foundPath ["; List.iter (fun boy -> fprintf stderr "%i; " boy) startBoys; 
	      fprintf stderr "%s" "]\n";
            );

            match foldAndFindList searchPath state startBoys with
	    | (cu, cv, _), (Some l) -> cu, cv, (Some l)
	    | (cu, cv, (shift, h)) as state, None ->
              LOG (
                fprintf stderr "got None, computing min\n";
	        fprintf stderr "updateBoys = %s\n" 
	          (List.fold_left  
	    	      (fun s x -> match x with
		       | Increment x -> sprintf "Increment %i;\n%s " x s
		       | Add x -> sprintf "Add %i;\n%s " x s) ""
	           cu)
              );
	    
	      let rec follow (cu, cv, (shift, h)) = 	    
                let minU = Urray.get u (List.hd startBoys) - shift in

	     	LOG (fprintf stderr "Heap = %s\n" (H.toString h));

		let _, delta = foldAndFind (fun h -> 
		    let (((boy, girl), _, _), slack) = H.findMin h in

		    LOG (fprintf stderr "Another edge is %i -> %i, slack = %i\n" boy girl slack);

		    if (Urray.get boyIsUsed boy) && not (Urray.get girlIsUsed girl)
                    then h, (Some slack) 
                    else (fst (H.removeMin h)), None
		) H.isEmpty h
 
                in

		LOG (fprintf stderr "delta = %s\n Heap = %s\n" (if delta = None then "None " else "Some") (H.toString h));

		if delta = None 
                then cu, cv, None 
                else
		    let delta = min minU ((unsome delta) - shift) in
	    
		    LOG (fprintf stderr "delta = %i\n" delta);
		
		    if delta = 0 
                    then cu, cv, None
		    else 
		      let shift = shift + delta in
		      match
		         foldAndFind 
		            (fun ((cu, cv, (sh, h)) as acc) -> 
			         let h, (e, _) = H.removeMin h in
			         LOG(let ((b, g), _, _) = e in fprintf stderr "going along %i -> %i\n" b g;);
		                searchPathForEdge (cu, cv, (sh, h)) e
		            )
		            (fun (_, _, (shift, h)) -> (H.isEmpty h) ||  ((snd (H.findMin h)) - shift > 0)) 
		            ((Increment (-delta)) :: cu, (Increment delta) :: cv, (shift, h))
		      with
		      | (cu, cv, _), (Some l) -> cu, cv, (Some l)
		      | state , None -> follow state
	     in follow state 
	  in
	  
	  let freeBoys = foldNum (fun l n -> if Urray.get Graph.boyIsFree n then n :: l else l) [] (G.nBoys graph) in
	  
	  match  foundPath ([], [], (0, H.empty)) freeBoys with
          | cu, cv, (Some _) -> 
	     Urray.fill boyIsUsed 0 (Urray.length boyIsUsed) false;
	     Urray.fill girlIsUsed 0 (Urray.length girlIsUsed) false;
	    
	     LOG(
	       fprintf stderr "u = (";
	       Urray.iter (fprintf stderr "%i ") u;
	       fprintf stderr ")\nv = (";
	       Urray.iter (fprintf stderr "%i ") v;
	       fprintf stderr ")\n";	    
	       fprintf stderr "updateBoys = %s\n" 
	          (List.fold_left  
	    	       (fun s x -> match x with
		  	| Increment x -> sprintf "Increment %i;\n%s " x s
			| Add x -> sprintf "Add %i;\n%s " x s) ""
	           cu)
	     );
	    
             ignore (List.fold_left (fun increment command -> match command with 
		| Increment delta -> increment + delta
		| Add boy -> 
		    Urray.set u boy ((Urray.get u boy) + increment);
		    increment
	     ) 0 cu);
	    
	     ignore (List.fold_left (fun increment command -> match command with 
		| Increment delta -> increment + delta
		| Add girl -> 
		    Urray.set v girl (Urray.get v girl + increment);
		    increment
	     ) 0 cv);
	    
	     LOG (
                fprintf stderr "u = (";
	        Urray.iter (fprintf stderr "%i ") u;
	        fprintf stderr ")\nv = (";
	        Urray.iter (fprintf stderr "%i ") v;
	        fprintf stderr ")\n";
	     );
							    
             let _, Some path = foldAndFindList searchPath ([], [], (0, H.empty)) freeBoys in 
	    
             LOG (
  	        fprintf stderr "%s" "path = [";
                List.iter (fun ((boy, girl), _, _) -> fprintf stderr "%i -> %i; " boy girl) path;
                fprintf stderr "%s" "];\n"
             );

	     updateMatch path;

             LOG (
                Urray.iter (fprintf stderr "%b, ") Graph.boyIsFree;
	        fprintf stderr "%s" "\n";
	        Urray.iter (fun (inp, ((b, g), w)) -> fprintf stderr "%b, %i -> %i (%i)\n" inp b g w ) Graph.pm;
	        fprintf stderr "%s" "\n";
	        Urray.iter (fun x -> fprintf stderr "%s" (match x with | None -> "none " | Some _ -> "some ")) Graph.husband
             );

	     bigStage ()
	
	  | _, _, None -> ()	      
      in
     
     bigStage ();

     LOG (
       fprintf stderr "%s" "computed ans\n";
       fprintf stderr "%s" " It is:\n";     

       Urray.iter (fun (inp, ((b, g), w)) -> if inp then fprintf stderr "%i -> %i, w = %i\n" b g w) Graph.pm;

       Graph.checkPM ();
     );
    
     Urray.fold_left (fun ans (inp, e) -> if inp then e :: ans else ans) [] Graph.pm
     
  end

module MakeExhaustiveSearch (G : BiGraph) = 
  struct

    let search graph = 

      let boys = Urray.fold_left 
	  (fun boys ((boy, girl), w) -> Urray.set boys boy ((girl, w) :: (Urray.get boys boy)); boys) 
	  (Urray.make (G.nBoys graph) []) 
	  (G.edges graph)
      in

      let girlIsUsed = Urray.make (G.nGirls graph) false in

      let rec inner boy = 
	if boy = -1 then 0, [] 
        else
          List.fold_left 
	    (fun (max1, ans1) ((girl, w) as e) ->
	      if Urray.get girlIsUsed girl then (max1, ans1) 
	      else begin
		Urray.set girlIsUsed girl true;
		let max2, ans2 = inner (boy - 1) in
		let ans =
		  if max2 + w > max1 then (max2 + w, (Some e) :: ans2) 
		  else max1, ans1
		in

		Urray.set girlIsUsed girl false;

		ans
	      end
	    ) 
	    (let m, a = inner (boy - 1) in m, None :: a) (Urray.get boys boy)
      in
      let check m a = 

	let girlUsed = Urray.make (G.nGirls graph) false in

	if (List.length a) != (G.nBoys graph) then raise (Failure "length !!!") 
	else
	  let sum = List.fold_left 
	      (fun s e -> 
		match e with 
		| None -> s 
		| Some (girl, w) -> 
		    if Urray.get girlUsed girl then raise (Failure "girl already used!!!")
		    else begin
		      Urray.set girlUsed girl true;
		      s + w
		    end
	      ) 0 a
	  in
	  if sum != m then raise (Failure "sum !=m ")
      in
      let m, a = inner ((G.nBoys graph) - 1) in
      check m a;
      m
	
  end

	
