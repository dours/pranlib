(*
 * Alts: alt finding algorithms.
 * Copyright (C) 2005
 * Sergey Galanov, St.Petersburg State University
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

open List

module Make (D: DFST.Sig) = 
  struct

    module T = D
    module G = D.G
    module P = D.Post

    (* Useful modules *)
    module type Unit = sig end
    module Unit = struct end

    let graph = D.graph
    let start = D.start

    (* Signature for maximal alt access module *)
    module type MA = 
      sig

	val get : G.Node.t -> G.Node.t list

      end

    (* Apply function for a set of numbers *)
    let rec iterNum fn first last = 
        if first > last then () else (fn first; iterNum fn (first + 1) last)
    let rec iterNumRev fn last first = 
        if last < first then () else (fn last; iterNumRev fn (last - 1) first)

    (* Incoming edges counting module *)
    module INS =
      struct

        module type Sig = 
          sig
              val fill : unit -> unit
              val get : int -> int
              val dec : int -> unit
              val inc : int -> unit
          end

        module Make (Unit : Unit) = 
          struct

            (** Data *)
            let incoming = Urray.make (G.nnodes graph) (0, 0)

            (** Fill array with proper values *)
            let fill () =
              Urray.iteri 
                (fun i _ -> Urray.set incoming i (length (G.ins (P.node i)), 0)) 
                    incoming

            (** Get a counter for node *)
            let get i = fst (Urray.get incoming i)

            (** Decrease a node's counter *)
            let dec i = 
              let x, y = Urray.get incoming i in
              LOG (Printf.printf "dec %d: (outer=%d, inner=%d)\n" i x y);
              Urray.set incoming i (x - 1, y + 1)
                  
            (** Increase a node's counter *)
            let inc i = 
              let x, y = Urray.get incoming i in
              LOG (Printf.printf "inc %d: (outer=%d, inner=%d)\n" i x y);
              if y > 0 then Urray.set incoming i (x + 1, y - 1)

          end
      end

    (* Nodes set module (nodes are represented by their post numbers) *)
    module NodeSet =
      struct

        module type Sig = 
          sig
              val empty : unit -> bool
              val mem : int -> bool
              val add : int -> unit
              val remove : int -> unit
              val choose : unit -> int
              val elements : unit -> int list
          end

        module MakeListed (Unit : Unit) = 
          struct

            type entry = 
              { 
                  mutable belongs : bool;
                  mutable next : int;
                  mutable prev : int;
              }
            
            (* Set data *)
            let set = Urray.init (G.nnodes graph) 
                          (fun _ -> { belongs = false; next = -1; prev = -1 })
            let head = ref (-1)
                                                  
            (* Check whether set is empty *)
            let empty () = (!head = -1)

            (* Check whether a node belongs to the set *)
            let mem nd = (Urray.get set nd).belongs

            (* Add a node to the set *)
            let add nd = 
                let e = Urray.get set nd in
                if e.belongs then () 
                else begin
                    e.belongs <- true;
                    let h = !head in
                    if h <> -1 then (Urray.get set h).prev <- nd;
                    e.next <- h;
                    head := nd
                end

            (* Retrieve a node from the set *)
            let choose () = 
                match !head with -1 -> failwith "No elements"
                    | nh -> 
      let e = Urray.get set nh in
      e.belongs <- false;
      head := e.next;
      if e.next != -1 then (Urray.get set (e.next)).prev <- -1;
      e.next <- -1; e.prev <- -1;
      nh

            (* Retrieve a given node from the set *)
            let remove nd = 
                let e = Urray.get set nd in
                if not e.belongs then ()
                else begin
                    e.belongs <- false;
                    let next = e.next and prev = e.prev in
                    if next != -1 then (Urray.get set next).prev <- prev;
                    if prev != -1 then (Urray.get set prev).next <- next;
                    if prev = -1 then head := next;
                    e.next <- -1; e.prev <- -1
                end

            (* Remove all elements from the set and place them into a list *)
            let elements () = 
                let rec aux res e = 
                    if e = -1 then res 
                    else begin 
                        let en = Urray.get set e in
                        en.prev <- -1;
                        en.belongs <- false;
                        let next = en.next in en.next <- -1;
                        aux (e :: res) next
                    end in
                let l = aux [] !head in
                head := -1; l
            
          end

        module MakeBriggs (Unit : Unit) = 
          struct

            (* Set data *)
            let size = G.nnodes graph
            let index = Urray.make size (-1)
            let value = Urray.make size (-1)
            let next = ref 0
                                             
            (* Check whether set is empty *)
            let empty () = (!next = 0)

            (* Check whether a node belongs to the set *)
            let mem nd = (Urray.get index nd) >= 0

            (* Add a node to the set *)
            let add nd = 
                if not (mem nd) then begin
                    let pos = !next in
                    Urray.set index nd pos;
                    Urray.set value pos nd;
                    next := pos + 1
                end

            (* Retrieve a node from the set *)
            let choose () = 
                let pos = !next in
                if pos = 0 then failwith "No elements" 
                else begin
                    let np = pos - 1 in
                    let nd = Urray.get value np in
                    Urray.set index nd (-1);
                    next := np; nd
                end

            (* Retrieve a given node from the set *)
            let remove nd = 
                let i = Urray.get index nd in
                if i >= 0 then begin
                    Urray.set index nd (-1);
                    let np = !next - 1 in
                    if i < np then begin
                        let n = Urray.get value np in
                        Urray.set index n i;
                        Urray.set value i n
                    end;
                    next := np
                end

            (* Remove all elements from the set and place them into a list *)
            let elements () = 
                let rec aux res e = 
                    if e = -1 then res 
                    else begin 
                        let en = Urray.get value e in
                        Urray.set index en (-1);
                        aux (en :: res) (e - 1)
                    end in
                let l = aux [] (!next - 1) in
                next := 0; l
            
          end

        module Make = MakeBriggs
      end

    (* Hooks for maximal alt building module type *)
    module type Hooks = 
      sig
        val filterNode : int -> bool
        val onUnfoldBD : int -> int -> unit
        val descs : int -> int list
      end

    module MakeMA (INS : INS.Sig) (BD : NodeSet.Sig) (MA : NodeSet.Sig) (H: Hooks) =
      struct

        let getNumbers node =
          let n = P.number node in

          (* First collect all nodes reachable from given (filtering those that can't belong to alt) *)
          let rec forward fr = match fr with [] -> () 
              | v::rest ->
                forward (fold_left (fun fr w ->
                     LOG (Printf.printf "Forward: processing node %s\n" (G.Node.toString (P.node w)));
                     INS.dec w;
                     if not (H.filterNode w) then (fr)
                     else if not (MA.mem w)
                     then
                       if (INS.get w) > 0
                       then (BD.add w; MA.add w; w :: fr)
                       else (MA.add w; w :: fr)
                     else 
                       if (INS.get w) = 0 
                       then (BD.remove w; fr)
                       else fr) rest
                   (H.descs v)
                  ) 
          in

          (* Then exclude nodes with outer edges leading to them and all of their descendants *)
          let rec backward () =
            if BD.empty () then ()
            else 
              let v = BD.choose () in
              let f = MA.mem v in
              if f then MA.remove v;
                (iter (fun w ->
                     LOG (Printf.printf "Backward: processing node %s\n" (G.Node.toString (P.node w)));
                     if f then INS.inc w;
                     if (MA.mem w) && (w <> n)
                     then (H.onUnfoldBD v w; BD.add w)
                     else ())
                   (H.descs v)
                ); backward ()
          in 

          (* Execute the algorithm *)
          MA.add n;
          let _ = forward [n] in
          backward ();
(*          LOG (
            Printf.printf "incomings after MA construction:\n";
            Urray.iteri 
              (fun i (n, m) -> Printf.printf "  %d: (outer=%d, inner=%d)\n" i n m) 
              (Lazy.force INS.incoming);
            Printf.printf "end incomings\n"
          );*)
          MA.elements ()

        let get node = map P.node (getNumbers node)

      end

      (* The default hooks *)
      module DefHooks = 
        struct

          let filterNode _ = true
          let onUnfoldBD _ _ = ()
          let descs node = 
              map (fun e -> P.number (G.dst e)) (G.outs (P.node node))
        end

      (* Basic algorithm *)
      module MA = 
        struct

            let all = 
              lazy (
                let module INS = INS.Make (Unit) in
                let module BD = NodeSet.Make (Unit) in
                let module MA = NodeSet.Make (Unit) in
                let module Core = MakeMA (INS) (BD) (MA) (DefHooks) in

                Urray.init (G.nnodes graph)
                  (fun i -> lazy (INS.fill (); Core.get (P.node i)))
              )

            let get node = Lazy.force (Urray.get (Lazy.force all) (P.number node))

        end

    (* Create maximal alt from dominance tree *)
    module MAFromDom (T : Tree.Tree with type t = G.Node.t) =
      struct
	
        let data = lazy (
          let a = Urray.make (G.nnodes graph) [] in
          iterNumRev 
            (fun n -> 
	      let ma = T.children (P.node n) in
	      Urray.set a n 
		(fold_left 
		   (fun cur child -> (Urray.get a (P.number child)) @ cur)
		   [P.node n] 
		   ma
		)
            ) 
            (G.nnodes graph - 1) 0;
          a
         )
	    
        let get node = Urray.get (Lazy.force data) (P.number node)
          
      end

    (* Hierarchy builder *)
    module HIER = 
        struct

          (* Data to associate with each node *)
          type nodeData = { 
              mutable char : int;
              mutable descs : int list;
              mutable parent : int;
          }

          module HT = Hashtbl.Make (G.Node)

          let create =
            lazy (
              (* Initialize data *)
              let count = G.nnodes graph in
              let data = Urray.init count (fun n -> 
                                              {char = n;
                                               descs = DefHooks.descs n;
                                               parent = 0}) in

              (* Initialize nodes characteristics *)

              let getChar nd = (Urray.get data nd).char in
              let setChar nd ch = (Urray.get data nd).char <- ch in

              iterNum 
		(fun pn -> 
		  iter 
                    (fun cn -> 
		      let d = Urray.get data cn in 
                      if d.char > pn then if cn > pn then d.char <- pn
		    ) 
                    (Urray.get data pn).descs
		) 
		0 
		(count - 1);

              (* Initialize resulting hash *)
              let hash = HT.create count in
              iter (fun nd -> HT.add hash nd (None, [])) (G.nodes graph);

              (* Initialize incoming counters and the sets needed by the basic algorithm *)
              let module INS = INS.Make (Unit) in
              INS.fill ();
              let module BD = NodeSet.Make (Unit) in
              let module MA = NodeSet.Make (Unit) in
              let module PartialMA = MakeMA (INS) (BD) (MA) in

              (* Temporary set for graph factorization purposes *)
              let module DS = NodeSet.Make (Unit) in

              (* Build alts for nodes *)
              let rec build n = if n < 0 then hash else begin
                  let node = P.node n in
                  LOG (Printf.printf "building MA for node %d (%s)\n" n (G.Node.toString node));

                  (* Create base alt building module *)
                  let module Hooks = 
                    struct
                      let filterNode nd = getChar nd >= n
                      let onUnfoldBD parent nd = 
                          let ch = getChar parent in
                          if ch < getChar nd then setChar nd ch
                      let descs nd = (Urray.get data nd).descs
                    end
                  in
                  let module SINGLE = PartialMA (Hooks) in

                  (* Build alt *)
                  let maInt = SINGLE.getNumbers node in
                  let ma = map P.node maInt in
                  LOG (Printf.printf "MA for %s is:\n" (G.Node.toString node));
                  LOG (iter (fun nd -> Printf.printf "%s\n" (G.Node.toString nd)) ma);

                  (* Factorize graph (put all nodes in the alt into one node
                     by recalculating the descendants) *)
                  iter (fun v -> iter (fun w -> DS.add w) (Hooks.descs v)) maInt;
                  iter (fun v -> DS.remove v) maInt;
                  (Urray.get data n).descs <- DS.elements ();
                  iter (fun nd -> INS.inc nd) ((Urray.get data n).descs);

                  (* Store information for tree *)
                  iter (fun nd -> if nd != node then 
                            let children = snd (HT.find hash nd) in
                            HT.replace hash nd (Some node, children)) ma;
                  HT.replace hash node (None, filter (fun nd -> nd != node) ma);

                  build (n - 1)
                end
              in build (count - 1)
            )

          (* Dominance tree *)
          module Tree = 
            struct

              type t = G.Node.t

              let root = start
              let hash () = (Lazy.force create)
              let parent nd = fst (HT.find (hash()) nd)
              let children nd = snd (HT.find (hash()) nd)

            end

          module MA = MAFromDom (Tree)

          (* Hierarchy printer *)
          open Printf
          let toDOT () = 
              let printNode nd = 
                  sprintf "node%d [label=\"%s\"];" (G.Node.hash nd) (G.Node.toString nd)
              in
              let printEdge e = 
                  let src = G.src e and dst = G.dst e in
                  sprintf "node%d -> node%d [label=\"%s\"];" 
                      (G.Node.hash src) (G.Node.hash dst) (G.Edge.toString e)
              in
              let printList printElem lst = 
                  fold_left (fun res el -> res ^ "\n" ^ (printElem el)) "" lst
              in
              let rec printAlt node = 
                  let children = Tree.children node in
                  let isLeaf nd = 
                      let ch = Tree.children node in
                      length ch == 0
                  in
                  let leaves, rest = partition isLeaf children in
                  sprintf "subgraph cluster_%d {\n%s\n%s}\n" (G.Node.hash node)
                      (printList printNode (node :: leaves)) 
                      (printList printAlt rest)
              in
              sprintf "digraph G {\n%s%s\n}" (printAlt Tree.root)
                  (printList printEdge (G.edges graph))
                                          
        end

  end
