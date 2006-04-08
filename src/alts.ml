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
 * (enclosed in the file LGPL).
 *)

open List

module Make (D: DFST.Sig) = 
	struct

    module T = D
    module G = D.G

    let graph = D.graph
    let start = D.start

    module INS =
      struct

    let incoming = 
      lazy (
            Array.init (G.nnodes graph) 
          (fun i -> 
        if D.isValid i 
        then 
          let n = length (G.ins (D.Post.node i)) in 
          LOG (Printf.printf "%d -> %d\n" i n);
          (n, 0)
        else (0, 0)
          )
      )

    let get i = fst (Lazy.force incoming).(i)
    let dec i = 
      let x, y = (Lazy.force incoming).(i) in
      LOG (Printf.printf "dec %d: (outer=%d, inner=%d)\n" i x y);
      (Lazy.force incoming).(i) <- (x - 1, y + 1)
          
    let inc i = 
      let x, y = (Lazy.force incoming).(i) in
      LOG (Printf.printf "inc %d: (outer=%d, inner=%d)\n" i x y);
      if y > 0 then (Lazy.force incoming).(i) <- (x + 1, y - 1)

      end

    module MA =
      struct

    let all = 
      lazy (
        Array.init (G.nnodes graph)
          (fun i ->
        if D.isValid i
        then
          lazy (
            let node = D.Post.node i in
            let module NS = Set.Make (G.Node) in
            let key       = D.Post.number node in
            let rec forward ((frontier, backdoor, alt) as current) = 
              match frontier with
              | [] -> current
              | v :: frontier' ->
              forward 
                (fold_left 
                   (fun ((frontier, backdoor, alt) as current) e -> 
                 LOG (Printf.printf "Forward: processing edge %s\n" (G.Edge.toString e));
                 let w = G.dst e in
                 let n = D.Post.number w in
                 INS.dec n;
                 if not (NS.mem w alt) 
                 then
                   if (D.Post.number w < key) || (INS.get n) > 0
                   then (frontier, NS.add w backdoor, alt)
                   else (w :: frontier, backdoor, NS.add w alt)
                 else 
                   if (INS.get n) = 0 
                   then (frontier, NS.remove w backdoor, alt)
                   else current
                   ) 
                   (frontier', backdoor, alt)
                   (G.outs v) 
                )
            in
            let rec backward ((backdoor, alt) as current) = 
              if NS.is_empty backdoor 
              then current
              else 
            let v = NS.choose backdoor in
            let f = NS.mem  v alt      in
            
            backward
              (fold_left 
                 (fun ((backdoor, alt) as current) e ->
                   LOG (Printf.printf "Backward: processing edge %s\n" (G.Edge.toString e));
                   let w = G.dst e in
                   if f then INS.inc (T.Post.number w);
                   if (NS.mem w alt) && ((G.Node.compare w node) <> 0) 
                   then (NS.add w backdoor), alt
                   else current
                 ) 
                 (NS.remove v backdoor, if f then NS.remove v alt else alt)
                 (G.outs v) 
              )
            in 
            let _, backdoor, alt = forward ([node], NS.empty, (NS.singleton node)) in
            let _, alt = backward (backdoor, alt) in
            LOG (
              Printf.printf "incomings after MA construction:\n";
              Array.iteri 
                (fun i (n, m) -> Printf.printf "  %d: (outer=%d, inner=%d)\n" i n m) 
                (Lazy.force INS.incoming);
              Printf.printf "end incomings\n"
                    );
            NS.elements alt
          )
        else lazy []
          )
      )

    let get node = Lazy.force (Lazy.force all).(D.Post.number node)

      end

  end

(*    
(* Maximal alts hierarchy building *)
open Treebuilder

module Hierarchy (G: Digraph.Sig) = 
  struct

    module SINGLE = Alt(G)

    (* Alts tree module *)
    module ALT_INFO = struct
        type t = SINGLE.info
        let equal (e1, _) (e2, _) = G.Node.equal e1 e2
        let hash (e, _) = G.Node.hash e
        let toString (e, _) = G.Node.toString e
    end

    module TREE_BUILDER = Treebuilder.Make (ALT_INFO)

    type dfst = Cfa.DFST(G).info
    module DFST = Cfa.DFST(G)
                              
    type info = 
    { 
        dfst: dfst;
        alts: TREE_BUILDER.info_out 
    }

    let foldNumRev fn a last first = 
        let rec aux a' n = 
            if n >= first then aux (fn n a') (n - 1)
            else a'
        in aux a last

    let create dfst =
        (* Root consists of all nodes *)
        let count = G.nnodes dfst.DFST.graph in
        let root = (dfst.DFST.start, G.nodes dfst.DFST.graph) in

        let iterNodes fn = 
            let notBelongs ((_, pinfo) as p) ((_, cinfo) as c) = 
                if for_all (fun nd -> memq nd pinfo) cinfo then
                    (* Tell tree builder about parent and child found *)
                    (fn c p; false)  
                else true
            in
            let build n infos = 
                let info = SINGLE.create dfst (dfst.DFST.Post.node n) in

                (* Find children *)
                info :: (filter (notBelongs info) infos)
            in

            (* Build info and tree for all nodes except for start *)
            let rest = foldNumRev build [] (count - 1) 1 in

            (* Now add root children *)
            iter (fun nd -> fn nd root) rest
        in
            
        let info_in = { TREE_BUILDER.iter = iterNodes; 
                        TREE_BUILDER.root = root; 
                        TREE_BUILDER.size = count } in
        { dfst = dfst; alts = TREE_BUILDER.create info_in }

    open Printf
    open TREE_BUILDER
    let toString info = 
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
        let rec printAlt ((entry, nodes) as root) = 
            let children = info.alts.get_childs root in
            let s, ch_nodes = fold_left (fun (res, ch_nodes) el -> 
                           let s, new_ch_nodes = printAlt el in
                               (res ^ "\n" ^ s, ch_nodes @ new_ch_nodes))
                ("", []) children
            in 
            let rest_nodes = filter (fun nd -> not (memq nd ch_nodes)) nodes in
            (sprintf "subgraph cluster_%d {\n%s\n%s}\n" (G.Node.hash entry)
                 (printList printNode rest_nodes) s, nodes)
        in
        sprintf "digraph G {\n%s%s\n}" (fst (printAlt info.alts.get_root))
            (printList printEdge (G.edges (info.dfst.DFST.graph)))
                                    

    module Dom = Doms.DOM(G)
    module DTB = Dom.INT_TREE_BUILDER

    let dumpDoms dfst doms = 
        let rec dump t = 
            let nd = dfst.DFST.Pre.node t in
            Printf.printf "Node %s dominates over:\n" (G.Node.toString nd);
            iter (fun n -> Printf.printf "%s; " 
                               (G.Node.toString (dfst.DFST.Pre.node n)))
                (doms.DTB.get_childs t);
            print_endline "";
            iter dump (doms.DTB.get_childs t)
        in dump doms.DTB.get_root
                     
    let test info = 
        let dfst = info.dfst in
        let pre = dfst.DFST.Pre.number in
        let doms = Dom.create (dfst.DFST.graph) dfst in
        (*dumpDoms dfst doms;*)

        let rec cmp ((at, _) as ai) dt = 
            (* Compare tree nodes *)
            ((pre at) == dt) && 

            (* Compare children *)
            (let ach = sort (fun (e1, _) (e2, _) -> pre e1 - pre e2) 
                           (info.alts.get_childs ai)
             and dch = sort compare (doms.DTB.get_childs dt) in
             if length ach != length dch then false 
             else fold_left2 (fun res a d -> res && (cmp a d)) true ach dch)
        in
        cmp info.alts.get_root doms.DTB.get_root


end
*)
