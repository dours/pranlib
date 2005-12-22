(*
 * Alts: alts formation algorithms.
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

(* Maximal alt formation algorithm implementation *)
open List

module Alt (G: Digraph.Sig) = struct

    type info = G.Node.t * G.Node.t list
    module DFST = Cfa.DFST(G)
    type dfst = Cfa.DFST(G).info

    module NodeSet = Set.Make(G.Node)

    (* Frontier, back-door and maximal alt sets management module *)
    module Sets = struct
        type t = NodeSet.t * NodeSet.t * NodeSet.t

        let create fr bd ma = (fr, bd, ma)

        module type G_CHOOSE_SET = sig
            val extract : t -> NodeSet.t
            val save : t -> NodeSet.t -> t
        end

        (* Base module for others *)
        module Base (CS: G_CHOOSE_SET) = struct
            let value sets = CS.extract sets
            let add v sets = 
                let set = CS.extract sets in
                    CS.save sets (NodeSet.add v set)
            let get sets =
                let set = CS.extract sets in
                let v = NodeSet.choose set in
                let set' = NodeSet.remove v set in (v, CS.save sets set')
            let rem v sets = 
                let set = CS.extract sets in
                let set' = NodeSet.remove v set in CS.save sets set'
            let isEmpty sets = 
                NodeSet.is_empty (CS.extract sets)
            let isMem v sets = 
                NodeSet.mem v (CS.extract sets)
        end

        (* Frontier, back-door and maximal alts support modules *)
        module Fr = Base (struct 
                              let extract (fr, _, _) = fr
                              let save (fr, bd, ma) fr' = (fr', bd, ma)
                          end)
        module BD = Base (struct 
                              let extract (_, bd, _) = bd
                              let save (fr, bd, ma) bd' = (fr, bd', ma)
                          end)
        module MA = Base (struct 
                              let extract (_, _, ma) = ma
                              let save (fr, bd, ma) ma' = (fr, bd, ma')
                          end)

    end

    (* Build node immediate descendants set *)
    let desc node = fold_left 
        (fun s e -> NodeSet.add (G.dst e) s) NodeSet.empty 
        (G.outs node)
    let printSet set label = Printf.printf "set %s is:\n" label;
        NodeSet.iter (fun n -> Printf.printf "node: %s\n" (G.Node.toString n)) set
    let applyIf cond fn arg = if cond then fn arg else arg 

    type nodeState = { mutable incoming: int }

    open Sets
    let create dfst entry = 

        (* Array with nodes states *)
        let graph = dfst.DFST.graph in
        let states = Array.make (G.nnodes graph) { incoming = 0 } in
        let setState nd st = states.(dfst.DFST.post nd) <- st in
        let getState nd = states.(dfst.DFST.post nd) in
        
        (* Initial values *)
        let sets = Sets.create (NodeSet.singleton entry)    (* frontier *)
                               (NodeSet.empty)              (* backdoor *)
                               (NodeSet.singleton entry)    (* maximal alt *)
        in
        iter (fun n -> setState n { incoming = length (G.ins n) })
            (G.nodes graph);

        (* Part one: visit descendants *)
        let rec partOne sets = 
            if Fr.isEmpty sets then sets
            else begin
                let (v, sets) = Fr.get sets in
                let vNum = dfst.DFST.post v in
                partOne (NodeSet.fold (fun w sets -> 
                    let st = getState w in
                    st.incoming <- st.incoming - 1;

                    if not (MA.isMem w sets) then begin
                        if dfst.DFST.post w > vNum then
                            applyIf (st.incoming > 0) (Sets.BD.add w) 
                                        (Sets.MA.add w (Sets.Fr.add w sets))
                        else sets
                    end
                    else if st.incoming == 0 then BD.rem w sets
                    else sets
                ) (desc v) sets)
            end
        in
        let sets = partOne sets in

        (* Part two: exclude backdoors *)
        let rec partTwo sets = 
            if BD.isEmpty sets then sets 
            else begin
                let (v, sets) = BD.get sets in
                let sets = MA.rem v sets in

                partTwo (NodeSet.fold (fun w sets ->
                    if (MA.isMem w sets) && (w != entry) then 
                        BD.add w sets
                    else sets) (desc v) sets)
            end
        in 
        let sets = partTwo sets in

        (entry, NodeSet.elements (MA.value sets))

end

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
                let info = SINGLE.create dfst (dfst.DFST.post'1 n) in

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
            let nd = dfst.DFST.pre'1 t in
            Printf.printf "Node %s dominates over:\n" (G.Node.toString nd);
            iter (fun n -> Printf.printf "%s; " 
                               (G.Node.toString (dfst.DFST.pre'1 n)))
                (doms.DTB.get_childs t);
            print_endline "";
            iter dump (doms.DTB.get_childs t)
        in dump doms.DTB.get_root
                     
    let test info = 
        let dfst = info.dfst in
        let pre = dfst.DFST.pre in
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
