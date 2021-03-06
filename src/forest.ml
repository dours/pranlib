(*
 * Forest: forest manipulation functions.
 * Copyright (C) 2008
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

module type Sig = 
  sig

    type t
    
    type node

    val create : unit -> t
     
    val join : t -> node -> node ->unit

    val root : t -> node -> node
    
    val parent : t -> node -> node option 

    val toString : t -> string
    
    (* This module provides top-to-down tree view *)
    module Tree :
    sig
      
      (* Type of element of this view of the forest *)
      type treeEl = Node of node 
                   | Head of node * (treeEl list)
      
      (* Creates top-to-down view of the forset *)
      val create : t -> treeEl list
      
      (* dotty clusterded representation *)
      val toDOT : treeEl list -> string 
      
      (* Traces forest *)
      val print : treeEl list -> unit 
       
    end

  end 

module Make (G : CFG.Sig ) (O : Order.Sig with module G = G) =
  struct 

    type t = (int * int) Urray.t
    
    type node = G.Node.t
   
    let create unit = 
      let nodes = Urray.make 
                  (G.nnodes G.graph ) 
                  (-1,-1) in
      nodes

    let findRoot nodes el_id =
      let rec root el_id rewrite = 
        let (el_parent, el_root) = Urray.get nodes el_id in 
        if el_root = -1 then
          el_id
        else (
          let root = root el_root true in
          if rewrite then Urray.set nodes el_id (el_parent, root);
          root
        ) in
      root el_id false

    let join nodes leaf parent =
      let leaf_id = O.number leaf in 
      let parent_id = O.number parent in
      let parent_root_id = findRoot nodes parent_id in
      let leaf_id = findRoot nodes leaf_id in
      Urray.set nodes leaf_id (parent_id, parent_root_id)

    let root nodes leaf = 
      O.node (findRoot nodes (O.number leaf))

    let parent nodes leaf = 
      let get = Urray.get nodes in
      let leaf_id = O.number leaf in
      let (leaf_parent, leaf_root) = get leaf_id in
      if (leaf_parent = -1) then (
        None
      ) else (
        Some (O.node leaf_parent)
      )

    let toString nodes = 
      let rec nodesList i =
        let (a,b) = (Urray.get nodes i) in
        let b = findRoot nodes i in
        let parent = 
          if a >= 0 then
            O.G.Node.toString (O.node a)
          else 
            "no parent" in
        let root = O.G.Node.toString (O.node b) in
        let this = O.G.Node.toString (O.node i) in
        let s = Printf.sprintf "%s: (parent:%s, root:%s)\n" this parent root in
        if i = (Urray.length nodes) - 1 then
          [s]
        else
          s::(nodesList (i+1)) in
      String.concat "" (nodesList 0)
          
    module Tree = 
    struct
     
       type treeEl = Node of node 
                   | Head of node * (treeEl list)

       module Cluster = 
         struct
           type t = int * G.Node.t list
           let name (i, _) = Printf.sprintf "cluster_%d" i
           let label _ = ""
           let attrs _ = []
           let nodes   = snd
         end

       let clusters roots = 
         let rec tree2dot i roots = 
           let process (els, hds, i) root =
             match root with 
             | Node nd -> nd::els, hds, i
             | Head (nd, loops) -> 
                let c, j = (tree2dot i loops)
                in els, c::hds, j
             in         
           let (els, hds, j) = List.fold_left (process) ([], [], i + 1) roots in
           match hds with 
           | [] -> DOT.Clusters.Leaf (i, els), j
           | _  -> DOT.Clusters.Node ((i, els), hds), j
         in
         fst (tree2dot 0 roots)
 
       module Printer = DOT.ClusteredPrinter 
         (struct
            include G.DOT.Info
            module Cluster = Cluster
          end
         ) 

       let toDOT roots = Printer.toDOT ((G.graph), [clusters roots])
        
       let create nodes =   
         let length = (G.nnodes G.graph) in         
         let levels = Urray.make length [] in
         let rec fold roots i =
           if i = length then
             roots
           else (
             let (p,r) = Urray.get nodes i in
             if p >=0 then (
               let els = Urray.get levels p in
               Urray.set levels p (i::els);
               fold roots (i+1)
             ) else (
               fold (i::roots) (i + 1) 
             )                          
           ) in
         let roots = fold [] 0 in
         let rec visitNode brothers i  =
           let i_childs = Urray.get levels i in
           match i_childs with 
           | [] -> 
             (Node (O.node i))::brothers
           | _  -> 
             (Head (O.node i, List.fold_left (visitNode) [] i_childs))::brothers in
         let roots = List.fold_left (visitNode) [] roots in
         roots
          
       let print roots = 
         let rec toString els i = 
           let visitNode = 
             function 
             | Node n ->
               Printf.printf "  %s%s\n" (String.make i ' ') (G.Node.toString n)
             | Head (n,l) ->  
               Printf.printf " %s+%s\n" (String.make i ' ') (G.Node.toString n);
               toString l (i + 1)
           in 
           List.iter (visitNode) els
         in
         toString roots 0             
     
    end

  end


