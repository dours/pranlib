(*
 * Region: Region Construction.
 * Copyright (C) 2004-2006
 * Shkredov Serjic, St.Petersburg State University
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

(*
 * Source is under development. It is not included in build. 
 *)

open List
open Hashtbl
open Printf

type 'a regularProgram =  
                      Compound of 'a regularProgram* 'a regularProgram
                    | Loop of 'a regularProgram*'a regularProgram
                    | If of 'a regularProgram*'a regularProgram*'a regularProgram*'a regularProgram
                    | Operator of 'a
                    | Condition of 'a regularProgram
                    | Or of 'a regularProgram * 'a regularProgram 
                    | And of 'a regularProgram * 'a regularProgram
                    | Not of 'a regularProgram
                    | BoolTrue
                    | BoolFalse  
                    | TempVariableSet of int
                    | TempVariableReset of int
                    | TempVariableCheck of int
                    | Empty

type condition_edge = TrueEdge 
                    | FalseEdge 
                    | SoleEdge

    (*
type nodeFormat = Simple | Complex
let rec printProgram = function 
  | Compound (c1,c2) -> (
    let (r1, t1), (r2, t1) = printProgram c1, printProgram c2 in
    
  )    
  | Loop (h, b) ->
  | If (c, t, f, e) ->
  | Operator o ->
  | Condition e1 ->
  | Or (e1, e2) ->
  | And (e1, e2) ->
  | Not e1 ->
  | BoolTrue ->
  | BoolFalse -> 
  | TempVariableSet i ->
  | TempVariableReset i ->
  | TempVariableCheck i ->
  | Empty ->

      *)

module type Sig =
sig

   module G_OLD : Digraph.Sig

   module G: Digraph.Sig

   val reduce_all : unit -> G.t

end


module Make (G_OLD : Digraph.Sig) (X : sig val graph : G_OLD.t val start : G_OLD.Node.t end)
       : Sig =
struct            

  exception Error of string 

  module G_OLD = G_OLD
  
  module G = Digraph.Make
      (struct 
       (* work graph node *)
	type t = string regularProgram * int



	let toString =  
          let up spaces = "  "^spaces in
   	  let rec toString spaces = function 
                       | Compound (x1, x2), _ -> 
                         sprintf "%s%s;\n%s%s" 
                           spaces 
                           (toString spaces (x1, 0))
                           spaces 
                           (toString spaces (x2, 0))
                       | If (x1, x2, x3, x4), _ -> 
                         sprintf "%sif (\n%s\n%s) {\n%s\n%s} else {\n%s\n%s}\n%s%s"
                           spaces
                           (toString (up spaces) (x1, 0) )
                           spaces
                           (toString (up spaces) (x2, 0))
                           spaces
                           (toString (up spaces) (x3, 0) )
                           spaces
                           spaces
                           (toString spaces (x4, 0))
                       | Operator x, _ -> spaces ^x
                       | Loop (x, y), _ ->  
                         sprintf "%swhile (\n%s\n%s) {\n%s\n%s}"
                           spaces
                           (toString (up spaces) (x, 0)) 
                           spaces
                           (toString (up spaces) (y, 0))
                           spaces
                       | Empty, _ -> "_"
                       | BoolTrue, _ -> "true"
                       | BoolFalse, _ -> "false"  
                       | Not x1,_ ->
                         sprintf "%snot (\n%s\n%s)"
                           spaces
                           (toString (up spaces) (x1, 0)) 
                           spaces
                       | TempVariableSet n, _ -> sprintf "set(%i)" n
                       | TempVariableReset n, _ -> sprintf "reset(%i)" n
                       | TempVariableCheck n, _ -> sprintf "check(%i)" n
                       | _,_ -> "$" in
          toString ""


       end
      )
      (
       struct 
         (* work graph edge *)
	 type t = condition_edge

	 let toString = function
         | TrueEdge  -> "true"  
         | FalseEdge -> "false"
         | SoleEdge  -> "none"

       end
      )

  type transformation = 
                    (* back edge, in,       optional out,    cost*)
    | LoopWithTemp  of G.Edge.t * G.Node.t*(G.Node.t option)*int
    | LoopWithSplit of G.Edge.t * G.Node.t*(G.Node.t option)*int
               (*head     src      dst *)
    | IfSplit of G.Node.t*G.Node.t*G.Node.t*int
    | IfWithTemp of G.Node.t*G.Node.t*G.Node.t*int
    | IfForceSplit of G.Node.t*G.Node.t*G.Node.t*G.Node.t*int
    | IfForceWithTemp of G.Node.t*G.Node.t*G.Node.t*G.Node.t*int
    | EmptyTransform

  let cost_of_transform = function
    | LoopWithTemp  (_, _, _, c1) -> 10000  
    | LoopWithSplit (_, _, _, c1) -> 10000
    | IfSplit (_,_,_,c1) -> c1
    | IfWithTemp (_,_,_,c1) -> 100000
    | IfForceSplit (_,_,_,_,c1) -> 100000
    | IfForceWithTemp (_,_,_,_,c1) -> 100000
    | EmptyTransform -> 100000000

   let size_of_node node =
       snd (G.Node.info node)

   let info_of_node node =
       fst (G.Node.info node)

   let all_of_node node =
       G.Node.info node

  (* mutable counter is used for temp variables naming *)
  let global_temp_counter = ref 0

  let get_next_temp () = 
    global_temp_counter := !global_temp_counter+1;
    !global_temp_counter 

  let some_node_to_string = function
    | None -> "-"
    | Some node -> G.Node.toString node

  (*return out edges of n1: (n1->n2, n1->some other)*)
  let find_edges n1 n2 = 
    let outs = G.outs n1 in
    match outs with 
    | [] -> None, None
    | hd::tl -> (
      match tl with 
      | [] -> Some hd, None
      | hd2::tl -> 
        if G.Node.equal (G.dst hd) n2 then (
          Some hd, Some hd2
        ) else (
          Some hd2, Some hd
        )
    )
     
  let t_to_string = function  
    | LoopWithTemp  (e, n1, n2, c1) -> 
      sprintf "Loop with temp (%s -> %s) IN:%s OUT:%s COST:%i"  
        (G.Node.toString (G.src e)) 
        (G.Node.toString (G.dst e)) 
        (G.Node.toString n1)
        (some_node_to_string n2) c1
    | LoopWithSplit (e, n1, n2, c1) -> 
      sprintf "Loop with split (%s -> %s) IN:%s OUT:%s COST:%i"  
        (G.Node.toString (G.src e)) 
        (G.Node.toString (G.dst e)) 
        (G.Node.toString n1)
        (some_node_to_string n2) c1
    | IfSplit (cond, s1, d1, c1) -> 
      sprintf "If split (%s) (%s <- %s) COST:%i"  
        (G.Node.toString cond)
        (G.Node.toString d1)
        (G.Node.toString s1) c1
    | IfWithTemp (cond, s1, d1, c1) -> 
      sprintf "If temp (%s) (%s <- %s) COST:%i"  
        (G.Node.toString cond)
        (G.Node.toString d1)
        (G.Node.toString s1) c1
    | IfForceSplit (s1, d1, s2, d2, c1) -> 
      sprintf "If force split (%s <- %s) (%s <- %s) COST:%i"  
        (G.Node.toString d1)
        (G.Node.toString s1)
        (G.Node.toString d2)
        (G.Node.toString s2) c1
    | IfForceWithTemp (s1, d1, s2, d2, c1) -> 
      sprintf "If force temp (%s <- %s) (%s <- %s) COST:%i"  
        (G.Node.toString d1)
        (G.Node.toString s1)
        (G.Node.toString d2)
        (G.Node.toString s2) c1
    | EmptyTransform -> "None"

  (* compares two transformation and returns one with min cost *)
  let choose t1 t2 = 
    printf "COMPARE\n\t%s\n\t%s\n" (t_to_string t1) (t_to_string t2);
    if (cost_of_transform t1) < (cost_of_transform t2) then t1 else t2

  let trace_list tostr l = 
    List.iter (fun x -> printf "<%s>," (tostr x)) l;
    printf "\n"

  let trace_list_nodes = trace_list (G.Node.toString) 
 
  module NODE_HASH = Hashtbl.Make(G.Node)

  module OLD_NODE_HASH = Hashtbl.Make(G_OLD.Node)

  (* Node info constructors *)
  let create_if (cond, w1) (tr, w2) (fa, w3) (finish, w4) = 
    If (cond, tr, fa, finish), w1+w2+w3+w4

  let create_comp (st1, w1) (st2, w2) = 
    Compound (st1, st2), w1+w2    

  let create_loop (st1, w1) (st2, w2) = 
    Loop (st1, st2), w1+w2    

  let create_not (st1, w1) = 
    Not st1, w1    


  (* Convert graph to graph with nodes marked with regularPrograms *)

  let mark_graph g start= 
    let g_new = G.create () in
    let old_to_new_nodes = OLD_NODE_HASH.create 100 in
    let g_new = fold_left 
      (fun g_new node -> 
         let g_new, insertedNode = 
           G.insertNode g_new (Operator (G_OLD.Node.toString node), 1) in
         OLD_NODE_HASH.add old_to_new_nodes node insertedNode;
         g_new
       ) g_new (G_OLD.nodes g) in 
    let g_new = fold_left      
      (fun g_new edge ->          
         let g_new, insertedEdge = 
           let src = OLD_NODE_HASH.find old_to_new_nodes (G_OLD.src edge) in
           let dst = OLD_NODE_HASH.find old_to_new_nodes (G_OLD.dst edge) in           
           match List.length (G_OLD.outs (G_OLD.src edge)) with
           | 1 -> 
             G.insertEdge g_new src dst SoleEdge
           | 2 -> (
             match List.length (G.outs src) with
             | 0 -> 
               G.insertEdge g_new src dst TrueEdge
             | 1 ->
               G.insertEdge g_new src dst FalseEdge
           )
           in
         g_new
       ) g_new (G_OLD.edges g) in
    g_new, OLD_NODE_HASH.find old_to_new_nodes start

  let change_edge_dst g edge node_to =
    let edge_mark = G.Edge.info edge in
    let src = G.src edge in
    let g = G.deleteEdge g edge in
    let g,e = G.insertEdge g src node_to edge_mark in
    printf "%s\n" (G.Node.toString node_to);
    g

  let change_edge_src g edge node_from =
    let edge_mark = G.Edge.info edge in
    let dst = G.dst edge in
    let g = G.deleteEdge g edge in
    let g,e = G.insertEdge g node_from dst edge_mark in
    g

  let convert_edge_to_loop g edge node = 
    let edge_mark = G.Edge.info edge in
    let g = G.deleteEdge g edge in
    let g,e = G.insertEdge g node node edge_mark in
    g
    
  let weight_of_node node = 
    let (_,i) = G.Node.info node in 
    i


  (* this transformation processes linear components of a graph and 
     transforms them into single node. e: |out(e.src)| = |in(e.dst)| = 1 *)
  let reduce_linear g start=
    let processed_nodes = NODE_HASH.create 100 in
    let rec reduce_dfst g cur start = 
      if NODE_HASH.mem processed_nodes cur then (
        g, start
      )
      else (
        let process_edge (g, start) el =
          let src = G.src el in
          let dst = G.dst el in
          if List.length (G.outs src) = 1 &&
             List.length (G.ins dst) = 1 &&
             not (G.Node.equal dst src) then (
            let src_elem, s1 = G.Node.info src in
            let dst_elem, s2 = G.Node.info dst in
            let g, new_node = G.insertNode g (Compound (src_elem, dst_elem), s1+s2) in
            let start = if G.Node.equal src start or G.Node.equal dst start then new_node else start in
            let move_in g edge =
              if (G.Node.equal (G.src edge) dst) then (
                convert_edge_to_loop g edge new_node
              ) else
                change_edge_dst g edge new_node in 
            let move_out g edge =
              if (G.Node.equal (G.dst edge) src) then
                convert_edge_to_loop g edge new_node
              else
              change_edge_src g edge new_node in 
            let g = fold_left move_in g (G.ins src) in
            let g = fold_left move_out g (G.outs dst) in 

            let g = G.deleteNode g src in 
            let g = G.deleteNode g dst in 
            reduce_dfst g new_node start
          ) else (
             NODE_HASH.add processed_nodes cur cur;
             reduce_dfst g dst start
          ) in  
        let (g, start) = List.fold_left process_edge (g, start) (G.outs cur) in
        g, start
      ) in
    reduce_dfst g start start
  
  (* this transformation reduces empty loops (e: e.src = e.dst) *)
  let reduce_empty_loops g start = 
    let reduce_edge g edge = 
      let node = G.src edge in
      let g = G.deleteEdge g edge in
      let g, node =
        match G.outs node with
        | [] -> 
          let inf, size =
            G.Node.info node in
          G.replaceNode g node ((Loop (BoolTrue, inf)), size )
        | hd::tl  ->
          let inf, size =
            G.Node.info node in
          match G.Edge.info hd with
          | TrueEdge -> 
            G.replaceNode g node ((Loop (inf, Empty)), size ) 
          | FalseEdge -> 
            G.replaceNode g node ((Loop (Not inf, Empty)), size ) 
          | _ -> raise (Error "wrong edege type in empty loop out.") in
      g in
    let g = List.fold_left reduce_edge g 
      (List.filter 
        (fun edge -> G.Node.equal (G.src edge) (G.dst edge))
        (G.edges g)  ) in
    g, start

  (* this module supports operations additional with DFST 
     Note that operations with graph shouldn't be performed in this module *)
  module TREE_OPERATIONS (GR: sig val graph : G.t val start: G.Node.t end) = 
  struct

    module D = DFST.Make (CFG.Make (G) (struct let graph = GR.graph let start = GR.start end) )

    (* parent in tree relation *)
    let parent_in_tree n1 n2 =
      (D.Pre.number n1 <= D.Pre.number n2) && (D.Post.number n1 <= D.Post.number n2) 

    (* Unsafe operation parent.*)
    let parent node = 
      match D.parent node with 
      | Some p -> p
      | None -> raise (Error "parent not found")

    (* List loop nodes. First node is src of edge. Last is dst of edge. *)
    let list_loop_nodes edge = 
      let rec loop_nodes src dst =
        if G.Node.equal src dst then
          [src]
        else (
          src::(loop_nodes (parent src) dst)
        ) in
      loop_nodes (G.src edge) (G.dst edge)


    (* Search best loop *)    
    let find_best_loop =
      (* process loop of given back edge *)
      let analyse_loop edge = 
        (* list of loop nodes. *)
        let loop_nodes = 
          List.rev (list_loop_nodes edge) in 
        (* determines if node of loop is 'in' node *)
        let is_enter_node node =
          (G.Node.equal (G.dst edge) node) || 
           List.length (G.ins node) > 1 in
        (* calculate loop metrics. (ins with temp),(outs with temp) *)
        let ( (max_fin, sum, max), 
              (l_start, l_out, length, l_max, l_cur)) =  
          let fold_analize ((max_fin, sum, max), 
                            (l_start, l_out, length, l_max, l_cur)) node = 
            (
              if is_enter_node node then (
                if sum >= max then (
                  node, 1, sum
                ) else (
                  max_fin, 1, max 
                )
              )
              else (
                max_fin, sum + 1, max
              )
            ),
            (
              if List.length (G.outs node) > 1 then (
                match l_cur with 
                | Some l_cur -> 
                  if (length >= l_max) then
                    (l_start, Some l_cur, 1, length, Some node)
                  else
                    (l_start, l_out, 1, length, Some node)
                | None -> (l_start, None, 1, 0, Some node) 
              )
              else (
               match l_cur with 
                | Some l_cur -> 
                  (l_start, None, length + 1, l_max, Some l_cur)
                | None ->
                  (l_start + 1, l_out, 0, 0, None)
              )
            ) in
          List.fold_left fold_analize 
            ((G.dst edge, 0, 0),(0, None, 0, 0, None)) loop_nodes in
        (* correct loop metrics... *)
        let l_out = 
          match l_cur with 
          | Some l_cur -> 
            if (length + l_start + 1 > l_max) then (
              Some l_cur
            ) else (
              l_out
            )
          | None ->
            None in
        let max_fin = 
          if sum > max then
            ( G.dst edge )
          else
            ( max_fin ) in
        (* calculate cost of loop with using temp variables for in edges *)
        let cost_any_temp b best_node cond =
          printf "  best = %s\n" (G.Node.toString best_node);
          let next_node nd = 
            if G.Node.equal (G.dst edge) nd then
              G.src edge
            else 
              parent nd in
          let rec cost_any_temp threads sum len sum1 node =           
            if G.Node.equal best_node node then (
              printf "  cost temp fin %s\n" (G.Node.toString node);
              if b then sum + threads*3 else sum1
            ) else (
              if cond node then (
                printf "  cost temp add %s\n" (G.Node.toString node);
                cost_any_temp (threads + 1) (sum + threads) (len + 1) (sum1 + (len + 3)) (next_node node)
              ) else (
                printf "  cost temp same %s\n" (G.Node.toString node);
                cost_any_temp (threads) (sum + threads) (len + 1) (sum1) (next_node node)
              )
            ) in 
          cost_any_temp 0 0 0 0 (next_node best_node) in
        let cost_temp_in = 
          printf "cost temp in %s -> %s\n" (G.Node.toString (G.src edge)) (G.Node.toString (G.dst edge));
          cost_any_temp true max_fin
            (fun node ->
               G.Node.equal (G.dst edge) node || 
                  List.length (G.ins node) > 1) in
        let cost_temp_out =
          printf "cost temp out %s -> %s\n" (G.Node.toString (G.src edge)) (G.Node.toString (G.dst edge));
          match l_out with 
          | None -> 0
          | Some l_out ->
            cost_any_temp false l_out
              (fun node -> List.length (G.outs node) > 1) in          
        (* calculate loop metrics (ins with split) *)
        let max_start, cur_start, sum, max, all =
          let fold_analize (max_start, cur_start, sum, max, all) node = 
            let size = size_of_node node in
            if is_enter_node node then (
              if sum >= max then (
                cur_start, Some node, size, sum, all + size
              ) else (
                max_start, cur_start, size, max, all + size
              )
            )
            else (
              max_start, cur_start, size + sum, max, all + size
            ) in
          List.fold_left fold_analize (None, None, 0, 0, 0) loop_nodes in
        (* correct loop metrics *)
        let cur_start = 
          match cur_start with 
          | Some cur_start -> cur_start
          | None -> raise (Error "unexpected none...") in
        let max_start, cost = 
          match max_start with 
          | Some max_start ->
            if sum > max then
              ( cur_start, all - sum )
            else
              ( max_start, all - max )
          | None -> cur_start, all - sum in 
        let trans1 = LoopWithSplit (edge, max_start, l_out, cost + cost_temp_out) in
        let trans2 = LoopWithTemp  (edge, max_fin,   l_out, cost_temp_in + cost_temp_out) in
        printf "      tr1=%s\n" (t_to_string trans1);
        printf "      tr2=%s\n" (t_to_string trans2);
        (* choose simplest transformation of a loop *)
        choose trans1 trans2 in 
      let fold_back_edges t edge = 
        let t1 = analyse_loop edge in
        choose t1 t in
      let best_loop = 
        List.fold_left fold_back_edges EmptyTransform
        (List.filter (fun edge -> (D.sort edge = DFST.Back)) (G.edges D.graph)) in
      best_loop

    (* Search best condition *)
    let find_best_cond = 
      let common x y =
        let rec common1 x y = 
          if parent_in_tree y x then 
            y
          else (
            common1 x (parent y)) in
        common1 x y in                             
      let analize_cross edge = 
        let src = G.src edge in
        let dst = G.dst edge in
        let common = common src dst in  
        let rec analize_force_cross node = 
          (* Search edge that connects branches of this 'if'... *)
          if (G.Node.equal node common) then
            None
          else (
            let result = 
              let process_cross_edge best_src cr_edge = 
                let analize_abstract cr_src =
                  if parent_in_tree cr_src src then (
                    Some cr_src
                  ) else (
                    None
                  ) in
                let cr_src = analize_abstract (G.src cr_edge) in
                match (best_src, cr_src) with
                | (Some best_src, Some cr_src) -> 
                  if (parent_in_tree cr_src best_src) then Some best_src else Some cr_src
                | (Some best_src, None) -> Some best_src
                | (None, Some cr_src) -> Some cr_src
                | (None, None) -> None in
              List.fold_left process_cross_edge None 
                (List.filter (fun x -> D.sort x = DFST.Cross) (G.ins node)) in            
            match result with 
            | Some x -> 
              Some (x, node)
            | None -> analize_force_cross (parent node)
          ) in
        let analize_force_cross_result = analize_force_cross (parent dst) in
        let common = 
          (* find new if condition (common)... *)
          match analize_force_cross_result with
          | Some (common,_) -> common
          | None -> common in
        (* src and dst branches of if are evaluated.*)
        let src_list = 
          let rec src_list src = 
              if G.Node.equal src common then 
                [src]
              else                 
                (src::(src_list (parent src))) 
            in
          src_list src in
        let src_list = dst::src_list in          
        let dst_list = 
          let rec dst_list dst = 
            match analize_force_cross_result with 
            |  None -> 
              if G.Node.equal dst common then 
                [dst]
              else                 
                (dst::(dst_list (parent dst))) 
            | Some (force_src, force_dst) -> 
              if G.Node.equal dst force_dst then 
                [dst; force_src]
              else                 
                (dst::(dst_list (parent dst))) in
          dst_list dst in
        let cost1, cost2 = 
          let cost_of_branch (in_split, in_all, in_temp, in_branches, out_temp, out_length) node =
            if (G.Node.equal node common) then (
              (in_split, in_all, in_temp, in_branches, out_temp, out_length)
            ) else (
              if G.Node.equal node dst then (
                (in_split, in_all, in_temp, in_branches, out_temp, out_length)
              ) else
              (
                let br_out =
                  (List.length (G.outs node)) > 1 in
                let br_ins =
                  (List.length (G.ins node)) > 1 in
                ((if br_ins then in_all + (weight_of_node node) else in_split), 
                 (in_split + (weight_of_node node)), 
                 (if br_ins then in_branches + 1 + in_temp else in_branches + in_temp),
                 (if br_ins then in_branches + 1 else in_branches),
                 (if br_out then out_temp + out_length + 1 else out_temp), 
                 (out_length + 1) )
              )
            ) in
          let (in_split1, in_all1, in_temp1, in_branches1, out_temp1, out_length1) = 
            List.fold_left cost_of_branch (0,0,0,0,0,0) src_list in
          let (in_split2, in_all2, in_temp2, in_branches2, out_temp2, out_length2) = 
            List.fold_left cost_of_branch (0,0,0,0,0,0) dst_list in
          in_split1 + in_split2 + out_temp1 + out_temp2, 
          in_temp1 + in_temp2 + out_temp1 + out_temp2 in
        let trans1, trans2 = 
          match analize_force_cross_result with
          | Some (src_force, dst_force) ->
            IfForceSplit (src, dst, src_force, dst_force, cost1), 
            IfForceWithTemp (src, dst, src_force, dst_force, cost1) 
          | None ->
            IfSplit (common, src, dst, cost1), 
            IfWithTemp (common, src, dst, cost1) in
        choose trans1 trans2 in 
      let fold_cross_edges t edge = 
        let t1 = analize_cross edge in
        choose t1 t in
      let res = 
        List.fold_left fold_cross_edges EmptyTransform
        (List.filter 
          (fun edge -> (D.sort edge = DFST.Cross) || (D.sort edge = DFST.Forward) ) 
          (G.edges D.graph)) in
      res

  (* dfst based transformations follow *)

    (* transforms loop with split method. *)
    let transform_loop_with_split e in_node out_node =
      let src = G.src e in 
      let dst = G.dst e in
      let next_node nd = 
        match G.Node.equal dst nd with
        | true -> src
        | false -> parent nd in
      let is_loop_edge edge = 
        (G.Edge.equal e edge) or
        (D.sort edge = DFST.Tree 
           && parent_in_tree dst (G.src edge)
           && parent_in_tree (G.dst edge) src) in
      (* applies split transformation *)    
      let g = GR.graph in
      let split e in_node = 
        let split_way, new_nodes =
          let rec split_way nd = 
            if G.Node.equal nd in_node then (
              [], []
            ) else (
              match split_way (next_node nd) with
              | [], new_nodes ->
                if (List.length (G.ins nd) > 1) then (
                  [nd], new_nodes
                ) else (
                  [], nd::new_nodes
                )
              | hd::tl, new_nodes -> nd :: (hd::tl), new_nodes
            ) in
          split_way (next_node in_node) in
        let split_way = List.rev split_way in
        match split_way with
        | [] -> g, new_nodes, out_node
        | hd::tl -> (
          let edge_to_change nd= 
            List.find (fun edge -> G.Node.equal (G.dst edge) nd)
                      (G.outs (next_node nd)) in
          let process_way_node (isFirst, prevNode, g, new_nodes, out_node) nd = 
            let g, new_node = G.insertNode g (G.Node.info nd) in
            let new_out_node = match out_node with 
              | Some out_node -> Some nd
              | None -> None in
            let process_out_edges g edge = 
              if is_loop_edge edge then (
                g 
              ) else (
                fst (G.insertEdge g new_node (G.dst edge) (G.Edge.info edge))
              ) in
            let g = List.fold_left process_out_edges g (G.outs nd) in 
            let edge_to_change = edge_to_change nd in
            let g = 
              if isFirst then (
                change_edge_dst g edge_to_change new_node
              ) else (
                fst (G.insertEdge g prevNode new_node (G.Edge.info edge_to_change) )
              ) in 
            (false, new_node, g, new_node::new_nodes, out_node) in
          let (_, new_node, g, new_nodes, out_node) = 
            List.fold_left process_way_node (true, (next_node hd), g, new_nodes, out_node) split_way in
          let info = (G.Edge.info (edge_to_change in_node)) in
          let g,_ = G.insertEdge g new_node in_node info in
          g, new_nodes, out_node
        )       
        in
      let g, new_nodes, out_node = split e in_node in
      match out_node with
      | None -> 
        let (new_node_mark, new_cost, g) = 
          let process_loop_node (new_node_mark, new_cost, g) node = 
            let mark, cost = G.Node.info node in
            let g = G.deleteNode g node in
            (Compound (new_node_mark,mark)), cost + new_cost, g in
          let in_mark, in_cost = G.Node.info in_node in
          List.fold_left process_loop_node (in_mark, in_cost, g) new_nodes in
        let g,_ = G.replaceNode g in_node (Loop ((Operator "true"), new_node_mark), new_cost) in
        g
      | Some out_node ->
        let new_nodes = in_node:: (List.rev new_nodes) in
        let new_head, new_body, _ = 
          (* constructs two lists of head and body nodes *)
          let process_node (new_head, new_body, is_body) node = 
            if is_body then (
              new_head, node::new_body, true
            ) else (
              if G.Node.equal node out_node then (
                (node::new_head, new_body, true)
              ) else (
                (node::new_head, new_body, false)                
              )                            
            ) in              
          List.fold_left process_node ([], [], false) new_nodes in
        let new_head, new_body = List.rev new_head, List.rev new_body in
        trace_list_nodes new_head;
        trace_list_nodes new_body;
        let loop_cond, out_edge_dst = 
          (* returns condition of loop exit (true or false) *)
          let (_, e_out) =
            match new_body with
            | [] -> 
              find_edges out_node in_node
            | hd::tl -> 
              find_edges out_node hd in
          match e_out with
          | Some e_out -> (
            match G.Edge.info e_out with
            | TrueEdge -> true, (G.dst e_out)
            | FalseEdge -> false, (G.dst e_out) 
            | _ -> raise (Error "eroor in loop. Wrong edge marking.")   
          )
          | None -> raise (Error "error in loop with out..." ) in
        let merge_table = NODE_HASH.create 20 in
        let get_temp node = 
          if NODE_HASH.mem merge_table node then (
            NODE_HASH.find merge_table node 
          ) else (
            let new_temp = get_next_temp () in 
            NODE_HASH.add merge_table node new_temp;
            new_temp
          ) in
        let process_part action_with_last nodes = 
          let rec process_part = function         
            | hd::tl -> (
              match tl with
              | hd2::tl2 -> (
                (*non last node*)
                match find_edges hd hd2 with
                | Some e_loop, Some e_out -> (
                  (*conditional node here*)
                  let to_node = G.dst e_out in
                  let temp_var = get_temp to_node in
                  match G.Edge.info e_loop with
                  | TrueEdge ->
                    create_if (all_of_node hd) 
                              (process_part tl)
                              (Compound (TempVariableSet temp_var, BoolFalse), 1)
                              (Empty, 0)
                  | FalseEdge -> 
                    create_if (all_of_node hd) 
                              (Compound (TempVariableSet temp_var, BoolFalse), 1)
                              (process_part tl)
                              (Empty, 0)
                )
                | Some e_loop, None ->
                  (*non condition node here*)
                  create_comp (all_of_node hd)
                              (process_part tl)
                | _ -> raise (Error "wrong loop analysis")
              )  
              | [] ->
                (* last node here *)
                action_with_last hd
            )            
            | [] -> Empty, 0 in
          process_part nodes in  
        let action_for_header hd = 
          match loop_cond with
          | false ->
            all_of_node hd
          | true-> 
            create_not (all_of_node hd) in 
        let action_for_body hd = 
          match find_edges hd in_node with
          | Some e_loop, Some e_out -> (
            (*conditional node here*)
            let to_node = G.src e_out in
            let temp_var = get_temp to_node in
            match G.Edge.info e_loop with
            | TrueEdge ->
              create_if (all_of_node hd) 
                        (Empty, 0)
                        (Compound (TempVariableSet temp_var, BoolFalse), 1)
                        (Empty, 0)
            | FalseEdge -> 
              create_if (all_of_node hd) 
                        (Compound (TempVariableSet temp_var, BoolFalse), 1)
                        (Empty, 0)
                        (Empty, 0)
          )
          | Some e_loop, None ->
            (*non condition node here*)
            all_of_node hd
          | _ -> raise (Error "wrong loop analysis") in
        let process_header = process_part action_for_header in
        let process_body = process_part action_for_body in
        let info_body = process_body new_body in 
        let temp_list_of_body =
          NODE_HASH.fold (fun k v l -> v::l) merge_table [] in 
        let info_header = process_header new_head in 
        let info_header = 
          match temp_list_of_body with 
          | [] ->
            (* no temp variables were introduced in loop body *)
            info_header
          | hd::tl -> 
            List.fold_left 
              (fun (cnd, m) n -> (Or (TempVariableCheck n, cnd)), n+m)
              (TempVariableCheck hd, 1) tl in           
        let g = List.fold_left 
          (fun g node -> G.deleteNode g node) g (List.tl new_head) in
        let g = List.fold_left 
          (fun g node -> G.deleteNode g node) g new_body in
        let node_to_replace = List.hd new_head in 
        let info_loop = create_loop info_header info_body  in
        let g, header = G.replaceNode g node_to_replace info_loop in 
        let (exit, is_double, g) = 
          let process to_node temp (exit, is_double, g) = 
            if G.Node.equal to_node out_edge_dst then (
              (exit, is_double, g)
            )
            else (
              let g, new_node = G.insertNode g (TempVariableCheck (temp), 1) in
              let g, new_edge = G.insertEdge g new_node to_node (FalseEdge) in
              let g, new_edge = G.insertEdge g exit new_node 
                (if is_double then TrueEdge else SoleEdge) in 
              (new_node, true, g) 
            ) in 
          NODE_HASH.fold (process) merge_table (header, false, g) in
        let g, new_edge = G.insertEdge g exit out_edge_dst (if is_double then FalseEdge else SoleEdge) in
        printf ">>\n%s\n>>\n" (G.Node.toString header);
      g 


    (* transforms simple if with split method *)
    let transform_if_with_split head cross_src cross_dst =
      (* applies split transformation *)    
      let g = GR.graph in
      let dst_branch, src_branch = 
        (* construct branches of original looop *)
        let rec some_branch node = 
          if G.Node.equal node head then 
            [] 
          else
            node::(some_branch (parent node)) in
        (some_branch cross_dst),(some_branch cross_src) in
      let dst_branch = tl dst_branch in 
      let dst_branch = List.rev dst_branch in 
      let src_branch = List.rev src_branch in 
      let rec process_branch branch (last_info, g) = 
        match branch with 
        | [] -> (last_info, g)
        | hd::tl -> (
          let all_info = all_of_node hd in 
          let hd2 = 
            match tl with 
            | [] -> cross_dst 
            | hd2::tl -> hd2 in
          let in_edge, out_edge = find_edges hd hd2 in
          let in_edge = 
            match in_edge with 
            | Some in_edge -> in_edge 
            | None -> raise (Error "if edge not found. corrupted if." ) in              
          let edge_type = G.Edge.info in_edge in
          match last_info with 
          | Some (last_node, last_edge_mark) -> (
            let g, new_node = G.insertNode g all_info in 
            let g, new_edge = G.insertEdge g last_node hd last_edge_mark in 
            match out_edge with 
            | Some out_edge -> 
              let g, _ = G.insertEdge g new_node (G.dst out_edge) (G.Edge.info out_edge) in
              process_branch tl (Some (new_node, edge_type), g)
            | None -> 
              process_branch tl (Some (new_node, SoleEdge), g)
          )
          | None -> (
            if List.length (G.ins hd) > 1 then (
              let g, new_node = G.insertNode g all_info in 
              let parent_in,_ = find_edges (parent hd) hd in 
              let parent_in = 
                match parent_in with
                | Some parent_in -> parent_in 
                | None -> raise (Error "if edge corrupted. ") in              
              let g = change_edge_dst g parent_in new_node in
              let g = 
                match out_edge with 
                | Some out_edge ->
                  let g, _ = G.insertEdge g new_node (G.dst out_edge) (G.Edge.info out_edge) in 
                  g
                | None -> g in
              process_branch tl (Some (new_node, edge_type), g)
            ) else (
              process_branch tl (None, g)
            )
          )
        ) in
      let last_info1, g = process_branch src_branch (None, g) in 
      let last_info2, g = process_branch dst_branch (None, g) in 
      let finish_last_info last_info g =
        match last_info with
        | Some (last_node, last_edge_info) -> 
          let g, _ = 
            G.insertEdge g last_node cross_dst last_edge_info in
          g
        | None -> g in
      let g = finish_last_info last_info1 g in 
      let g = finish_last_info last_info2 g in
      g



    let do_transform = function
    | LoopWithSplit (e, in_node, out_node,_) ->
      transform_loop_with_split e in_node out_node
    | IfSplit (hd, tr, fa, _) ->
      transform_if_with_split hd tr fa
    | EmptyTransform -> 
      raise (Error "Empty transform not executed!")
    | _-> 
      raise (Error "Some not impleemnted transform.")

  end 

  let reduce_all unit =
    printf "shit started\n"; 
    let g, start = mark_graph X.graph X.start in
    let reduce g start = 
      let module TREE = TREE_OPERATIONS( 
        DFST.Make (CFG.Make (G) (struct let graph = g let start = start end))) in
      let tr1 = TREE.find_best_cond in
      let tr2 = TREE.find_best_loop in
      printf "tr1==%s\n" (t_to_string tr1);
      printf "tr2==%s\n" (t_to_string tr2);
      let g = TREE.do_transform tr1 in
      g, start in 
(*
    let g, start = reduce_linear g start in
    let g, start = reduce_empty_loops g start in
  *)
    printf "%s" (G.toDOT g); 
    let g, start = reduce g start in
    printf "%s" (G.toDOT g); 
    g  
end






















 