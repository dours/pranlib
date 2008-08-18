(*
 * AliasView: Alias analysis implementation.
 * Copyright (C) 2008
 * Leonid Chistov, St.Petersburg State University
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

module Type =
  struct

    type t = Simple | Compound of t array 

  end

module Region =
  struct

    type t  = string * int  

    let name = fst
  
    let id = snd

    let create name id = (name, id)

    let compare x y = (snd x) - (snd y)

    let equal x y = match (x, y ) with
      (name1, id1), (name2, id2) when name1 = name2 && id1 = id2 -> true
    | _ -> false

    let hash = id

  end

module Block =
  struct

    type t   = Simple of Region.t * int |
               Compound of Region.t * int * int * t array (* third parameter should be always greater then second *)

    type rel = Same | Different | Nested | Containing
                     
    let region = function
    | Simple (r, _) | Compound (r, _, _, _) -> r

    let subblocks = function
    | Compound (_, _, _, sub) -> sub  
    | _  -> [||]

    let rec typ = function
    | Compound (_, _, _, sub) -> Type.Compound (Array.map typ sub)
    | Simple _                -> Type.Simple

    let relation x y =
      match (x, y) with
      | Simple    (r1, xi),        Simple    (r2, yi)
      | Compound (r1, xi, _, _),   Compound (r2, yi, _, _) when r1 = r2 -> if xi = yi then Same else Different
     
      | Simple (r1, xi), Compound (r2, yi, z, _) when r1 = r2 -> if xi >= yi && xi <= z then Nested     else Different
      | Compound (r1, yi, z, _), Simple (r2, xi) when r1 = r2  -> if xi >= yi && xi <= z then Containing else Different
 
      | _ -> Different 

   let start = function
   | Simple (_, x) | Compound (_, x, _, _) -> x

   let compare x y =
     let rd = Region.compare (region x) (region y) in
       if rd = 0 then
        let d = (start x) - (start y) in
          if d = 0 then
            match relation x y with
              Nested -> -1
            | Containing -> 1
            | _ -> 0
          else d
       else rd      

   
   let toViewer x = 
   let rec aux = function
   | Simple   (_, i)        -> View.int i
   | Compound (_, _, _, bs) -> View.insqbr (View.array (Array.map aux bs))
   in 
   View.seq [View.string (Region.name (region x)); View.string ":";  (aux x)]

   let toString x = View.toString (toViewer x)

  end

module Expression =
  struct

    type t =
	New    of Region.t * Type.t 
      | Block  of Block.t
      | Sub    of (Block.t -> Block.t option) * t
      | Value  of t
      | Unspec of t 
      | Any

  end

module Statement =
  struct

    type t =
	Assign   of Expression.t * Expression.t
      | Black    of Region.t list * Expression.t list

  end

module BSet = Set.Make (Block)
module BMap = Map.Make (Block)
module RSet = Set.Make (Region)
module RHash = Hashtbl.Make (Region)

type snapshot =
  { all       : BSet.t;
    unwrap    : Region.t -> BSet.t;
    getSimple : Region.t -> BSet.t
  }

module type Snapshot = sig
  (** Memory snapshot type *)
  type t = snapshot

  val s : t
end 

module Memory =
  struct
  
    let maxRegionId = ref 0 

    (* hash table contains set of blocks for given region *)
    let regions = RHash.create 10

    let create name = 
      incr maxRegionId;
      let r = (name, !maxRegionId) in
       RHash.add regions r BSet.empty;
    r

    (*allocFrom sp set t r - allocates block of type t in region r starting from sp in set *)
    let allocFrom startPos set t r  = 
     let set, next = RHash.find regions r, ref startPos
     in
     let rec aux set =
     let aux' (cells, set) t =
      let a = aux set t in
      (BSet.add (fst a) cells, snd a)
     in function
       Type.Simple         -> let cell = Block.Simple (r, !next) in                    
                                incr next;
                                (cell, BSet.add cell set)
     | Type.Compound parts -> let start = !next in                         
                              let (bset, set) = Array.fold_left aux' (BSet.empty, set) parts in                   
                              let cell = Block.Compound (r, start, !next - 1, Array.of_list (BSet.elements bset)) in
                                incr next;
                                (cell, BSet.add cell set)
     in
     let (cell, set) = aux set t
     in
     RHash.replace regions r set;
     cell

    let allocate t r =
     let set = RHash.find regions r in
     let next =
      if BSet.is_empty set
      then 0
      else ((Block.start (BSet.max_elt (set))) + 1) 
     in
     allocFrom next set t r

    (** allocates dynamic r t allocates dynamic block of type t in region r*)
    let allocateDynamic t r = 
     let set = RHash.find regions r in
     let less =
      if BSet.is_empty set
      then 0
      else (Block.start (BSet.min_elt (set))) 
     in
     let rec size = function
     | Type.Simple -> 1
     | Type.Compound parts -> Array.fold_left (fun c t -> c + (size t)) 0 parts
     in
     allocFrom (less - (size t)) set t r

    let dynamic = function
    | Block.Simple (_, i) | Block.Compound (_, i, _, _)
        when i < 0 -> true
    | _            -> false   

    let getMemorySnapshot () = 
      let isSimple = function
      | Block.Simple _ -> true | _ -> false
      in
      let hash = RHash.copy regions in
      let hashSimple = RHash.create 10 in
      RHash.iter (fun k v -> RHash.add hashSimple k (BSet.filter isSimple v)) regions;
      { all = RHash.fold (fun _ -> BSet.union) regions BSet.empty;
        unwrap = (fun r ->  if RHash.mem hash r then RHash.find hash r else BSet.empty);
        getSimple = RHash.find hashSimple 
      } 

    let cleanup () = 
      maxRegionId := 0;
      RHash.clear regions
  end

module NodeInfo =
  struct
    type t = Statement.t list

    let toString x =  
      let rec typ = function
        | Type.Simple -> "S"
        | Type.Compound bs -> "C[" ^ (String.concat ";"  (Array.to_list (Array.map typ bs)) ) ^ "]"
      in
      let block = Block.toString
      in
      let rec expr = function
        | Expression.New (r, t) -> "New (" ^ (Region.name r) ^ (typ t) ^ ")"
        | Expression.Block b    -> "Block (" ^ (block b) ^ ")"
        | Expression.Sub (f, e) -> "Sub (" ^ (expr e) ^ ")"
        | Expression.Value e    -> "Value (" ^ (expr e) ^ ")"
        | Expression.Unspec e   -> "Unspec (" ^ (expr e) ^ ")"
        | Expression.Any        -> "Any"
      in
      let stmt = function
        | Statement.Assign (e1, e2) -> (expr e1) ^ " = " ^ (expr e2)
        | Statement.Black  (rs, es) -> "Black (" ^
                             (String.concat "," ((List.map Region.name rs) @ (List.map expr es))) ^
                             ")"
      in
      String.concat "; " (List.map stmt x)
  end

module NodeInfo' = 
  struct
    type t = Start of NodeInfo.t | General of NodeInfo.t

    let toString = function
    | Start x | General x -> NodeInfo.toString x

  end

module EdgeInfo = 
  struct
    type t = Empty

    let toString _ = ""
  end

module Repr =
  struct
    type node = NodeInfo'.t
    type edge = EdgeInfo.t
  end

module Value (S: Snapshot) =
  struct

    (* V (s, r, u) stands for: set of blocks s + increasing list of regions r + undefined flag *)   
    type t = V of BSet.t * RSet.t * bool | Any

    module S = S

    let mem = S.s

    let empty = V (BSet.empty, RSet.empty, false)   
 
    let isEmpty = function
    | Any -> false
    | V (bs, rs, undf) -> (BSet.is_empty bs) && (RSet.is_empty rs) && (not undf)

    let (<@>) x y = match (x, y) with 
      _, Any | Any, _ -> Any
    | V (bs1, rs1, undf1), V (bs2 ,rs2, undf2) ->               
      let filter rs = BSet.filter (fun x -> not (RSet.mem (Block.region x) rs)) in
      V (BSet.union (filter rs2 bs1) (filter rs1 bs2), RSet.union rs1 rs2, undf1 || undf2)

    let (</>) x y = 
     let diff x y = match (x, y) with
     | V (bs1, rs1, undf1), V (bs2 ,rs2, undf2) ->
       let rs1' = RSet.diff rs1 rs2 in
       let (rs1Bad, rs1Good) = RSet.partition (fun r -> BSet.exists
                                                        (fun b -> Region.compare r (Block.region b) = 0)
                                                         bs2
                                             ) rs1'
       in
       let bs1' = BSet.filter (fun b -> not (RSet.mem (Block.region b) rs2)) bs1
       in
       let bs1'' = BSet.union bs1'
                             (RSet.fold (fun r -> BSet.union (mem.unwrap r)) rs1Bad BSet.empty)
       in
       V (BSet.diff bs1'' bs2, rs1Good, undf1 & (not undf2))
     in
     match (x, y) with
       _, Any -> empty
     | Any, v -> diff (V (S.s.all, RSet.empty, true)) v
     | v1, v2 -> diff v1 v2

    let toViewer = function  
    | Any -> View.string "Any"
    | V (bs, rs, undf) -> View.seq
                          [ View.string "Blocks: ";
                            View.list (List.map Block.toViewer (BSet.elements bs));
                            View.string " Regions: ";
			    View.list (List.map (fun r -> View.string (Region.name r)) (RSet.elements rs));
                            View.string " Undefined: ";
			    View.bool undf
                           ]

    let equal x y =
    match (x, y) with
      Any, Any -> true
    | V (s1, r1, b1), V (s2, r2, b2) when b1 = b2 &&
                                          BSet.equal s1 s2 &&
                                          RSet.equal r1 r2 -> true
    | _, _ -> false

    let unwrap = function
    | V (bs, rs, undf) -> (RSet.fold (fun r -> BSet.union (mem.unwrap r)) rs bs, undf ) 
    | Any -> (mem.all, true)

    let addBlock block = function
    | V (bs, rs, undf) -> V (BSet.add block bs, rs, undf)
    | Any -> Any

    let toString x = View.toString (toViewer x)
                                
  end

(* representation of block value *)
module type AAValue = 
  sig
    (* value type *)
    type t = V of BSet.t * RSet.t * bool | Any

    module S : Snapshot

    (* empty value *)
    val empty : t

    (** checks if value is empty *)
    val isEmpty : t -> bool

    (* unions two values *)
    val (<@>) : t -> t -> t

    (* counts difference of two values *) 
    val (</>) : t -> t -> t

    (* compares two values *)
    val equal : t -> t -> bool

    (* converts inner represnetation of block value to the list of blocks + undefined flag *)
    val unwrap : t -> BSet.t * bool

    (* adds block to value *)
    val addBlock : Block.t -> t -> t

    (* converts value to viewer *)
    val toViewer : t -> View.viewer

    (* string representation of value *)    
    val toString : t -> string

  end

module ASL (V: AAValue) = 
  struct

    module Value = V

    (* TO DO - ADD INCORRECTNESS *)
    type fail = No | May | Must 
 
    type t = L of ((Value.t BMap.t) * Region.t) list | Bottom 
 
    let top = L []

    let bottom = Bottom

    let (<@>) = Value.(<@>)

    let toString  = 
    let mapToList m =
      BMap.fold (fun block value pairs  -> (block, value) :: pairs ) m []
    in
    let blockValue (block, value) =
      View.seq [Block.toViewer block; View.string " = "; Value.toViewer value]
    in
    let aux l = 
     let region (map, r) = 
       View.seq
       [View.break;
        View.space;
        View.listBy (View.seq [View.break; View.space]) (List.map blockValue (mapToList map))
       ]
     in View.listBy View.break (List.map region l)
    in function
    | Bottom -> "Bottom"
    | L l    -> View.toString (aux l)

    let cap x y = 
     let rec cap' x y = match (x, y) with
       [], l | l, [] -> l
     | (m1, r1)::tl1, (m2, r2)::tl2 -> 
       if Region.compare r1 r2 = 0 then
         let f k v m' = 
           try
             BMap.add k (v <@> (BMap.find k m')) m'
           with Not_found -> BMap.add k v m'
         in (BMap.fold f m1 m2, r1)::(cap' tl1 tl2)
       else
       if Region.compare r1 r2 < 0
       then (m2, r2)::(cap' x tl2)
       else (m1, r1)::(cap' y tl1)
     in match (x, y) with
     | _, Bottom | Bottom, _ -> Bottom
     | L x, L y -> L (cap' x y) 

    let equal x y = 
     let rec equal' x y = match (x, y) with
     | [], []        -> true 
     | [], l | l, [] -> false
     | (m1, r1)::tl1, (m2, r2)::tl2 -> 
       if Region.compare r1 r2 = 0 then 
         (BMap.equal Value.equal m1 m2) && (equal' tl1 tl2)
       else false
     in match (x, y) with
       Bottom, Bottom -> true
     | Bottom, _ | _, Bottom -> false
     | L x, L y -> equal' x y

   let getValue block = 
     let region = Block.region block
     in
     let rec findr = function
     | [] -> Value.empty 
     | (m, r)::tl ->
       let d = Region.compare r region  
       in
       if d = 0 then
         try BMap.find block m 
         with Not_found -> Value.empty
       else
       if d < 0
         then findr tl
         else Value.empty
     in function
     | Bottom -> Value.Any
     | L l'   -> findr l'

  let putValue block value = 
    let reg = Block.region block
    in
    let rec aux = function
    | (m, r) :: tl when Region.compare reg r > 0 -> (m, r) :: (aux tl)
    | (m, r) :: tl when Region.compare reg r = 0 -> (BMap.add block value m,  r) :: tl
    | tl -> (BMap.add block value BMap.empty, reg) :: tl
    in function
    | Bottom -> Bottom
    | L l    -> L (aux l)
    
  end

module type AASemilattice = 
  sig
    include Semilattice.Base

    module Value : AAValue
 
   (* computes value of block for given semilattice element *)
    val getValue : Block.t -> t -> Value.t

   (* putValue b v l lets block b have value equal to b *)
    val putValue : Block.t -> Value.t -> t -> t
  end


module AAAdapter (SL : AASemilattice) = 
  struct
    module P = Repr

    module L = Semilattice.Make (SL)

    module E = Expression
    module S = Statement 

    module Value = SL.Value

    let mem = Value.S.s

    let (<@>) = Value.(<@>)
    let (</>) = Value.(</>)

    let flow' =   
      let expr l = 
        let addSub sub block set = match sub block with
          Some b -> (match (Block.relation b block) with
                       Block.Same | Block.Nested -> BSet.add b set
                     | _ -> set
                    )
        | None -> set
        in
        let addVal block value = match block with
        | Block.Compound _ -> value
        | simpl -> (SL.getValue simpl l) <@> value
        in
        let rec expr' = function
        | E.New (r, typ)   -> failwith "Internal: Failure in flow' at E.New"
        | E.Block b -> Value.V (BSet.singleton b, RSet.empty, false)
        | E.Sub (sub, e) -> let addSub' = addSub sub in
                            (match expr' e with
                               Value.V (bs, rs, undf) ->
                                 Value.V                                        
                                 ( BSet.union (BSet.fold addSub' bs BSet.empty)
                                              (RSet.fold (fun r ->  BSet.fold addSub' (mem.unwrap r)) rs BSet.empty),
                                   RSet.empty,
                                   undf  
                                 )
                             | Value.Any -> 
                                 Value.V (BSet.fold addSub' mem.all BSet.empty, RSet.empty, true)
                            )
        | E.Value e      -> (match expr' e with
                               Value.V (bs, rs, undf) -> 
                                 BSet.fold addVal
                                           bs
                                           (RSet.fold (fun r -> BSet.fold addVal (mem.getSimple r))
                                                      rs
                                                      (Value.V (BSet.empty, RSet.empty, undf))
                                           )
                             | Value.Any -> BSet.fold addVal mem.all Value.empty
                            )
        | E.Unspec e     -> (match expr' e with
                               Value.V (bs, rs, _) ->
                               Value.V 
                               ( BSet.empty,
                                 BSet.fold (fun block -> RSet.add (Block.region block)) bs rs,
                                 true
                               )
                             | Value.Any -> Value.Any
                            )
                              
        | E.Any          -> Value.Any
        in expr'
      in    
      let flw stmt l = 
        let add value = fun block -> SL.putValue block ((SL.getValue block l) <@> value)
        in
        let exprl = expr l
        in
        match stmt with
          S.Assign (e1, e2) ->
            (match (exprl e1), (exprl e2) with
               Value.V (bs, rs, _), value
                 when BSet.cardinal bs = 1 && RSet.is_empty rs && not (Memory.dynamic (BSet.choose bs)) ->  
                   SL.putValue (BSet.choose bs) value l 
             | Value.V (bs, rs, _), value  -> 
                 BSet.fold (add value) bs (RSet.fold (fun r -> BSet.fold (add value) (mem.getSimple r)) rs l)                
             | Value.Any, value -> BSet.fold (add value) mem.all l
            ) 
        | S.Black (rs, es) ->  
            let value = (List.fold_left (<@>) Value.empty (List.map exprl es)) <@>
                         (Value.V (BSet.empty, List.fold_right RSet.add rs RSet.empty, false))
            in
            let rec closure value news =
             if Value.isEmpty news
             then value 
             else
               let value' = value <@> news in
                let tryAdd block news' = match block with
                | Block.Simple _   ->  (SL.getValue block l) <@> news' 
                | Block.Compound _ -> let subs = Array.to_list (Block.subblocks block) in
                                       let subv = List.fold_right Value.addBlock subs Value.empty in
                                        subv <@> news'
               in
               let news' =  BSet.fold tryAdd (fst (Value.unwrap news)) Value.empty
               in
               closure value' (news' </> value')
            in
            let clos = closure Value.empty value in
              SL.cap (BSet.fold (fun block -> SL.putValue block clos) (fst (Value.unwrap clos)) SL.top) l
      in    
      let rec aux stmts l = match stmts with
        []     -> l
      | hd::tl -> aux tl (flw hd l)
      in aux

   let flow node l = match node with
     NodeInfo'.Start x   -> let undef = Value.V (BSet.empty, RSet.empty, true) in
                              BSet.fold (function
                                         | Block.Simple _ as block -> SL.putValue block undef
                                         | _ -> (fun x -> x))
                                        mem.all
                                        SL.top
   | NodeInfo'.General x ->  flow' x l 
    
    let init _ = L.top
  end


module Results (A : ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t )
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge) =
struct
 
  type may  = BSet.t * bool

  module S = 
    struct
      type t = snapshot
      
      let s = Memory.getMemorySnapshot ()
    end

  module V = Value (S)

  module SL = ASL (V)

  (* Abstractor wrapper *)
  module A' = 
  struct
    module Concrete = A.Concrete
    
    module Abstract = 
    struct
      type node = Repr.node
      
      type edge = A.Abstract.edge
    end

    module NodeHash = Hashtbl.Make (G.Node)

    let dynHash = 
     let module S = Statement in
     let module E = Expression in
     let hash = NodeHash.create 100
     in
     let rec expr = function
      | E.New (r, t) -> E.Block (Memory.allocateDynamic t r)
      | E.Block b as e -> e
      | E.Sub (sub, e) -> E.Sub (sub, expr e)
      | E.Value e -> E.Value (expr e)
      | E.Unspec e -> E.Unspec (expr e)
      | E.Any -> E.Any
     in   
     let stmt = function
     | S.Assign (e1, e2) -> S.Assign (expr e1, expr e2)
     | S.Black  (rs, es) -> S.Black  (rs, List.map expr es)
     in
     let node x =
      let s = List.map stmt (A.node x) in
       let mark = 
        if x == G.start 
        then NodeInfo'.Start s
        else NodeInfo'.General s       
       in 
       NodeHash.add hash x mark
     in  
     List.iter node (G.nodes G.graph);
    NodeHash.copy hash

    let node = NodeHash.find dynHash  

    let edge = A.edge
  end
  
  module PView = ProgramView.Make (ProgramView.ForwardAdapter (AAAdapter (SL))) (A') (G) 

  module Analyse = DFAEngine.RevPost (PView) (DFST.Make (G))
                
  let alias node block =
    match G.outs node with
      []       -> raise (Failure "Semilattice element cannot be assigned to the end node.")
    | hd :: _  -> V.unwrap (SL.getValue block (Analyse.get hd))

end


