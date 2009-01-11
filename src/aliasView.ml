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

module Type =
  struct

   type t = 
    | Simple of Region.t
    | Compound of Region.t * t array 

  end

module RSet = Set.Make (Region)
module RMap = Map.Make (Region)

module Block =
  struct

    type t   = Simple of Region.t * int |
               Compound of Region.t * int * t array 

    type rel = Same | Different | Nested | Containing

    let region = function
    | Simple (r, _) | Compound (r, _, _) -> r

    let subblocks = function
    | Compound (_, _, sub) -> sub  
    | _  -> [||]

    let rec typ = function
    | Compound (r, _, sub) -> Type.Compound (r, (Array.map typ sub))
    | Simple   (r, _)      -> Type.Simple r

    let rec relation x y =
      let nested y = Array.fold_left (fun ex x -> ex || let r = relation y x in
                                                          r = Nested || r = Same
                                     ) false
      in
      match (x, y) with
      | Simple    (_, xi),        Simple    (_, yi)
      | Compound (_, xi, _),   Compound (_, yi, _)
        when xi = yi -> Same 

      | x, Compound (_, _, ys) when (nested x ys) -> Nested
      | Compound (_, _, xs), y when (nested y xs) -> Containing

      | _ -> Different 

    let id = function
    | Simple (_, x) | Compound (_, x, _) -> x

    let compare x y = (id y) - (id x)

    let toViewer x = 
      let rec aux = function
      | Simple   (_, i)        -> View.int i
      | Compound (_, _, bs) -> View.insqbr (View.array (Array.map aux bs))
      in 
      View.seq [View.string (Region.name (region x)); View.string ":";  (aux x)]

    let toString x = View.toString (toViewer x)

  end

module BSet = Set.Make (Block)
module BMap = Map.Make (Block)

module Expression =
  struct

    type t =
        New    of Type.t 
      | Block  of Block.t
      | Sub    of (Block.t -> Block.t option) * t
      | Value  of t
      | Region of t 
      | Some   of Region.t
      | Any

    let alloc t   = New t

    let block b   = Block b

    let sub   f e = Sub (f, e)

    let value e   = Value e

    let region e  = Region e

    let some r    = Some r

    let any       = Any

  end

module Statement =
  struct

    type t =
        Assign   of Expression.t * Expression.t
      | Black    of Region.t list * Expression.t list

    let assign e1 e2 = Assign (e1, e2)

    let black  rs es = Black (rs, es)

  end

module Memory =
  struct

    exception RegionNotFound = Not_found

    let regToRegInfo map region  = 
      try
        RMap.find region map
       with Not_found -> raise RegionNotFound

    module RegionInfo =
      struct
        (** set of static blocks * set of dynamic blocks * children regions *)
        type t = BSet.t * BSet.t * Region.t list

        let empty = (BSet.empty, BSet.empty, [])

        let counter = ref 0

        let addEdge (s1, s2, rs) toR = (s1, s2, toR :: rs)

        let rec alloc map mode typ =
          let updateInfo (s, d, rs) block = function
          | `Static  -> (BSet.add block s, d, rs)
          | `Dynamic -> (s, BSet.add block d, rs)
          in
          let addBlock map r block mode =
            let rInfo' = updateInfo (regToRegInfo map r) block mode in
            RMap.add r rInfo' map
          in
          match typ with
            Type.Simple r         -> let block = Block.Simple (r, !counter) in
                                       incr counter;
                                      (addBlock map r block mode, block)
          | Type.Compound (r, ts) ->   let (map', bs) =
                                         Array.fold_left
                                           (fun (map, bs) t -> let (map', block) = alloc map mode t
                                                               in  (map', block :: bs))
                                           (map, [])
                                           ts
                                       in
                                       let block = Block.Compound (r, !counter, Array.of_list (List.rev bs)) in
                                         incr counter;
                                         (addBlock map' r block mode, block)

        let isDynamic (_, dn, _) block = BSet.mem block dn

        let fold f acc = function
        | (st, dn, _) -> BSet.fold f dn (BSet.fold f st acc)

      end

    (** Type of memory : map from regions to region info * number of allocated regions *)
    type t = RegionInfo.t RMap.t * int

    let empty = (RMap.empty, 0)

    let createRegion (map, i) name fathers = 
      let newRegion = Region.create name i
      in
      ( ( RMap.fold (fun k v -> RMap.add k (RegionInfo.addEdge v newRegion))
                    map 
                    (RMap.add newRegion RegionInfo.empty RMap.empty),
          i + 1),
        newRegion
      ) 

    let subregions (map, _) =
      let third (_, _, t) = t
      in
      let rec aux r =
       let rl = third (regToRegInfo map r)
       in
       List.fold_right (fun sr -> RSet.union (aux sr)) rl (RSet.singleton r)
      in
      aux 

    let allocAux mode (map, i) typ =
      let (map, block) = RegionInfo.alloc map mode typ
      in ((map, i), block)

    let allocateBlock =
      allocAux `Static

    let allocateDynamic =
      allocAux `Dynamic

    let fold f (map, _) r acc =
      RegionInfo.fold f acc (regToRegInfo map r) 
 
    let foldAll f (map, _) =
      RMap.fold (fun _ ri acc -> RegionInfo.fold f acc ri) map

    let dynamic (map, _) block = 
      let ri = regToRegInfo map (Block.region block)
      in RegionInfo.isDynamic ri block

  end

module NodeInfo =
  struct
    type t = Statement.t list

    let empty = []

    let toString x =  
      let rec typ = function
        | Type.Simple _ -> "S"
        | Type.Compound (_, bs) -> "C[" ^ (String.concat ";"  (Array.to_list (Array.map typ bs)) ) ^ "]"
      in
      let block = Block.toString
      in
      let rec expr = function
        | Expression.New t      -> "New (" ^ (typ t) ^ ")"
        | Expression.Block b    -> "Block (" ^ (block b) ^ ")"
        | Expression.Sub (f, e) -> "Sub (" ^ (expr e) ^ ")"
        | Expression.Value e    -> "Value (" ^ (expr e) ^ ")"
        | Expression.Region e   -> "Region (" ^ (expr e) ^ ")"
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

module Value (M: sig val memory : Memory.t end) =
  struct

    (* V (s, r, u) stands for: set of blocks s + set of regions r + undefined flag *)   
    type t = V of BSet.t * RSet.t * bool | Any

    module M = M

    let mem = M.memory

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
                             (RSet.fold (Memory.fold BSet.add mem) rs1Bad BSet.empty)
       in
       V (BSet.diff bs1'' bs2, rs1Good, undf1 & (not undf2))
     in
     match (x, y) with
       _, Any -> empty
     | Any, v -> diff (V (Memory.foldAll BSet.add mem BSet.empty, RSet.empty, true)) v
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
    | V (bs, rs, undf) -> (RSet.fold (Memory.fold BSet.add mem) rs bs, undf ) 
    | Any -> (Memory.foldAll BSet.add mem BSet.empty, true)

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

    module M :
      sig
        val memory : Memory.t
      end

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

  end

module ASL (Value: AAValue) = 
  struct

    module Value = Value

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

  let getExprValue e l = 
    let module E = Expression
    in
    let mem = Value.M.memory
    in
    let addSub sub block set = match sub block with
      Some b -> (match (Block.relation b block) with
                   Block.Same | Block.Nested -> BSet.add b set
                 | _ -> set
                )
    | None -> set
    in
    let addVal block value = match block with
    | Block.Compound _ -> value
    | simpl -> (getValue simpl l) <@> value
    in
    let rec expr = function
    | E.New typ      -> failwith "Internal: Failure in getExprWidth at E.New"
    | E.Block b      -> Value.V (BSet.singleton b, RSet.empty, false)
    | E.Sub (sub, e) -> let addSub' = addSub sub in
                        (match expr e with
                          Value.V (bs, rs, undf) ->
                             Value.V                                        
                             ( BSet.union (BSet.fold addSub' bs BSet.empty)
                                          (RSet.fold (Memory.fold addSub' mem) rs BSet.empty),
                               RSet.empty,
                               undf  
                             )
                         | Value.Any -> Value.V (Memory.foldAll addSub' mem BSet.empty, RSet.empty, true)
                        )
    | E.Value e      -> (match expr e with
                           Value.V (bs, rs, undf) -> 
                             BSet.fold addVal
                                       bs
                                       (RSet.fold (Memory.fold addVal mem)
                                                  rs
                                                  (Value.V (BSet.empty, RSet.empty, undf))
                                       )
                         | Value.Any -> Memory.foldAll addVal mem Value.empty 
                        )
    | E.Region e     -> (match expr e with
                           Value.V (bs, rs, _) ->
                           Value.V 
                           ( BSet.empty,
                             BSet.fold (fun block -> RSet.union (Memory.subregions mem (Block.region block))) bs rs,
                             true
                           )
                         | Value.Any -> Value.Any
                        )
    | E.Some r       -> Value.V (BSet.empty, Memory.subregions mem r, false) (** ????? *)
    | E.Any          -> Value.Any
    in expr e

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

   (* computes value of expression for given semilattice element *)
    val getExprValue : Expression.t -> t -> Value.t

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

    let mem = Value.M.memory

    let (<@>) = Value.(<@>)
    let (</>) = Value.(</>)

    let flow' =   
      let expr l e = SL.getExprValue e l
      in    
      let flw stmt l = 
        let add value block l = match block with
        | Block.Compound _ -> l
        | _                -> SL.putValue block ((SL.getValue block l) <@> value) l
        in
        let exprl = expr l
        in
        match stmt with
          S.Assign (e1, e2) ->
            (match (exprl e1), (exprl e2) with
               Value.V (bs, rs, _), value
                 when BSet.cardinal bs = 1 && RSet.is_empty rs && not (Memory.dynamic mem (BSet.choose bs)) ->  
                   SL.putValue (BSet.choose bs) value l 
             | Value.V (bs, rs, _), value  -> 
                 BSet.fold (add value) bs (RSet.fold (Memory.fold (add value) mem) rs l)                
             | Value.Any, value -> Memory.foldAll (add value) mem l
            ) 
        | S.Black (rl, el) ->  
            let rs = List.fold_right (fun r -> RSet.union (Memory.subregions mem r)) rl RSet.empty
            in 
            let value = List.fold_left (<@>) (Value.V (BSet.empty, rs, false))
                                             (List.map exprl el)
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
                             flow' x (Memory.foldAll (function | Block.Simple _ as block -> SL.putValue block undef
                                                               | _ -> (fun x -> x))
                                                      mem
                                                      SL.top)
   | NodeInfo'.General x ->  flow' x l 

    let init _ = L.top
  end


module Results (A : ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t )
               (G : CFG.Sig with
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge )
               (M : sig val memory : Memory.t end) =
struct

  type aliasInfo  = BSet.t * bool

  module NodeMap = Map.Make (G.Node)

  let (memory, map) = 
    let module E = Expression
    in
    let module S = Statement
    in
    let map = NodeMap.empty
    in
    let rec expr m = function 
    | E.New t        -> let (m', block) = Memory.allocateDynamic m t 
                        in (m', E.Block block)
    | E.Sub (sub, e) -> let (m', e') = expr m e
                        in (m', E.Sub (sub, e'))
    | E.Value e      -> let (m', e') = expr m e
                        in (m', E.Value e')
    | E.Region e     -> let (m', e') = expr m e
                        in (m',E.Region e')
    | e -> (m, e)
    in
    let stmt m = function
    | S.Assign (e1, e2) -> let (m', e1')  = expr m e1
                           in
                           let (m'', e2') = expr m' e2
                           in
                           (m'', S.Assign (e1', e2'))
    | S.Black  (rs, es) -> let (m', es') =
                             List.fold_right (fun e (m, es) -> let (m', e') = expr m e
                                                               in (m', e' :: es)
                                             )
                                             es
                                             (m, [])
                           in (m', S.Black  (rs, es'))
    in
    let stmts m sts = List.fold_right (fun st (m, sts) -> let (m', st') = stmt m st
                                                          in (m', st' :: sts)
                                      ) 
                                      sts
                                      (m, [])
    in
    List.fold_left (fun (mem, map) node -> let (mem', info) = stmts mem (A.node node) in
                                               (mem', NodeMap.add node info map)
                    )
                    (M.memory, map)
                    (G.nodes G.graph)


  module V = Value (struct let memory = memory end)

  module SL = ASL (V)

  module PView = ProgramView.Make
                  (ProgramView.ForwardAdapter (AAAdapter (SL)))
                  (struct
                     module Concrete = A.Concrete

                     module Abstract = 
                       struct
                         type node = Repr.node

                         type edge = A.Abstract.edge
                       end

                     let node x = if x == G.start 
                                  then NodeInfo'.Start (NodeMap.find x map)
                                  else NodeInfo'.General (NodeMap.find x map)

                     let edge = A.edge
                   end
                  )
                  (G) 

  module Analyse = DFAEngine.RevPost (PView) (DFST.Make (G))

  let before node expr =
    match List.map Analyse.get (G.ins node) with
      []       -> raise (Failure "Semilattice element cannot be assigned to the start node.")
    | hd :: tl  -> V.unwrap (SL.getExprValue expr (List.fold_left SL.cap hd tl))
               
  let after node expr =
    match G.outs node with
      []       -> raise (Failure "Semilattice element cannot be assigned to the end node.")
    | hd :: _  -> V.unwrap (SL.getExprValue expr (Analyse.get hd))

end

