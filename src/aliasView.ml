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
module Tree = 
  struct
    module type Sig = 
      sig

        type mark

        type t

        val makeLeaf : mark -> t

        val makeNode : mark -> t list -> t
 
        val mark : t -> mark

        val isLeaf : t -> bool

        val children : t -> t list

      end  

    module Make (M : sig type mark end) =
      struct
        type mark = M.mark  

        type t = Leaf of mark | Node of mark * t list

        let makeLeaf i = Leaf i

        let makeNode i ch = Node (i, ch)

        let mark = function
        | Leaf i | Node (i, _) -> i

        let isLeaf = function
        | Leaf _ -> true | Node _ -> false

        let children = function
       | Leaf _ -> [] | Node (_, ch) -> ch

      end
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

module RSet = Set.Make (Region)
module RMap = Map.Make (Region)

module type BlockInfo =
  sig
    type t

    val toString : t -> string
    
    val region   : t -> Region.t
 end
 
module Type = Tree.Make (struct type mark = Region.t end)


module type BlockSig =
  sig

    module Info : BlockInfo

    module InfoTree : Tree.Sig with type mark = Info.t

    type t

    type rel = Same | Different | Nested | Containing 

    val subblocks : t -> t array 

    val typ : t -> Type.t

    val info : t -> Info.t

    val relation : t -> t -> rel
    
    val compare : t -> t -> int

  end

module type ExprSig =
  sig
    module Block : BlockSig

    type t 

    val alloc  : Block.InfoTree.t -> t

    val block  : Block.t -> t

    val sub    : (Block.t -> Block.t option) -> t -> t

    val value  : t -> t
    
    val region : t -> t

    val some : Region.t -> t 

    val undef  : t

    val any    : t
  end

module type StmtSig =
  sig
  
    module Expr : ExprSig

    type t 

    val assign : Expr.t -> Expr.t -> t

    val black  : Region.t list -> Expr.t list -> t

  end 

module type MemorySig =
  sig
    module Block : BlockSig
    
    exception RegionNotFound

    type t

    val empty : t

    val createRegion : t -> string -> Region.t list -> t * Region.t 
    
    val allocateBlock : t -> Block.InfoTree.t -> t * Block.t
  end
  
module type SemilatticeSig = 
  sig
    include Semilattice.Base

    module S : StmtSig
    
    module Transformer :
      sig
        val apply : S.t -> t -> t
      end

  end

module AAdapter (Repr : ProgramView.Repr)
                (SL : SemilatticeSig)  = 
  struct
    module P = Repr

    module L = Semilattice.Make (SL)

    let flow stmts l =   
      let rec aux l = function
      | []     -> l
      | hd::tl -> aux (SL.Transformer.apply hd l) tl
      in aux l stmts

    let init _ = L.top
  end

module type Sig = 
  sig

    module S : StmtSig

    module M : MemorySig with module Block = S.Expr.Block

    module Analyse (MI : sig val memory : M.t end)
                   (A: ProgramView.Abstractor with
                        type Abstract.node = S.t list) 
                   (G: CFG.Sig with
                        type Node.t = A.Concrete.node and 
                        type Edge.t = A.Concrete.edge ) :
      sig
        (* Abstract type of alias analysis results *)
        type aliasInfo 
    
        (** [before n b] returns result of alias analysis for expression [e] before execution of the statement
                         settled in node [n].
            Expression [e] must not contain dynamic memory allocation.
         *)
        val before : G.Node.t -> S.Expr.t -> aliasInfo
    
        (** [after b] returns result of alias analysis for expression [e] after execution of the statement
                      settled in node [n].
            Expression [e] must not contain dynamic memory allocation.
         *)  
        val after  : G.Node.t -> S.Expr.t -> aliasInfo
    
        (** [may a1 a2] is [true] if and only if [a1] and [a2] may alias *)  
        val may    : aliasInfo -> aliasInfo -> bool
        
        (** [must a1 a2] is [true] if and only if [a1] and [a2] must alias *)  
        val must   : aliasInfo -> aliasInfo -> bool
      end

  end

module Make (BI: BlockInfo) =
  struct
  
    module Block =
      struct

        module Info = BI

        module InfoTree = Tree.Make (struct type mark = Info.t end)

        type t =  Simple   of int * [`Static | `Dynamic] * Info.t |
                  Compound of int * [`Static | `Dynamic] * Info.t * t array |
                  Pseudo   of int * Region.t 

        type rel = Same | Different | Nested | Containing

        let region = function
        | Simple (_, _, i) | Compound (_, _, i, _) -> Info.region i
        | Pseudo (_, r) -> r

        let subblocks = function
        | Compound (_, _, _, sub) -> sub  
        | _  -> [||]

        let rec typ = function
        | Compound (_, _, i, sub) -> Type.makeNode (Info.region i) (Array.to_list (Array.map typ sub))
        | Simple   (_, _, i)      -> Type.makeLeaf ((Info.region i))
        | Pseudo   _              -> raise (Failure "Operation typ is not applicable to pseudo-blocks")

        let info = function
        | Simple (_, _, i) | Compound (_, _, i, _) -> i
        | Pseudo _ -> raise (Failure "Operation info is not applicable to pseudo-blocks")

        let rec relation x y =
          let nested y = Array.fold_left (fun ex x -> ex || let r = relation y x in
                                                              r = Nested || r = Same
                                         ) false
          in
          match (x, y) with
          | Simple   (xi, _, _),     Simple   (yi, _, _)
          | Compound (xi, _, _ , _), Compound (yi, _, _, _)
          | Pseudo   (xi, _),        Pseudo   (yi, _)
             when xi = yi -> Same 

          | x, Compound (_, _, _, ys) when (nested x ys) -> Nested
          | Compound (_, _, _, xs), y when (nested y xs) -> Containing

          | _ -> Different 

        let id = function
        | Simple (x, _, _) | Compound (x, _, _, _) | Pseudo (x, _) -> x
    
        let dynamic = function
        | Simple (_, `Dynamic, _) | Compound (_, `Dynamic, _, _) | Pseudo _ -> true
        | _ -> false

        let compare x y = (id y) - (id x)

        let toViewer x = 
          let rec aux = function
          | Simple   (i, _, _)     -> View.int i
          | Compound (_, _, _, bs) -> View.insqbr (View.array (Array.map aux bs))
          | Pseudo   _             -> View.string "Pseudo"
        in 
        View.seq [View.string (Region.name (region x)); View.string ":";  (aux x)]

        let toString x = View.toString (toViewer x)
      end

    module BSet = Set.Make (Block)
    module BMap = Map.Make (Block)

    module Expr = 
      struct
      
        module Block = Block

        type t =
          New    of Block.InfoTree.t 
        | Block  of Block.t
        | Sub    of (Block.t -> Block.t option) * t
        | Value  of t
        | Region of t 
        | Some   of Region.t
        | Undef
        | Any

        let alloc t   = New t

        let block b   = Block b

        let sub   f e = Sub (f, e)

        let value e   = Value e

        let region e  = Region e

        let some r    = Some r

        let undef     = Undef

        let any       = Any

      end

    module S = 
      struct

        module Expr = Expr

        type t =
        | Start
        | Assign   of Expr.t * Expr.t
        | Black    of Region.t list * Expr.t list 
        
        let assign e1 e2 = Assign (e1, e2)

        let black  rs es = Black (rs, es)
        
        let start = Start
      end

    module M = 
      struct
 
        module Block = Block

        exception RegionNotFound = Not_found

        let regToRegInfo map region  = 
          try
            RMap.find region map
          with Not_found -> raise RegionNotFound

        module RegionInfo =
          struct
            (** set of blocks * children regions *)
            type t = BSet.t * Region.t list

            let empty = (BSet.empty, [])

            let addChild (s, rs) toR = (s, toR :: rs)
            
            let children = snd

            let alloc mode map aT bCounter =
              let module T = Block.InfoTree in
              let getRegion at = Block.Info.region (T.mark at) in
              let counter = ref bCounter in
              let addBlock map r block =
                let (bs, rs)  = regToRegInfo map r in
                RMap.add r (BSet.add block bs, rs) map
              in
              let rec aux map aT = match (T.isLeaf aT) with
                true  -> let block = Block.Simple (!counter, mode, T.mark aT) in
                           incr counter;
                           (addBlock map (getRegion aT) block, block)
              | false -> let (map', bs) =
                           List.fold_left (fun (map, bs) aT -> let (map', block) = aux map aT in  (map', block :: bs))
                                          (map, [])
                                          (T.children aT)
                         in
                           let block = Block.Compound (!counter, mode, T.mark aT, Array.of_list (List.rev bs)) in
                           incr counter;
                           (addBlock map' (getRegion aT) block, block)
              in aux map aT
              
            let allocPseudo (map, region, cntr) =
              let block = Block.Pseudo (cntr, region)
              in
              let (bs, rs)  = regToRegInfo map region
              in
              (RMap.add region (BSet.add block bs, rs) map, block)

            let fold f acc = function
            | (bs, _) -> BSet.fold f bs acc

          end

        module PseudoStorage =
          struct
            type t = BSet.t BMap.t
            
            let empty = BMap.empty

            let create map regions cntr ps =
             let (map', blockSet, cntr) as result =
               RSet.fold (fun r (map, set, cntr) ->
                            let (map', block) = RegionInfo.allocPseudo (map, r, cntr)
                            in
                            (map', BSet.add block set, cntr + 1)
                         )
                         regions
                         (map, BSet.empty, cntr)
            in
            (result, BSet.fold (fun b -> BMap.add b blockSet) blockSet ps)
            
            let take = BMap.find

          end

        (** Type of memory : map from regions to region info * number of allocated regions *)
        type t = { map : RegionInfo.t RMap.t; (* map from regions to region info *)
                   ps  : PseudoStorage.t;
                   rCounter : int; (* number of allocated regions *)
                   bCounter : int (* number of allocated blocks *)
                 }

        let empty = {map = RMap.empty; ps = PseudoStorage.empty; rCounter = 0; bCounter = 0}

        let createRegion mem name fathers = 
          let newRegion = Region.create name mem.rCounter
          in
          ( { map = RMap.fold (fun k v -> RMap.add k (RegionInfo.addChild v newRegion))
                              mem.map 
                             (RMap.add newRegion RegionInfo.empty RMap.empty);
              ps = mem.ps;
              rCounter = mem.rCounter + 1;
              bCounter = mem.bCounter
            },
            newRegion
          ) 

        let subregions mem =
          let rec aux r =
           let rl = RegionInfo.children (regToRegInfo mem.map r) in
           List.fold_right (fun sr -> RSet.union (aux sr)) rl (RSet.singleton r)
          in
          aux 

        let allocAux mode mem aT =
          let (map, block) = RegionInfo.alloc mode mem.map aT mem.bCounter
          in ({map = map; ps = mem.ps; rCounter = mem.rCounter; bCounter = (Block.id block)}, block)

        let allocateBlock   = allocAux `Static

        let allocateDynamic = allocAux `Dynamic

        let allocatePseudos mem regions =
          let ((map, blocks, cntr), ps) = PseudoStorage.create mem.map regions mem.bCounter mem.ps
          in
          {map = map;
           ps  = ps;
           rCounter = mem.rCounter;
           bCounter = cntr
          }

        let takePseudos block mem = PseudoStorage.take block mem.ps

        let fold f mem r acc =
          RegionInfo.fold f acc (regToRegInfo mem.map r) 

        let foldAll f mem acc =
          RMap.fold (fun _ ri acc -> RegionInfo.fold f acc ri) mem.map acc
          
      end
    
    module Analyse (MI : sig val memory : M.t end)
                   (A: ProgramView.Abstractor with
                        type Abstract.node = S.t list) 
                   (G: CFG.Sig with
                        type Node.t = A.Concrete.node and 
                        type Edge.t = A.Concrete.edge ) =
      struct

        module NodeMap = Map.Make (G.Node)

        let (memory, stmtMap) = 
          let map = NodeMap.empty
          in
          let rec expr m = function 
          | Expr.New aT       -> let (m', block) = M.allocateDynamic m aT 
                                 in (m', Expr.Block block)
          | Expr.Sub (sub, e) -> let (m', e') = expr m e
                                 in (m', Expr.Sub (sub, e'))
          | Expr.Value e      -> let (m', e') = expr m e
                                 in (m', Expr.Value e')
          | Expr.Region e     -> let (m', e') = expr m e
                                 in (m', Expr.Region e')
          | e                 -> (m, e)
          in
          let stmt m = function
          | S.Assign (e1, e2) -> let (m', e1')  = expr m e1
                                 in
                                 let (m'', e2') = expr m' e2
                                 in
                                 (m'', S.Assign (e1', e2'))
          | S.Black  (rs, es) -> let (m', es') = List.fold_right (fun e (m, es) -> let (m', e') = expr m e
                                                                                   in (m', e' :: es)
                                                                 ) es (m, [])
                                 in
                                 let rs' =  List.fold_right (fun r -> RSet.union (M.subregions m' r)) rs RSet.empty
                                 in
                                 let m'' = M.allocatePseudos m' rs'
                                 in
                                 (m'', S.Black  (rs, es'))
          in
          let stmts m sts = List.fold_right (fun st (m, sts) -> let (m', st') = stmt m st in (m', st' :: sts)) sts (m, [])
          in
          List.fold_left (fun (mem, map) node -> let (mem', info) = stmts mem (A.node node) in
                                                     (mem', NodeMap.add node info map)
                          )
                          (MI.memory, map)
                          (G.nodes G.graph)
      
        module SL = 
          struct

            module S = S

            (* representation of block value *)
            module Value =
              struct
        
                (* [V (s, r, u)] stands for: set of blocks [s] + set of regions [r] + undefined flag [u]
                   Note: blocks from set [b] should not be contained in any of regions from [r] *)   
                type t = V of BSet.t * RSet.t * bool | Any
        
                (* empty value *)
                let empty = V (BSet.empty, RSet.empty, false)  
                 
                (* undefined value *)
                let undef = V (BSet.empty, RSet.empty, true)
         
                (** checks if value is empty *)
                let isEmpty = function
                | Any -> false
                | V (bs, rs, undf) -> (BSet.is_empty bs) && (RSet.is_empty rs) && (not undf)
        
                (* unions two values *)
                let (<@>) x y = match (x, y) with 
                  _, Any | Any, _ -> Any
                | V (bs1, rs1, undf1), V (bs2 ,rs2, undf2) ->               
                  let filter rs = BSet.filter (fun x -> not (RSet.mem (Block.region x) rs)) in
                  V (BSet.union (filter rs2 bs1) (filter rs1 bs2), RSet.union rs1 rs2, undf1 || undf2)
                  
                (* counts difference of two values *) 
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
                                         (RSet.fold (M.fold BSet.add memory) rs1Bad BSet.empty)
                   in
                   V (BSet.diff bs1'' bs2, rs1Good, undf1 & (not undf2))
                 in
                 match (x, y) with
                   _, Any -> empty
                 | Any, v -> diff (V (M.foldAll BSet.add memory BSet.empty, RSet.empty, true)) v
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
        
                (* converts inner represnetation of block value to the list of blocks + undefined flag *)
                let unwrap = function
                | V (bs, rs, undf) -> (RSet.fold (M.fold BSet.add memory) rs bs, undf)  
                | Any -> (M.foldAll BSet.add memory BSet.empty, true)
        
                (* adds block to value *)
                let addBlock block = function
                | V (bs, rs, undf) -> V (BSet.add block bs, rs, undf)
                | Any -> Any
        
                (* converts value to a string representation *)
                let toString x = View.toString (toViewer x)
                
              end
               
            type t = L of ((Value.t BMap.t) * Region.t) list | Bottom
        
            let top = L []
        
            let bottom = Bottom
            
            let (<@>) = Value.(<@>)
            let (</>) = Value.(</>)
              
            module Eval =
              struct
        
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
                  let addSub sub block set = match block with
                    Block.Pseudo _ 
                             -> BSet.union (M.takePseudos block memory) set
                    | _      -> ( match (sub block) with
                                    Some b -> (match (Block.relation b block) with
                                                 Block.Same | Block.Nested -> BSet.add b set
                                               | _ ->  set        
                                             )
                                  | None   -> set 
                                )
                  in
                  let addVal block value = match block with
                  | Block.Compound _ -> value
                  | simpl -> (getValue simpl l) <@> value
                  in
                  let rec expr = function
                  | Expr.New typ      -> failwith "Internal: Failure in getExprWidth at E.New"
                  | Expr.Block b      -> Value.V (BSet.singleton b, RSet.empty, false)
                  | Expr.Sub (sub, e) -> let addSub' = addSub sub in
                                         (match expr e with
                                           Value.V (bs, rs, undf) ->
                                              Value.V                                        
                                              ( BSet.union (BSet.fold addSub' bs BSet.empty)
                                                           (RSet.fold (M.fold addSub' memory) rs BSet.empty),
                                                RSet.empty,
                                                undf  
                                              )
                                          | Value.Any -> Value.V (M.foldAll addSub' memory BSet.empty, RSet.empty, true)
                                         )
                  | Expr.Value e      -> (match expr e with
                                           Value.V (bs, rs, undf) -> 
                                             BSet.fold addVal
                                                       bs
                                                       (RSet.fold (M.fold addVal memory)
                                                                  rs
                                                                  (Value.V (BSet.empty, RSet.empty, undf))
                                                       )
                                          | Value.Any -> M.foldAll addVal memory Value.empty 
                                         )
                  | Expr.Region e     -> (match expr e with
                                            Value.V (bs, rs, _) ->
                                             Value.V 
                                             ( BSet.empty,
                                               BSet.fold (fun block -> RSet.union (M.subregions memory (Block.region block))) bs rs,
                                               false
                                             )
                                          | Value.Any -> Value.Any
                                         )
                  | Expr.Some r       -> Value.V (BSet.empty, M.subregions memory r, false) 
                  | Expr.Undef        -> Value.undef
                  | Expr.Any          -> Value.Any
                  in expr e
      
              end 
                 
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
      
            let update mode value block = 
             let reg = Block.region block
             in
             let rec aux = function
             | (m, r) :: tl when Region.compare reg r > 0 -> (m, r) :: (aux tl)
             | (m, r) :: tl when Region.compare reg r = 0 ->
               (match mode with
                  `Weak   -> (try
                               let oldValue = BMap.find block m in
                                 (BMap.add block (value <@> oldValue) m,  r) 
                              with Not_found -> (BMap.add block value m,  r)
                             )
                | `Strong -> (BMap.add block value m,  r)
               ) :: tl
             | tl -> (BMap.add block value BMap.empty, reg) :: tl
             in function
             | Bottom -> Bottom
             | L l    -> L (aux l)
            
            (* semilattice element corresponding to an initial program state *)  
            let initial = (M.foldAll (function | Block.Simple _ as block -> update `Strong Value.undef block
                                               | Block.Pseudo _ as block -> update `Strong Value.undef block
                                               | _ -> (fun x -> x))
                                      memory
                                      top
                          )
                         
            module Transformer =
              struct
                open S
               
                let apply s l = 
                 let addSimple block bl = match block with
                 | Block.Compound _ -> bl
                 | _                  -> block :: bl
                 in
                 let exprl e = Eval.getExprValue e l
                 in
                 match s with 
                   Start -> initial  
                 | Assign (e1, e2) ->
                   (match (exprl e1), (exprl e2) with
                      Value.V (bs, rs, _), value ->
                      let updtList = BSet.fold addSimple bs (RSet.fold (M.fold addSimple memory) rs [])
                      in
                      (match updtList with
                         [] -> l
                       | [b] when not (Block.dynamic b) -> update `Strong value b l
                       | bl -> List.fold_right (update `Weak value) bl l 
                      )
                    | Value.Any, value -> M.foldAll (update `Weak value) memory l
                   ) 
                 | Black (rl, el) ->
                   let rs = List.fold_right (fun r -> RSet.union (M.subregions memory r)) rl RSet.empty
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
                       | Block.Simple _ | Block.Pseudo _ -> (Eval.getValue block l) <@> news' 
                       | Block.Compound _ -> let subs = Array.to_list (Block.subblocks block) in
                                              let subv = List.fold_right Value.addBlock subs Value.empty in
                                               subv <@> news'
                      in
                      let news' =  BSet.fold tryAdd (fst (Value.unwrap news)) Value.empty
                      in
                      closure value' (news' </> value')
                   in
                   let clos = closure Value.empty value in
                     cap (BSet.fold (update `Weak clos) (fst (Value.unwrap clos)) top) l
              end
          end 

        module PView = ProgramView.Make
                        (ProgramView.ForwardAdapter (AAdapter (A.Abstract) (SL)))
                        (struct
                           module Concrete = A.Concrete
      
                           module Abstract = A.Abstract
      
                           let node x =
                             let stmts = (NodeMap.find x stmtMap)
                             in
                             if x == G.start 
                             then S.start :: stmts
                             else stmts
      
                           let edge = A.edge
                         end
                        )
                        (G) 
      
        module Analyse = DFAEngine.RevPost (PView) (DFST.Make (G))
        
        type aliasInfo = SL.Value.t
      
        let before node expr =
          match List.map Analyse.get (G.ins node) with
            []       -> raise (Failure "Semilattice element cannot be assigned to the start node.")
          | hd :: tl  -> SL.Eval.getExprValue expr (List.fold_left SL.cap hd tl)
                     
        let after node expr =
          match G.outs node with
            []       -> raise (Failure "Semilattice element cannot be assigned to the end node.")
          | hd :: _  -> SL.Eval.getExprValue expr (Analyse.get hd)
          
        let may a1 a2 = 
          let (s1, _), (s2, _) = (SL.Value.unwrap a1), (SL.Value.unwrap a2)
          in
          not (BSet.is_empty (BSet.inter s1 s2))
          
        let must a1 a2 = 
          let (s1, u1), (s2, u2) = (SL.Value.unwrap a1), (SL.Value.unwrap a2)
          in
          (not u1) && (not u2) &&
          (BSet.cardinal s1 = 1) && (BSet.cardinal s2 = 1) &&
          (let b1, b2 = BSet.choose s1, BSet.choose s2 
           in
           not (S.Expr.Block.dynamic b1) && (S.Expr.Block.relation b1 b2 = S.Expr.Block.Same)
          )

      end
  end


