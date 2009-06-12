(*
 * AliasView: Alias analysis implementation.
 * Copyright (C) 2008-2009
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
  
    (* Type of the region: name and unique id *)
    type t  = string * int  

    (* [name r] returns name of region r *)
    let name = fst
  
    (* [id r] returns id of region r *)  
    let id = snd

    (* [create n i] creates new region with name [n] and id [i] *)
    let create name id = (name, id)

    (* provides order on regions *)
    let compare x y = (snd x) - (snd y)

    (* equality relation on regions *)
    let equal x y = match (x, y ) with
      (name1, id1), (name2, id2) when name1 = name2 && id1 = id2 -> true
    | _ -> false

    (* hash code for regions *)
    let hash = id

  end

module RSet = Set.Make (Region)
module RMap = Map.Make (Region)
 
module Type = Tree.Make (struct type t = Region.t end)

module type BlockInfo =
  sig
    type t
    val toString : t -> string
    val region   : t -> Region.t
  end

module type Block =
  sig

    module Info : BlockInfo
    module InfoTree : Tree.Sig with type label = Info.t
    type t
    type rel = Same | Different | Nested | Containing 
    val subblocks : t -> t array 
    val typ : t -> Type.t
    val info : t -> Info.t
    val relation : t -> t -> rel
    val compare : t -> t -> int
  end

module type Expr =
  sig
    module Block : Block
    type t 
    val alloc  : Block.InfoTree.t -> t
    val block  : Block.t -> t
    val sub    : (Block.t -> Block.t option) -> t -> t
    val value  : t -> t
    val region : t -> t
    val some   : Region.t -> t 
    val undef  : t
    val any    : t
  end

module type Stmt =
  sig
    module Expr : Expr
    type t 
    val assign : Expr.t -> Expr.t -> t
    val black  : Region.t list -> Expr.t list -> t
  end 

module type Memory =
  sig
    module Block : Block
    exception RegionNotFound
    type t
    val empty : t
    val createRegion : t -> string -> Region.t list -> t * Region.t 
    val allocateBlock : t -> Block.InfoTree.t -> t * Block.t
  end
  
(* signature to describe an effect of some actions on some states *)
module type Affection =
  sig
    (* module corresponding to some action *)
    module Action : sig type t end

    (* module corresponding to some state *)
    module State : sig type t end

    (* [affect a s] returns a state to which [s] turn as a result of [a] execution *)
    val affect : Action.t -> State.t -> State.t

  end

(* Functor to create data flow analysis adapter *)
module AAdapter (Repr : ProgramView.Repr)
                (SL   : Semilattice.Base)
                (S    : Stmt)
                (Aff  : Affection with type Action.t = S.t and
                                       type State.t  = SL.t
                ) = 
  struct
    (* Program view *)
    module P = Repr

    (* Data flow analysis semilattice *)
    module L = Semilattice.Make (SL)

    (* Data flow analysis flow functions *)
    let flow stmts l =   
      let rec aux l = function
      | []     -> l
      | hd::tl -> aux (Aff.affect hd l) tl
      in aux l stmts

    (* Initial values *)
    let init _ = L.top
    
  end

module type Sig = 
  sig
    module S : Stmt
    module M : Memory with module Block = S.Expr.Block
    module type MemoryInstance = sig val memory : M.t end
    module Analyse (MI : MemoryInstance)
                   (A: ProgramView.Abstractor with
                        type Abstract.node = S.t list) 
                   (G: CFG.Sig with
                        type Node.t = A.Concrete.node and 
                        type Edge.t = A.Concrete.edge ) :
      sig
        type aliasInfo 
        val before : G.Node.t -> S.Expr.t -> aliasInfo
        val after  : G.Node.t -> S.Expr.t -> aliasInfo
        val may    : aliasInfo -> aliasInfo -> bool
        val must   : aliasInfo -> aliasInfo -> bool
        module DOT : 
          sig
            val toDOT : unit -> string
          end
      end
  end

(* Alias analysis framework creator *)
module Make (BI: BlockInfo) =
  struct

    (* Module to represen alias analysis block *)
    module Block =
      struct

        (* Underlying block info module *)
        module Info = BI

        (* Repesentation of a tree with nodes labeled by block info *)
        module InfoTree = Tree.Make (struct type t = Info.t end)

        (* Type of the memory block:
              [Simple (i, at, info)] - simple block with unique identifier [i],
                                                         allocation type [at]
                                                         associated information [info].
              [Compound (i, at, info, sub)] - compound block; [sub] - array of immediate subblocks 
              [Pseudo (i, r)] - pseudo block allocated in region [r]
                                 (is used to represent all dynamic blocks created in black boxes in region [r])
              Note: allocation type of the block may be Static or Dynamic, static blocks are created by the user,
                    dynamic are created by the analysis to represent blocks allocated using [new] expression.
                    (pseudo-blocks are always dynamic)
         *)
        type t =  Simple   of int * [`Static | `Dynamic] * Info.t |
                  Compound of int * [`Static | `Dynamic] * Info.t * t array |
                  Pseudo   of int * Region.t 

        (* Relation between two blocks *)
        type rel = Same | Different | Nested | Containing

        (* [region b] returns region in which block [b] is allocated (so-called "true" region) *)
        let region = function
        | Simple (_, _, i) | Compound (_, _, i, _) -> Info.region i
        | Pseudo (_, r) -> r

        (* [subblocks b] returns array consisting of all subblocks of block [b] *)
        let subblocks = function
        | Compound (_, _, _, sub) -> sub  
        | _  -> [||]

        (* [typ b] returns type of block [b]: tree which nodes are labeled with true regions of subblocks of [b] *)
        let rec typ = function
        | Compound (_, _, i, sub) -> Type.make (Info.region i) (Array.to_list (Array.map typ sub))
        | Simple   (_, _, i)      -> Type.make (Info.region i) []
        | Pseudo   _              -> raise (Failure "Operation typ is not applicable to pseudo-blocks")

        (* [info b] returns information bound with the block [b] *)
        let info = function
        | Simple (_, _, i) | Compound (_, _, i, _) -> i
        | Pseudo _ -> raise (Failure "Operation info is not applicable to pseudo-blocks")

        (* [relation b1 b2] returns relation between [b1] and [b2] *)
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

        (* [compare b1 b2] provides order on blocks *)
        let compare x y = (id y) - (id x)

        (* [toViewer b] returns view of a block *)
        let toViewer x = 
          let rec aux = function
          | Simple   (i, _, _)     -> View.int i
          | Compound (_, _, _, bs) -> View.insqbr (View.array (Array.map aux bs))
          | Pseudo   _             -> View.string "Pseudo"
        in 
        View.seq [View.string (Region.name (region x)); View.string ":";  (aux x)]

        (* [toViewer b] returns string representation of a block *)
        let toString x = View.toString (toViewer x)
      end

    module BSet = Set.Make (Block) (* Set of memory blocks *)
    module BMap = Map.Make (Block) (* Map with memory blocks as keys *)

    (* Alias language expression module *)
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

    (* Alias language statement module *)
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

    (* Alias analysis memory module *)
    module M = 
      struct
        (* type of block identifiers generator *)
        type idGenerator = unit -> int
 
        (* Underlying block module *)
        module Block = Block

        (* Exception that is thrown when a region that does not belong to the memory is being addressed *)
        exception RegionNotFound = Not_found

        (* Redefines region map module with Not_found exception handling *)
        module RMap = 
          struct
            include RMap
            let find region map =
              try
                find region map
              with Not_found -> raise RegionNotFound
          end

        (* Memory region information representation *)
        module RegionInfo =
          struct
            (* set of blocks * children regions *)
            type t = BSet.t * Region.t list

            (* empty region information *)
            let empty = (BSet.empty, []) 

            (* [addChild fri cr] lets region [cr] to be a child of region [fri] *)
            let addChild (bs, rs) toR = (bs, toR :: rs)

            (* [addBlock ri b] adds block [b] to region info [ri] *)
            let addBlock (bs, rs) block = (BSet.add block bs, rs)
            
            (* [children ri] returns all children regions of region information [ri] *)
            let children = snd

            let fold f acc = function
            | (bs, _) -> BSet.fold f bs acc

          end

        (* module to provide functions to create blocks *)
        module BlockCreator :
          sig

            (* [create m t nextId] creates block (and all of its subblocks if it is compound)
                                        with allocation mode [m] and type [t] using [nextId]
                                        as a free identifiers generator.
                                        Returns a pair of top level block and list of its subblocks.
             *)
            val create : [`Static | `Dynamic] -> Block.InfoTree.t -> idGenerator -> Block.t * Block.t list

          end = 
          struct

            let create mode aT nextId =
              let module T = Block.InfoTree in
              let rec aux aT = match (T.children aT = []) with
                true  -> let block = Block.Simple (nextId (), mode, T.label aT) in
                           (block, [])
              | false -> let (bs, subs) =
                           List.fold_left (fun (bs, subs) aT -> let (block, sub) = aux aT in (block :: bs, sub @ subs))
                                          ([], [])
                                          (T.children aT)
                         in
                         let block = Block.Compound (nextId (), mode, T.label aT, Array.of_list (List.rev bs)) in
                           (block, bs @ subs)
              in aux aT

          end

        (* Module to provide operations on pseudo blocks set *)
        module PseudoStorage :
          sig
             (* Type of pseudo-blocks storage *)
            type t

            (* empty storage *)
            val empty  : t

            (* [update ps rs freeId] updates pseudo storage [ps] with 
                  a tie on a region set [rs] used in one black box statement
                  using [nextId] as a free identifiers generator.
                Returns updated pseudo storage and a set of created blocks.
              *)
            val update : t -> RSet.t -> idGenerator -> t * BSet.t

             (* [take pb ps] returns all pseudo-blocks associated with [pb] in pseudo-storage [ps] *)
            val take   : Block.t -> t -> BSet.t            
          end =
          struct
            (* Map from pseudo blocks to a pseudo blocks set:
                we associate pseudo blocks that may correspond to a pair of dynamic blocks created
                in one black box.
              *)
            type t = BSet.t BMap.t 
            
            let empty = BMap.empty

            let update ps regions nextId =
             let findBlock region map =
               BMap.fold (fun b _ res -> match res with
                            Some _ -> res
                          | None   -> if (Block.region b = region)
                                      then Some b
                                      else None
                          ) map None
             in
             let (newBlocks, oldBlocks) =
               RSet.fold (fun r (nset, oset) ->
                            match findBlock r ps with
                              Some b -> (nset, BSet.add b oset)
                            | None   -> (BSet.add (Block.Pseudo (nextId (), r)) nset, oset)  
                         )
                         regions
                         (BSet.empty, BSet.empty)
            in
            let blockSet = BSet.union newBlocks oldBlocks
            in
            let ps' =
              BSet.fold (fun b ps -> try 
                                      BMap.add b (BSet.union (BMap.find b ps) blockSet) ps
                                    with
                                      Not_found -> BMap.add b blockSet ps
                        )
                        blockSet ps
            in
            (ps', newBlocks)
            
            let take pBlock ps = BMap.find pBlock ps
          end

        (* Type of memory : map from regions to region info * number of allocated regions *)
        type t = { map : RegionInfo.t RMap.t; (* map from regions to region info *)
                   ps  : PseudoStorage.t; (* Storage of pseudo blocks *)
                   rCounter : int; (* number of allocated regions *)
                   nextId : unit -> int (* generator of a next free identifier *)
                 }

        (* Initial empty memory *)
        let empty = {map = RMap.empty;
                     ps = PseudoStorage.empty;
                     rCounter = 0;
                     nextId = let mid = ref (-1)
                              in (fun () -> incr mid; !mid)
                    }

        let createRegion mem name fathers =
          LOG (Printf.printf "Created region %s\n" name); 
          let newRegion = Region.create name mem.rCounter
          in
          ( { mem with map = RMap.fold (fun k v -> RMap.add k 
                                                            (if List.mem k fathers
                                                             then RegionInfo.addChild v newRegion
                                                             else v
                                                            )
                                       )
                                       mem.map 
                                       (RMap.add newRegion RegionInfo.empty RMap.empty);
                       rCounter = mem.rCounter + 1
            },
            newRegion
          ) 

        (* [subregions m r] returns all subregions of region [r] i memory [m] *)
        let subregions mem =
          let rec aux r =
           let rl = RegionInfo.children (RMap.find r mem.map) in
           List.fold_right (fun sr -> RSet.union (aux sr)) rl (RSet.singleton r)
          in
          aux 

        (* Adds block to memory map *)
        let addBlockToMemoryMap block map = 
          let r = Block.region block
          in
          let ri = RMap.find r map
          in
          RMap.add r (RegionInfo.addBlock ri block) map

        let allocAux mode mem aT =
          let (block, subs) = BlockCreator.create mode aT mem.nextId
          in
          let map' = List.fold_right addBlockToMemoryMap (block :: subs) mem.map 
          in
          ({mem with map = map'}, block)

        let allocateBlock   = allocAux `Static

        let allocateDynamic = allocAux `Dynamic

        (* Updates memory with pseudo blocks for some Black(rs, _) statement *)
        let allocatePseudos mem regions =
          let (ps', newBlocks) = PseudoStorage.update mem.ps regions mem.nextId
          in
          let map' = BSet.fold addBlockToMemoryMap newBlocks mem.map
          in
          {mem with map = map'; ps = ps'}

        (* [takePseudos b m] takes all pseudo-blocks associated with [b] in [m] *)
        let takePseudos block mem = PseudoStorage.take block mem.ps

        (* fold for all blocks in region r *)
        let fold f mem r acc =
          RegionInfo.fold f acc (RMap.find r mem.map) 

       (* fold for all memory blocks *)
        let foldAll f mem acc =
          RMap.fold (fun _ ri acc -> RegionInfo.fold f acc ri) mem.map acc
          
      end

    module type MemoryInstance = sig val memory : M.t end
    
    module Analyse (MI : MemoryInstance)
                   (A: ProgramView.Abstractor with
                        type Abstract.node = S.t list) 
                   (G: CFG.Sig with
                        type Node.t = A.Concrete.node and 
                        type Edge.t = A.Concrete.edge ) =
      struct

        module NodeMap = Map.Make (G.Node)

        (* Creates memory updated with dynamic and pseudo-blocks and
            map from old statements containg New expressions to statements
            with dynamic blocks on corresponding places.
            (We merge all dynamic blocks created in one place).
         *)
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
      
        (* Alias analysis semilattice module *)
        module SL = 
          struct

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
         
                (* checks if value is empty *)
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
                 
                (* conversion to viewer representation *)
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
                (* equality relation on semilattice elements *)
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
               
            (* type of semilattice element (actually a map from a block to ist value) *)
            type t = L of ((Value.t BMap.t) * Region.t) list |
                     Bottom
        
            let top = L []
        
            let bottom = Bottom
            
            let (<@>) = Value.(<@>)
            let (</>) = Value.(</>)
            
            (* module providing functions to evaluate expression/block values *)  
            module Eval =
              struct
                 (* [getValue b l] evaluates value of block [b] for semilattice element [l] *)
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
      
                 (* [getExprValue e l] evaluates value of expression [e] for semilattice element [l] *)
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
            
            (* string representation of semilattice element *)
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
        
            (* semilattice operation *)
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
               if Region.compare r1 r2 > 0
               then (m2, r2)::(cap' x tl2)
               else (m1, r1)::(cap' y tl1)
             in match (x, y) with
             | _, Bottom | Bottom, _ -> Bottom
             | L x, L y -> L (cap' x y) 
        
            (* equality relation on semilattice element *)
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
      
            (* [update m v b l] returns new semilattice element
                 with updated value of block [b].
                 If update mode [m] is 'Strong, then value of [b] is replaced with [v],
                 otherwise ('Weak) it is merged with [v].
             *)                  
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
          end 

         (* module to describe effect of statement execution on semilattice elements *)
         module StatementEffect :
            Affection with module Action = S and
                      module State  = SL = 
           struct
             module Action = S
             module State  = SL

             open S
             open SL

             let affect s l = 
               let addSimple block bl = match block with
               | Block.Compound _ -> bl
               | _                -> block :: bl
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

        module Adapter = AAdapter (A.Abstract) (SL) (S) (StatementEffect)

        module PView = ProgramView.Make
                        (ProgramView.ForwardAdapter (Adapter))
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
        
        (* type of alias information *)
        type aliasInfo = SL.Value.t

        let slBefore node = match List.map Analyse.get (G.ins node) with
          []       -> SL.top
        | hd :: tl -> List.fold_left SL.cap hd tl

        let slAfter node = match G.outs node with
            []       -> Adapter.flow (PView.Abstractor.node node) (slBefore node)
          | hd :: _  -> Analyse.get hd
      
        let before node expr = SL.Eval.getExprValue expr (slBefore node) 
                     
        let after node expr = SL.Eval.getExprValue expr (slAfter node) 
          
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
 
        (* Analysis results visualizer *)
        module DOT = 
          struct
            module NodeInfo =
              struct
                (* block * id of graph cluster * may be undefined *)
                type t = Block.t * int * bool

                let toString (b, i, _) = Printf.sprintf "%s:%d" (Block.toString b) i
              end

            module EdgeInfo = 
              struct
                type t = Contains | Points | Clustered of string * string
 
                let toString = function
                | Contains -> "Contains"
                | Points   -> "Points"
                | Clustered (src, dst) -> Printf.sprintf "Clustered : %s -> %s" src dst
              end

            module BG = Digraph.Make (NodeInfo) (EdgeInfo)

            module DotNode =
              struct
                type t = BG.Node.t

                let color node = match (BG.Node.info node) with
                  (_, _, true) -> "color", "green"
                | (_, _, false) -> "color", "blue"

                let attrs node =
                  (color node) ::
                  (match (BG.Node.info node) with
                     ((Block.Pseudo _), _, _) -> [("shape", "diamond")]
                   | (b, _, _) when not (Block.dynamic b)
                                              -> [("shape", "box")]
                   | _                        -> [])
          
                let label node = match (BG.Node.info node) with
                  (Block.Pseudo (_, r), _, _) -> "Pseudo: " ^ Region.name r
                | (block, _, _)               -> BI.toString (Block.info block)
          
                let name node = match BG.Node.info node with
                  (block, id, _) -> Printf.sprintf "block_%d_%d" (Block.id block) id
             end
          
            module DotEdge =
              struct
                type t = BG.Edge.t
          
                let attrs e = match (BG.Edge.info e) with
                  EdgeInfo.Points               -> [("style", "dotted")]
                | EdgeInfo.Contains             -> [("style", "solid")]
                | EdgeInfo.Clustered (src, dst) -> [ ("minlen", "3");
                                                     ("style", "solid"); ("color", "red");
                                                     ("lhead", dst);     ("ltail", src) ]

                let label _ = ""
              end

            module DotCluster =
              struct
                type t = string * string * DotNode.t list
                let name = function | (n, _, _) -> n
                let label = function | (_, s, _) -> s
                let nodes = function | (_, _, list) -> list
                let attrs _ = []
              end

            module PR = DOT.ClusteredPrinter (
              struct
                include Digraph.DotInfo (BG) (DotNode) (DotEdge)
                let attrs x = ("compound", "true") :: (attrs x)
                module Cluster = DotCluster
              end
            )

            (* [addNodeCluster g l i l] adds cluster providing graph representation
                  of semilattice element [l] with id [i] and label [l] to graph [g] *)
            let addNodeCluster graph label id sl =
             LOG (Printf.printf "addNodeCluster id=%d label=%s %!\n" id label);
             let rec addBlock block (graph, map) =
               let aux block subs =  
                 try BMap.find block map, (graph, map)
                 with Not_found ->
                   let (graph, node) = BG.insertNode graph (block, id, false)
                   in
                   let map = BMap.add block node map
                   in
                   node, List.fold_left (fun (g, m) b -> let n, (g', m') = addBlock b (g, m) in
                                                         (fst (BG.insertEdge g' node n EdgeInfo.Contains), m')
                                        ) (graph, map) subs
               in match block with
                 Block.Simple _                -> aux block []
               | Block.Compound (_, _, _, sub) -> aux block (Array.to_list sub)
               | Block.Pseudo _                -> let ps = M.takePseudos block memory in
                                                  let pl = BSet.fold (fun b acc -> b :: acc) ps []
                                                  in aux block pl
             in  
             (* creating graph with all blocks contained in memory and adding edges corresponding
                to a containing relationship *)
             let (graph, map) = M.foldAll (fun b gm -> snd (addBlock b gm)) memory (graph, BMap.empty)
             in 
             let name = Printf.sprintf "cluster%d" id
             in
             let rec sub node = List.fold_right sub (BG.succ node)
             in
             (* building cluster structure *)
             let (clusters, other) =
               BMap.fold (fun block node (cs, bs) -> match block with
                             Block.Pseudo _ -> (cs, node :: bs)
                          | _               -> ( match (sub node []) with 
                                                   [] -> (cs, node :: bs)
                                                 | ns -> let clusterName = Printf.sprintf "%s_%d" name (BG.Node.hash node)
                                                         in
                                                         ((DOT.Clusters.Leaf (clusterName, "", ns)) :: cs, bs)
                                               )
                         ) map ([], [])
             in
            (* adding edges corresponding to a point-to relationship *)
             let graph = 
               BMap.fold
                (fun b n g -> match b with
                              | Block.Pseudo _ | Block.Simple _ ->
                                 let (bs, undef) = SL.Value.unwrap (SL.Eval.getExprValue (Expr.Value (Expr.Block b)) sl)
                                 in
                                 let (b, i, _) = BG.Node.info n
                                 in
                                 let (g, n) = BG.replaceNode g n (b, i, undef)
                                 in
                                 BSet.fold (fun b' g -> fst (BG.insertEdge g n (BMap.find b' map) EdgeInfo.Points)) bs g
                              | _ -> g
                ) map graph
             in
             (graph, DOT.Clusters.Node ((name, label, other),  clusters)) 

            (* [toDOT ()] returns graph representation of the analysis result *)
            let toDOT () =
             LOG (Printf.printf "toDOT conversion started%!\n"); 
             let rec takeClusterNode = function
             | DOT.Clusters.Leaf c        -> List.hd (DotCluster.nodes c)
             | DOT.Clusters.Node (c, sub) -> (match DotCluster.nodes c with
                                                []      -> takeClusterNode (List.hd sub)
                                              | hd :: _ -> hd
                                             )
             in 
             let (graph, cMap, _) =
              List.fold_left
                (fun (g, cMap, i) node ->
                   let label =  G.Node.toString node 
                   and name  = Printf.sprintf "cluster%d" i
                   and labelAfter = "after" 
                   and slAfter    =  slAfter node
                   and labelBefore = "before" 
                   and slBefore    =  slBefore node in
                   let (g, cb) = addNodeCluster g labelBefore (i + 1) slBefore in
                   let (g, ca) = addNodeCluster g labelAfter (i + 2) slAfter in
                   let c = DOT.Clusters.Node ((name, label, []), [cb; ca]) in
                   (g, NodeMap.add node c cMap, i + 3)
                )
               (BG.create (), NodeMap.empty, 0)
               (G.nodes G.graph)
             in (* adding edges between clusters *)
             let clusterName = function
             | DOT.Clusters.Leaf c | DOT.Clusters.Node (c, _) -> DotCluster.name c
             in
             let graph = 
              NodeMap.fold
                (fun node cluster graph ->
                   let src = takeClusterNode cluster in
                     List.fold_right
                       (fun node' graph ->
                          let cluster' = NodeMap.find node' cMap in
                          let dst = takeClusterNode cluster' in
                            fst (BG.insertEdge graph src dst (EdgeInfo.Clustered ((clusterName cluster),
                                                                                  (clusterName cluster'))))
                       )
                       (G.succ node)
                       graph
                ) cMap graph
             in 
             PR.toDOT (graph, NodeMap.fold (fun _ c l -> c :: l) cMap [])  

          end
      end
  end


