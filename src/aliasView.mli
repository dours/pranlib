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

(** {1 Alias analysis interface} *)


(** Representation of a tree with marked nodes *) 
module Tree :
  sig
  
    (** Tree signature *)
    module type Sig = 
      sig

        (** type of the node marks *)
        type mark

        (** type of the tree *)
        type t

        (** [makeLeaf i] creates a tree consisting of a single node with mark [i] *)
        val makeLeaf : mark -> t

        (** [makeNode i sl] creates a node with mark [i] having nodes from list [sl] as sons *)
        val makeNode : mark -> t list -> t
 
        (** [mark t] returns mark of a root of the tree [t] *)
        val mark : t -> mark

        (* [isLeaf t] returns if a tree is a leaf *)
        val isLeaf : t -> bool

        (** [children t] returns children of a root of the tree [t] *)
        val children : t -> t list

      end  

    (** Functor to construct marked tree module *)
    module Make (M : sig type mark end) : Sig with type mark = M.mark
end

(** {2 Alias Language. *)

(** Region representation *)
module Region :
  sig

    (** Type of the region *)
    type t 

    (** [name r] returns the name of region [r] *)
    val name : t -> string

  end

(** Block info signature *)
module type BlockInfo =
  sig

    (** Type of the info *)
    type t

    (** [toString info] returns string representation of [info] *)
    val toString : t -> string

    (** [region info] returns region associated with a block information [info] *)
    val region   : t -> Region.t
 end

(** Block type representation *)
module Type : Tree.Sig with type mark = Region.t

(** Memory block signature *)
module type BlockSig =
  sig

    (** Underlying block info module *)
    module Info : BlockInfo

    (** Repesentation of a tree with nodes marked by block info *)
    module InfoTree : Tree.Sig with type mark = Info.t

    (** Type of the memory block *)
    type t  

    (** Relation between two blocks *)
    type rel =
    | Same (** blocks are equal*)
    | Different (** blocks do not overlap *)
    | Nested (** blocks are nested *)
    | Containing (** reversed Nested relation *)

    (** [subblocks b] returns array consisting of all subblocks of block [b] *)
    val subblocks : t -> t array 

    (** [typ b] returns type of block [b] *)
    val typ : t -> Type.t

    (** [info b] returns information bound with the block [b] *)
    val info : t -> Info.t

    (** [relation b1 b2] returns relation between [b1] and [b2] *)
    val relation : t -> t -> rel

    (** [compare b1 b2] provides order on blocks *)
     val compare : t -> t -> int
  end


(** Expression signature *)
module type ExprSig =
  sig

    (** Underlying block module *)
    module Block : BlockSig

    (** Type of the expression *) 
    type t 

    (** [alloc t] creates an expression which evaluates to a dynamically alocated block of type [t]*)
    val alloc  : Block.InfoTree.t -> t

    (** [block b] creates an expression evaluating to block [b] *) 
    val block  : Block.t -> t

    (** [sub f e] applies function [f] to the result of expression [e].
        [f] is a function returning subblock of a given block or
       [None] if operation is not applicable to given block.
     *)
    val sub    : (Block.t -> Block.t option) -> t -> t

    (** [value e] denotes dereference of a block to which expression [e] evalutes *)
    val value  : t -> t

    (** [region e] denotes unspecified operation returning any block from the region
                   (or from some of its subregions) to which value of [e] belongs.
     *)
    val region : t -> t

    (** [some r] denotes unspecified operation returning any block from the region [r]
        or some of its subregions *)
    val some : Region.t -> t 

   (** [undef] creates an expression, with undefined element as a value *)
    val undef  : t

    (** [any] creates an expression which may be evaluated to any block *)
    val any    : t
  end


(** Statement signature *)
module type StmtSig =
  sig
 
    (** Underlying expression module *) 
    module Expr : ExprSig 

    (** Type of the statement *)
     type t

    (** [assign (e1, e2)] creates assignment statement that
        lets block to which [e1] evalutes to contain reference
        on block to which [e2] evalutes
     *)
     val assign : Expr.t -> Expr.t -> t

     (** [black (rl, el)] stands for ``external function call''. 
         A set of open blocks is defined as a set of blocks which
         are created in the function or
         can be accessed by operations of subblock or value taking
         from a block belonging to a region from [rl] list or to
         a value of expression from [el] list.
         Black can create blocks in regions [rl] and assign a refererence
         on an open block to an open block.
      *)
     val black  : Region.t list -> Expr.t list -> t
  end


(** {2 Memory model} *)

(** Memory of alias analysis. Contains allocated blocks and regions. *)
module type MemorySig =
  sig

    (** Underlying block module *)
    module Block : BlockSig

    (** Exception that is thrown when a region that does not belong to the memory is being addressed *)
    exception RegionNotFound

    (** Type of the memory *)
    type t

    (** [empty] denotes empty memory *)
    val empty : t

    (** [createRegion m n f] creates in memory [m] a new region named [n]
        and a set of son-father relationships between it and regions from the [f] list.
        Returns updated memory and created region.
     *)
    val createRegion : t -> string -> Region.t list -> t * Region.t 

    (** [allocateBlock m t] allocates in memory [m] a new block of type [t]
       and returns updated memory and the allocated block.
     *) 
    val allocateBlock : t -> Block.InfoTree.t -> t * Block.t
  end


(** {2 Alias analysis representation} *)

(** Signature of alias analysis framework *)
module type Sig = 
  sig

    (** Underlying block module *)
    module S : StmtSig

    (** Underlying memory module *)
    module M : MemorySig with module Block = S.Expr.Block

    (** Functor to create and run analyser *)
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

(** Alias analysis framework creator *)
module Make (BI: BlockInfo) : Sig with
   module S.Expr.Block.Info = BI
  
  

