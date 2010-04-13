(*
 * AliasView: Alias analysis implementation.
 * Copyright (C) 2008-2010
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

(** {2 Alias Language} *)

(** Region representation. *)
module Region :
  sig

    (** Type of the region. *)
    type t 

    (** [name r] returns the name of region [r]. *)
    val name : t -> string

  end

(** Block type representation. *)
module Type : Tree.Sig with type label = Region.t

(** Block info signature. *)
module type BlockInfo =
  sig
    
    (** Type of the info. *)
    type t
	  
    (** [toString info] returns string representation of [info]. *)
    val toString : t -> string
	
    (** [region info] returns region associated with a block information [info]. *)
    val region : t -> Region.t
	
  end

(** Memory block signature. *)
module type Block =
  sig
    
    (** Underlying block info module. *)
    module Info : BlockInfo

    (** Repesentation of a tree with nodes labeled by block info. *)
    module InfoTree : Tree.Sig with type label = Info.t

    (** Type of the memory block. *)
    type t  

    (** Relation between two blocks. *)
    type rel =
      | Same       (** blocks are equal*)
      | Different  (** blocks do not overlap *)
      | Nested     (** blocks are nested *)
      | Containing (** reversed Nested relation *)

    (** [subblocks b] returns array consisting of all subblocks of block [b]. *)
    val subblocks : t -> t array 

    (** [typ b] returns type of block [b]. *)
    val typ : t -> Type.t

    (** [info b] returns information bound with the block [b]. *)
    val info : t -> Info.t

    (** [relation b1 b2] returns relation between [b1] and [b2]. *)
    val relation : t -> t -> rel

    (** [compare b1 b2] provides order on blocks. *)
    val compare : t -> t -> int

  end

(** Expression signature. *)
module type Expr =
  sig

    (** Underlying block module. *)
    module Block : Block

    (** Type of the expression. *) 
    type t 

    (** [alloc t] creates an expression which evaluates to a dynamically alocated block of type [t]. *)
    val alloc  : Block.InfoTree.t -> t

    (** [block b] creates an expression evaluating to block [b]. *) 
    val block  : Block.t -> t

    (** [sub f e] applies function [f] to the result of expression [e].
        [f] is a function returning subblock of a given block or
        [None] if operation is not applicable to given block.
     *)
    val sub : (Block.t -> Block.t option) -> t -> t

    (** [value e] denotes dereference of a block to which expression [e] evalutes. *)
    val value : t -> t

    (** [region e] denotes unspecified operation returning any block from the region
        (or from some of its subregions) to which value of [e] belongs.
     *)
    val region : t -> t

    (** [some r] denotes unspecified operation returning any block from the region [r]
        or some of its subregions.
     *)
    val some : Region.t -> t 

    (** [oneOf e1 e2] creates an expression which result is either result of [e1] or of [e2] *)
    val oneOf : t -> t -> t

    (** [undef] creates an expression with undefined element as a value. *)
    val undef  : t

    (** [any] creates an expression which may be evaluated to any block. *)
    val any : t

  end

(** Statement signature. *)
module type Stmt =
  sig
 
    (** Underlying expression module. *) 
    module Expr : Expr

    (** Type of the statement. *)
    type t

    (** [assign (e1, e2)] creates assignment statement that
        lets block to which [e1] evalutes to contain reference
        on block to which [e2] evalutes.
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
module type Memory =
  sig

    (** Underlying block module. *)
    module Block : Block

    (** Exception that is thrown when a region that does not belong to the memory is 
        being addressed.
     *)
    exception RegionNotFound

    (** Type of the memory. *)
    type t

    (** [empty] denotes empty memory. *)
    val empty : t

    (** [createRegion n f m] creates in memory [m] a new region named [n]
        and a set of son-father relationships between it and regions from the [f] list.
        Returns updated memory and created region.
     *)
    val createRegion : name:string -> ?super:(Region.t list) -> t -> t * Region.t 

    (** [allocateBlock t m] allocates in memory [m] a new block of type [t]
        and returns updated memory and the allocated block.
     *) 
    val allocateBlock : Block.InfoTree.t -> t -> t * Block.t

  end

(** {2 Alias analysis representation} *)

(** Signature of alias analysis framework. *)
module type Sig = 
  sig

    (** Underlying block module. *)
    module S : Stmt

    (** Underlying memory module. *)
    module M : Memory with module Block = S.Expr.Block

    (** Memory instance module signature. *)
    module type MemoryInstance =
      sig

        (** Memory instance value. *)
        val memory : M.t

      end

    (** Asserts about points-to relationship that can be stated based on analysis results. *)
    module type AssertsSig =
      sig
        type node

        (** Assert type *)
        type t = private
        (** [ValueIsOneOf (b, bs)] corresponds to the assert that value of block [b]
            is one of the blocks from [bs] list.
            Block [b] and blocks [bs] are all user-allocated. *)
        | ValueIsOneOf of M.Block.t * (M.Block.t list)
        (** [ValueIsOneOf (b, bs)] corresponds to the assert that 
            a blocks from [bs] list cannot be a value of [b] block.
            Block [b] and blocks [bs] are all user-allocated *)
        | ValueIsNotOneOf of M.Block.t * (M.Block.t list)

        (** [before n] returns a list of asserts that can be made about every possible program state
            before execution of statement in the node [n] *)
        val before : node -> t list
  
        (** [after n] returns a list of asserts that can be made about every possible program state
            after execution of statement in the node [n] *)
        val after : node -> t list
      end

    (** Functor to create and run analyser. *)
    module Analyse (MI : MemoryInstance)
                   (A: ProgramView.Abstractor with type Abstract.node = S.t list) 
                   (G: CFG.Sig with type Node.t = A.Concrete.node and 
                                    type Edge.t = A.Concrete.edge ) :
      sig



        (** Abstract type of alias analysis results. *)
        type aliasInfo 
    
        (** [before n e] returns result of alias analysis for expression [e] before execution of the statement
            settled in node [n]. Expression [e] must not contain dynamic memory allocation.
            Result may be thought as a descriptor of a set of possible values of [e].
         *)
        val before : G.Node.t -> S.Expr.t -> aliasInfo
    
        (** [after n e] returns result of alias analysis for expression [e] after execution of the statement
            settled in node [n]. Expression [e] must not contain dynamic memory allocation.
            Result may be thought as a descriptor of a set of possible values of [e].
         *)  
        val after : G.Node.t -> S.Expr.t -> aliasInfo
    
        (** [may a1 a2] is [true] iff [a1] and [a2] may alias. *)  
        val may : aliasInfo -> aliasInfo -> bool
        
        (** [must a1 a2] is [true] iff [a1] and [a2] must alias. *)  
        val must : aliasInfo -> aliasInfo -> bool

        (** [undefined a] is Some `May iff [a] may be an undefined value and
                             Some `Must iff [a] must be an undefined value *)
        val undefined : aliasInfo -> [ `May | `Must ] option

        (* Module containing asserts about program state.
           Can be used as a representation of analysis results alternative to may and must functions. *)
        module Asserts : AssertsSig with type node = G.Node.t
          
        (** Analysis results visualizer. *)
        module DOT : 
          sig

            (** [toDOT ()] returns graph representation of the analysis result. *)
            val toDOT : unit -> string            

          end

      end

  end

(** Alias analysis framework constructor. *)
module Make (BI: BlockInfo) : Sig with module S.Expr.Block.Info = BI
  
  

