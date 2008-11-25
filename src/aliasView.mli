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

(** Block kind *)
module Type :
  sig

    (** Kind of the block *)
    type t = 
    | Simple (** indivisible cell of memory *)
    | Compound of t array (** ordered set of blocks *)

  end

(** Region type *)
module Region :
  sig

    (** Type of the region *)
    type t 

    (** [name r] returns the name of region [r] *)
    val name : t -> string

  end

(** Block type *)
module Block :
  sig

    (** Type of the block *)
    type t  

    (** Relation between two blocks *)
    type rel =
    | Same (** blocks are equal*)
    | Different (** blocks do not overlap *)
    | Nested (** blocks are nested *)
    | Containing (** reversed Nested relation *)

    (** [subblocks b] returns array consisting of all subblocks of block [b] *)
    val subblocks : t -> t array 

    (** [region b] returns region to which block [b] belongs to *)
    val region : t -> Region.t

    (** [typ b] returns kind of block [b] *)
    val typ : t -> Type.t

    (** [relation b1 b2] returns relation between [b1] and [b2] *)
    val relation : t -> t -> rel

    (** [compare b1 b2] provides order on blocks *)
    val compare : t -> t -> int

  end

(** Expression type *)
module Expression :
  sig

    (** Type of the expression *) 
    type t 

    (** [alloc r t] creates an expression corresponding to a block of type [t] dynamically alocated in region [r] *)
    val alloc  : Region.t -> Type.t -> t

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
                   (or from some its descendant) to which value of [e] belongs.
     *)
    val region : t -> t

    (** This expression may be evaluated to any block *)
    val any    : t

  end

(** Statement type *)
module Statement :
  sig 

   (** Type of the statement *)
    type t

   (** [assign (e1, e2)] creates assignment statement that
       lets block to which [e1] evalutes to contain reference
       on block to which [e2] evalutes
    *)
    val assign : Expression.t -> Expression.t -> t

    (** [black (rs, es)] stands for ``external function call''. It is presumed that external 
        function is any program that can use blocks from regions [rs] and blocks to which
        expressions [es] are evaluated, but cannot use expressions [Unspec] and [Any]
     *)
    val black  : Region.t list -> Expression.t list -> t

  end

(** Memory of alias analysis. Contains allocated blocks and regions. *)
module Memory : 
  sig

    (** Type of the memory *)
    type t

    (** Exception that is thrown when a region that does not belong to the memory is being addressed *)
    exception RegionNotFound

    (** [empty] denotes empty memory *)
    val empty       : t
 
    (** [createRegion m n f] creates in memory [m] a new region named [n]
        and a set of son-father relationships between it and regions from the [f] list.
        Returns updated memory and created region.
     *)
    val createRegion : t -> string -> Region.t list -> t * Region.t 

    (** [allocateBlock m t r] allocates in memory [m] in region [r] a new block of type [t]
        and returns updated memory and the allocated block, or raises RegionNotFound
        if region [r] is not in the memory [m].
     *) 
    val allocateBlock : t -> Type.t -> Region.t -> t * Block.t

  end

(** Information in node of the graph needed for alias analysis *)
module NodeInfo :
  sig

    (** Node information type *)
    type t = Statement.t list

  end

(** Information in edge of the graph needed for alias analysis *)
module EdgeInfo :
  sig

    (** Edge information type *)
    type t = Empty

  end

(** Alias analysis constuructor. *)
module Results (A: ProgramView.Abstractor with
                    type Abstract.node = NodeInfo.t and
                    type Abstract.edge = EdgeInfo.t)
               (G : CFG.Sig with
                    type Node.t    = A.Concrete.node and 
                    type Edge.t    = A.Concrete.edge )
               (M : sig val memory : Memory.t end) :
  sig

    (** type of alias analysis result:
        set of blocks on which an alias may exist and a flag indicating if block value can be undefined
    *)
    type aliasInfo = Set.Make(Block).t * bool

    (** [before n b] returns result of alias analysis for expression [e] before execution of the statement
                     settled in node [n] *)
    val before : G.Node.t -> Expression.t -> aliasInfo

    (** [after b] returns result of alias analysis for expression [e] after execution of the statement
                  settled in node [n] *)  
    val after  : G.Node.t -> Expression.t -> aliasInfo

  end

