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
    | Different (** block do not overlap *)
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
    type t =
    | New    of Region.t * Type.t (** [New (r, t)] is a block of type [t] dynamically alocated in region [r] *)
    | Block  of Block.t (** [Block b] denotes block [b] *)
    | Sub    of (Block.t -> Block.t option) * t (** [sub (f, e)] applies function [f] to the result of expression [e].
                                                     [f] is a function returning subblock of a given block or [None]
                                                    if operation is not applicable to given block
                                                *)
    | Value  of t (** [Value e] denotes dereference of block to which expression [e] evalutes *)
    | Unspec of t (** [Unspec e] denotes unspecified operation returning any block from region
                      to which value of [e] belongs.
                  *)
    | Any (** This expression may be evaluated to any block *)


  end

(** Statement type *)
module Statement :
  sig 

   (** Type of the statement *)
    type t =
    | Assign of Expression.t * Expression.t (** [Assign (e1, e2)] lets block to which [e1] evalutes to contain reference
                                                on block to which [e2] evalutes
                                            *)
    | Black of Region.t list * Expression.t list
     (** [Black (rs, es)] stands for ``external function call''. It is presumed that external   	                                            function is any program that can use blocks from regions [rs] and blocks to which                                                          expressions [es] are evaluated, but cannot use expressions [Unspec] and [Any] *)
                  
  end

(** Memory of alias analysis. Contains allocated blocks and regions. *)
module Memory : 
  sig
    (** [create name] creates and returns new region named [name] *)
    val create   : string -> Region.t

    (** [allocate t r] allocates and returns new block of type t in region r *)
    val allocate : Type.t -> Region.t -> Block.t

   (** [cleanup ()] frees all allocated regions and blocks *)
    val cleanup  : unit   -> unit  
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
                    type Node.t = A.Concrete.node and 
                    type Edge.t = A.Concrete.edge) :
  sig

  (** type of may analysis result:
      set of blocks on which an alias may exist and a flag indicating if block value can be undefined
  *)
  type may = Set.Make(Block).t * bool

  (** [aliases n b] returns result of alias analysis for block [b] in node [n] *)
  val alias : G.Node.t -> Block.t -> may

  end

