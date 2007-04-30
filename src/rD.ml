module type Set =
  sig

    type 'a t

    val empty : 'a t
    val full  : 'a t

    val union     : 'a t -> 'a t -> 'a t
    val intersect : 'a t -> 'a t -> 'a t
    val diff      : 'a t -> 'a t -> 'a t

  end

module Def (S : Set) =
  struct

    type def = int

    val clobbers : def -> def S.t

  end

module P =
  struct

    type t

    val defs : t -> def list

  end

