(** Main type *)
type 'a t

(** Gets array length *)
val length : 'a t -> int

(** Gets array element *)
val get : 'a t -> int -> 'a

(** Sets array element *)
val set : 'a t -> int -> 'a -> unit

(** Makes array (see Array.make for details) *)
val make : int -> 'a -> 'a t

(** Constructs array (see Array.init for details) *)
val init : int -> (int -> 'a) -> 'a t

(** Iterators *)
val iter  : (       'a -> unit) -> 'a t -> unit
val iteri : (int -> 'a -> unit) -> 'a t -> unit

(** List conversion *)
val of_list : 'a list -> 'a t
