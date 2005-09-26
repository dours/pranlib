module INTEGER :

sig

  type t = int

  val equal : int -> int -> bool

  val hash : int -> int 

  val compare : int -> int -> int 

end
