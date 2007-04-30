module type Set =
  sig

    type t

    val empty : t
    val full  : t

    val union     : t -> t -> t
    val intersect : t -> t -> t
    val diff      : t -> t -> t

  end

module Abstract =
  struct

    type def  = int
    type node = 
    type edge = unit	  

  end

