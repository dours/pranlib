module type Sig = 
sig

    type t
    
    type node_type

    (** creates empty forest *)
    val create : unit -> t
     
    (** joins it's first parameter of node_type to the second *)
    val join : t -> node_type -> node_type -> unit

    (** Poot element of the hierarchy. *)
    val root : t -> node_type -> node_type
    
    (** Parent of given element. *)
    val parent : t -> node_type -> node_type option  

    val toString : t -> string
    
end 

module Make (G : CFG.Sig ) (O : Order.Sig with module G = G): Sig 
      with type node_type = G.Node.t

           
