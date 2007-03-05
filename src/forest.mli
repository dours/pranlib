module type Sig = 
sig

    type t
    
    type node_type

    val create : unit -> t
     
    val join : t -> node_type -> node_type -> t

    val root : t -> node_type -> node_type
    
    val parent : t -> node_type -> node_type    

end 

module Make (G : CFG.Sig ) (O : Order.Sig with type G.Node.t = G.Node.t): Sig 
      with type node_type = G.Node.t  
           
