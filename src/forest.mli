
(** Abstract forest signature *)
module type Sig = 
sig

    (** Forest type *)
    type t
    
    (** Type of elements stores in the forest *)
    type node_type

    (** creates empty forest *)
    val create : unit -> t
     
    (** joins it's first parameter of node_type to the second *)
    val join : t -> node_type -> node_type -> unit

    (** Poot element of the hierarchy. *)
    val root : t -> node_type -> node_type
    
    (** Parent of given element. *)
    val parent : t -> node_type -> node_type option  

    (** Simple string representation of the forest *)
    val toString : t -> string
    
    (** This module provides top-to-down tree view *)
    module Tree :
    sig
      
      (** Type of element of this view of the forest *)
      type treeEl = Node of node_type 
                   | Head of node_type * (treeEl list)
      
      (** Creates top-to-down view of the forset *)
      val create : t -> treeEl list

      (** dotty clusterded representation *)
      val toDOT : treeEl list -> string 
            
      (** Traces forest *)
      val print : treeEl list -> unit 
       
    end
    
end 

module Make (G : CFG.Sig ) (O : Order.Sig with module G = G): Sig 
      with type node_type = G.Node.t

           
