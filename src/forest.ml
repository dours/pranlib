
module type Sig = 
sig

    type t
    
    type node_type

    val create : unit -> t
     
    val join : t -> node_type -> node_type -> t

    val root : t -> node_type -> node_type
    
    val parent : t -> node_type -> node_type    

end 

module Make (G : CFG.Sig ) (O : Order.Sig) =
struct 

    type t = (int * int) Urray.t
    
    type node_type = G.Node.t
   
    let create unit = 
      let nodes = Urray.make 
                  (G.nnodes G.graph ) 
                  (-1,-1) in
      nodes

    let findRoot nodes el_id =
      let rec root el_id rewrite = 
        let (el_parent, el_root) = Urray.get nodes el_id in 
        if el_root = -1 then (
          el_id
        ) else (
          let root = root el_root true in
          if rewrite then Urray.set nodes el_id (el_parent, root);
          root
        ) in
      root el_id false
      
    let join nodes leaf parent =
      let set = Urray.set nodes in   
      let leaf_id = O.number leaf in 
      let parent_id = O.number parent in
      set leaf_id (parent_id, findRoot nodes parent_id) ;
      nodes

    let root nodes leaf = O.node (findRoot nodes (O.number leaf))
    
    let parent nodes leaf = 
      let get = Urray.get nodes in
      let leaf_id = O.number leaf in
      let (leaf_parent, leaf_root) = get leaf_id in
      if (leaf_parent = -1) then (
        leaf
      ) else (
        O.node leaf_parent
      )

end


