
type rd_sl = Bvect.t

type rd_node = {def : Bvect.t; kill : Bvect.t}

module CilToDefUseAdapter =
  struct 
      
    type nt = rd_node 

    type gnt = string

    let convert s = match s with 
                    | "" -> {def = Bvect.create 1 true; kill = Bvect.create 1 false}
                    | _ -> {def = Bvect.create 1 true; kill = Bvect.create 1 false}


    let a x y = 12 in
    let x = 12 in 
    a 12x
      
  end

module BitvRDSemilattice = Semilattice.Make
    (
     struct
       
       type t = rd_sl
	     
       let top = Bvect.create 1 true
	   
       let bottom = Bvect.create 1 false
	   
       let cap x y = Bvect.bw_or x y
	   
       let equal x y = x = y
	   
     end
    )

module RDMake =
  struct

    type t = rd_node

    type sl_t = rd_sl
    
    module L = BitvRDSemilattice
    
    let flow nd = fun x -> Bvect.bw_or (Bvect.bw_and x (Bvect.bw_not nd.kill)) nd.def

    let init _ = L.bottom    

  end 








