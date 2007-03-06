
module G = Digraph.Make (struct type t = string let toString x = x end) (struct type t = unit let toString _ = "" end) 

let _ = 
  let g = G.create () in
  let g, n1 = G.insertNode g "1" in
  let g, n2 = G.insertNode g "2" in
  let g, n3 = G.insertNode g "3" in

  let g, _ = G.insertEdge g n1 n2 () in
  let g, _ = G.insertEdge g n2 n3 () in
  let g, _ = G.insertEdge g n2 n1 () in
  let g, _ = G.insertEdge g n3 n2 () in

  let module L = 
    Loops.NestedLoops 
      (DFST.Make (CFG.Make (G) (struct let graph = g let start = n1 end))) in
  let l = L.buildLoops () in
    Printf.printf "%s" (L.LOOPS.toString l)
    


