module G = Digraph.Make (struct type t = string let toString x = x end) (struct type t = unit let toString _ = "" end) 

let _ = 
  let g = G.create () in
  let g, n1 = G.insertNode g "1" in
  let g, n2 = G.insertNode g "2" in
  let g, n3 = G.insertNode g "3" in
  let g, n4 = G.insertNode g "4" in
  let g, n5 = G.insertNode g "5" in
  let g, n6 = G.insertNode g "6" in
  let g, n7 = G.insertNode g "7" in

  let g, _ = G.insertEdge g n1 n2 () in
  let g, _ = G.insertEdge g n2 n3 () in
  let g, _ = G.insertEdge g n3 n4 () in
  let g, _ = G.insertEdge g n4 n2 () in
  let g, _ = G.insertEdge g n4 n3 () in
  let g, _ = G.insertEdge g n1 n5 () in
  let g, _ = G.insertEdge g n5 n6 () in
  let g, _ = G.insertEdge g n6 n5 () in
  let g, _ = G.insertEdge g n7 n6 () in
  let g, _ = G.insertEdge g n4 n7 () in

  let module L = 
    Loops.NestedLoops 
      (DFST.Make (CFG.Make (G) (struct let graph = g let start = n1 end))) in
  let l = L.buildLoops () in
  let tree = L.LOOPS.Tree.create l in 
    Printf.printf "%s" (L.LOOPS.toString l);
    L.LOOPS.Tree.print tree
    
















