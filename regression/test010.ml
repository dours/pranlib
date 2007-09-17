open Printf

module G = Digraph.Make (struct type t = string let toString x = x end) (struct type t = unit let toString _ = "" end) 

let _ = 
  let g = G.create () in
  let g, a = G.insertNode g "a" in
  let g, b = G.insertNode g "b" in
  let g, c = G.insertNode g "c" in
  let g, d = G.insertNode g "d" in
  let g, e = G.insertNode g "e" in
  let g, f = G.insertNode g "f" in
  let g, z = G.insertNode g "g" in
  let g, h = G.insertNode g "h" in
  let g, i = G.insertNode g "i" in
  let g, j = G.insertNode g "j" in
  let g, k = G.insertNode g "k" in

  let g, _ = G.insertEdge g a b () in
  let g, _ = G.insertEdge g b c () in
  let g, _ = G.insertEdge g c d () in
  let g, _ = G.insertEdge g d e () in
  let g, _ = G.insertEdge g e f () in
  let g, _ = G.insertEdge g f k () in
  let g, _ = G.insertEdge g c e () in
  let g, _ = G.insertEdge g e c () in
  let g, _ = G.insertEdge g d c () in
  let g, _ = G.insertEdge g b z () in
  let g, _ = G.insertEdge g z h () in
  let g, _ = G.insertEdge g h i () in
  let g, _ = G.insertEdge g i j () in
  let g, _ = G.insertEdge g j k () in
  let g, _ = G.insertEdge g i h () in
  let g, _ = G.insertEdge g j z () in

  let module L = Loops.Make (Dominance.Make (DFST.Make (CFG.Make (G) (struct let graph = g let start = a end)))) in
  let comps = 
    List.map 
      (fun s -> G.DOT.Clusters.Leaf (L.Region.R.NodeSet.fold (fun x l -> x :: l) s [])) 
      (snd (List.split (L.Region.SCC.get ()))) 
  in
  
  printf "%s" (G.Clustered.toDOT g (G.DOT.Clusters.Node ([], comps)))
    















