
module NodeInfo = struct
    type t = int  
    let toString t = string_of_int t
end

module EdgeInfo = struct
    type t = string  
    let toString t = t
end

module G = Digraph.Make (NodeInfo) (EdgeInfo) ;;

let g    = G.create () in
let g, n1 = G.insertNode g 1 in
let g, n2 = G.insertNode g 2 in
let g, n3 = G.insertNode g 3 in
let g, n4 = G.insertNode g 4 in
let g, n5 = G.insertNode g 5 in
let g, n6 = G.insertNode g 6 in
let g, n7 = G.insertNode g 7 in
let g, n8 = G.insertNode g 8 in

let g, e12 = G.insertEdge g n1 n2 "1->2" in
let g, e23 = G.insertEdge g n2 n3 "2->3" in
let g, e34 = G.insertEdge g n3 n4 "3->4" in
let g, e45 = G.insertEdge g n4 n5 "4->5" in

let g, e28 = G.insertEdge g n2 n8 "2->8" in
let g, e82 = G.insertEdge g n8 n2 "8->2" in
let g, e37 = G.insertEdge g n3 n7 "3->7" in
let g, e73 = G.insertEdge g n7 n3 "7->3" in
let g, e46 = G.insertEdge g n4 n6 "4->6" in
let g, e64 = G.insertEdge g n6 n4 "6->4" in
(*Printf.printf "%s\n" (G.toDOT g);*)
let module DFST = DFST.Make (G) (struct let graph = g let start = n1 end) in
(*Printf.printf "%s\n" ( DFST.DOT.toDOT () );*)
let module H = Hammocks.Make(DFST) in
(*List.iter (fun node -> Printf.printf "node: %d, K: %d\n" (H.T.G.Node.index node) (H.K.number node)) (H.T.G.nodes H.T.graph)*)
Printf.printf "%s\n" ( H.DOT.toDOT () )