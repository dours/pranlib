
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
let g, n0 = G.insertNode g 0 in
let g, n1 = G.insertNode g 1 in
let g, n2 = G.insertNode g 2 in
let g, n3 = G.insertNode g 3 in
let g, n4 = G.insertNode g 4 in
let g, n5 = G.insertNode g 5 in

let g, e01 = G.insertEdge g n0 n1 "0->1" in
let g, e12 = G.insertEdge g n1 n2 "1->2" in
let g, e23 = G.insertEdge g n2 n3 "2->3" in

let g, e14 = G.insertEdge g n1 n4 "1->4" in
let g, e41 = G.insertEdge g n4 n1 "4->1" in
let g, e45 = G.insertEdge g n4 n5 "4->5" in

let module DFST = DFST.Make (G) (struct let graph = g let start = n0 end) in
let module H = Hammocks.Make(DFST) in
(*Printf.printf "%s\n" ( H.DOT.toDOT () );
H.hammocks ()*)
();

