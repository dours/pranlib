module NODE_INFO = 
struct 
  type t = int
  let toString x = string_of_int x
end

open Printf

module G = Digraph.Make (NODE_INFO) (NODE_INFO)

let g = G.create ();;
(* Graph nodes *)
let (g, n1) = G.insertNode g 1;;
let (g, n2) = G.insertNode g 2;;
let (g, n3) = G.insertNode g 3;;
let (g, n4) = G.insertNode g 4;;
let (g, n5) = G.insertNode g 5;;
let (g, n6) = G.insertNode g 6;;
(* Graph edges *)
let (g, e1) = G.insertEdge g n6 n1 (6);;         (*                      *)
let (g, e6) = G.insertEdge g n2 n6 (6);;         (*                      *)
let (g, e7) = G.insertEdge g n3 n2 (7);;         (*                      *)
let (g, e8) = G.insertEdge g n4 n3 (8);;         (*                      *)
let (g, e10) = G.insertEdge g n5 n4 (10);;       (*                      *)
(* tree *)                                               
let (g, e11) = G.insertEdge g n4 n5 (11);;       (*                      *)
let (g, e12) = G.insertEdge g n3 n4 (12);;       (*                      *)
let (g, e13) = G.insertEdge g n2 n3 (13);;       (*                      *)
let (g, e14) = G.insertEdge g n1 n2 (14);;       (*                      *)
let (g, e15) = G.insertEdge g n1 n6 (15);;       (*                      *)

module Df = Cfa.DFST(G)

let dfs = Df.create (g,n1);;

module Uf = Unionfind.Make(G.Node)

module Ufi = Unionfind.Make(Util.INTEGER)

module Loop = Loops.LOOP (G) (Uf) (Ufi)

module Dm = Doms.DOM (G)

let info = Loop.loops_gao_lee g dfs;;

let info1 = Loop.loops_halvak_i g dfs;;

let rec output_info nd pref = 
  let childs = info.Loop.INT_TREE_BUILDER.get_childs nd in
  if childs <> [] then (
    printf "%sNode %i\n" pref nd
  )
  else (
    printf "%s%i\n" pref nd
  );
  List.iter (fun x -> output_info x ((pref^" "))) childs in
output_info info.Loop.INT_TREE_BUILDER.get_root "";;

let rec output_info nd pref = 
  let childs = info1.Loop.NODE_TREE_BUILDER.get_childs nd in
  if childs <> [] then (
    printf "%sNode %s\n" pref (G.Node.toString nd)
  )
  else (
    printf "%s%s\n" pref (G.Node.toString nd)
  );
  List.iter (fun x -> output_info x ((pref^" "))) childs in
output_info info1.Loop.NODE_TREE_BUILDER.get_root "";;
  

















