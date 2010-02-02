open Printf
(* Test for a simple loop*)
(* int[] a;*)
(* int[] b;*)
(* for i1 from 0 to 10 do*)
(*  -> b[i1] = a[i1];*)
 
let _ = 
let module DDAIT = DDAInfo.DDAInfoTest in
let module NI = DDAIT.NodeInfo in
let module EI = DDAIT.EdgeInfo in
let module G = Digraph.Make(NI)(EI) in
let module V = DDAIT.Variable in
let module C = DDAIT.Coefficient in
let module SSC = DDAIT.SubscriptSpecComponent in
let module SS = DDAIT.SubscriptSpec in
let module R = DDAIT.Reference in
let module S = DDAIT.Statement in
let module Val = DDAIT.Value in
let module IVV = DDAIT.IterationVariableValue in
let module L = DDAIT.Loop in
let a = V.make "a" in
let b = V.make "b" in
let i1 = V.make "i1" in
let c1 = C.make_int 1 in
let ss = SS.make [SSC.make i1 c1] in
let a_ref = R.make a (Some ss) in
let b_ref = R.make b (Some ss) in
let inner_loop_statement = S.make a_ref [b_ref] in
let iteration_statement = S.make_evaluatable (
	fun vals -> let v = List.find (fun x -> V.equals (IVV.variable x) i1) vals in
 							Val.add (IVV.value v) (Val.make 1)) in
let i1_lower_bound = Some (IVV.make i1 (Val.make 0)) in
let i1_upper_bound = Some (IVV.make i1 (Val.make 10)) in
let loop = L.make_simple 
												i1 
												iteration_statement 
												i1_lower_bound 
												i1_upper_bound 
												[inner_loop_statement]
												in
let ni1 = NI.make_loop loop in
let g = G.create () in
let g, n1 = G.insertNode g ni1 in
let module MYG = CFG.Make (G)(struct let graph=g let start=n1 end) in
let module DG = DDG.Make (G)(struct let graph=g let start=n1 end) in
let module DS = DDGStructure.Make(DDAIT)(MYG) in
let module DAnalysis = DDA.Make(DDAIT)(MYG)(DG)(DS) in
LOG (printf "Started logging...\n");
LOG (printf "Graph:\n\n%s\n" (G.DOT.toDOT g));
let res = DAnalysis.analyze g DAnalysis.GCDLoopStatementDependencyTest.get_dependencies
in
List.iter (fun x -> LOG(printf "\nDependency:\n%s" (DAnalysis.Dependency.toString x))) res;
() 