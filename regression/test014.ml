open Printf
open DFACommon
open Lv

module GNodeInfo=
struct
	type t = Statement.t list
	
	let toString node_info = 
		let str_repro_map = List.map (fun x -> Statement.toString x) node_info in
		List.fold_right (fun x y -> x^";"^y) str_repro_map "" 
end

module GEdgeInfo=
struct
	type t = EdgeInfo.t
	
	let toString _ = "edge"
end

module G = Digraph.Make(GNodeInfo)(GEdgeInfo)

module PVA : (ProgramView.Abstractor with type Abstract.node = Statement.t list and 
																					type Abstract.edge = EdgeInfo.t and 
																					type Concrete.node = G.Node.t and 
																					type Concrete.edge = G.Edge.t)=
struct
	module Concrete : (ProgramView.Repr with type node = G.Node.t and type edge = G.Edge.t)=
		struct
			type node = G.Node.t
			type edge = G.Edge.t
		end
		
	module Abstract : (ProgramView.Repr with type node = Statement.t list and type edge = EdgeInfo.t)=
		struct
			type node = Statement.t list
			type edge = EdgeInfo.t
		end
		
	let node n = G.Node.info n
	let edge e = G.Edge.info e
end

let _ =
  printf "Unfinished"
	
	  