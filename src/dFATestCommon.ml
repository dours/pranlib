(*
 * DFATestCommon: contains common modules for data flow analysis tests
 * Copyright (C) 2008
 * Andrey Serebryansky, St.Petersburg State University
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file COPYING).
 *)

open Printf

module type Sig =
sig
  module SimpleVariable :
  sig
    include DFACommon.Variable
  end
  
  module DFAC : DFACommon.Sig
    
  module GNodeInfo : 
	sig
		type t = DFAC.Statement.t list
		
		val toString : t -> string 
	end
	
	module GEdgeInfo :
	sig
		type t = DFAC.EdgeInfo.t
		
		val toString : t -> string
	end
	
	module G : Digraph.Sig with type Node.info = GNodeInfo.t and type Edge.info = GEdgeInfo.t
	
	module PVA : (ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and 
																						type Abstract.edge = DFAC.EdgeInfo.t and 
																						type Concrete.node = G.Node.t and 
																						type Concrete.edge = G.Edge.t)(* : 
     sig
		module Concrete : (ProgramView.Repr with type node = G.Node.t and type edge = G.Edge.t) 
		module Abstract : (ProgramView.Repr with type node = DFAC.Statement.t list and type edge = DFAC.EdgeInfo.t)
                   end*)
	
	module StatementConstructor : 
	sig
      val construct : string -> string list -> DFAC.Statement.t
	end
end

module Make=
struct
  module SimpleVariable=
  struct
    type t = string
    
    let equals x y = (String.compare x y) == 0
    
    let toString x = x
      
    let make s = (s : t)
  end
  
    module DFAC = DFACommon.Make(SimpleVariable)
      
    module GNodeInfo=
	struct
		type t = DFAC.Statement.t list
		
		let toString node_info = 
			let str_repro_map = List.map (fun x -> DFAC.Statement.toString x) node_info in
			List.fold_right (fun x y -> x^";"^y) str_repro_map "" 
	end
	
	module GEdgeInfo=
	struct
		type t = DFAC.EdgeInfo.t
		
		let toString _ = "edge"
	end
	
	module G = Digraph.Make(GNodeInfo)(GEdgeInfo)
	
	module PVA : (ProgramView.Abstractor with type Abstract.node = DFAC.Statement.t list and 
																						type Abstract.edge = DFAC.EdgeInfo.t and 
																						type Concrete.node = G.Node.t and 
																						type Concrete.edge = G.Edge.t)=
	struct
		module Concrete : (ProgramView.Repr with type node = G.Node.t and type edge = G.Edge.t)=
			struct
				type node = G.Node.t
				type edge = G.Edge.t
			end
			
		module Abstract : (ProgramView.Repr with type node = DFAC.Statement.t list and type edge = DFAC.EdgeInfo.t)=
			struct
				type node = DFAC.Statement.t list
				type edge = DFAC.EdgeInfo.t
			end
			
		let node n = G.Node.info n
		let edge e = G.Edge.info e
    end
	
	module StatementConstructor=
	struct
	  let construct lp rp = DFAC.Statement.makeAssign (DFAC.V.make lp)  (List.map DFAC.V.make rp)
	end
end