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
																						type Concrete.edge = G.Edge.t)
	module StatementConstructor : 
	sig
      val construct : string -> string list -> DFAC.Statement.t
	end
end

module Make :
sig
  include Sig
end