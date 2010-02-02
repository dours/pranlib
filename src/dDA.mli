(*
 * DDA: Data-Dependency Analysis interface
 * Copyright (C) 2009
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

(** Signature for dependence distance representation *)
module type DependenceDistanceSig=
sig
	module Info : DDAInfo.Sig
	
	type t
	
	(** [distance_value dd var] returns dependence distance value for given variable/iteration variable *)
	val distance_value : t -> Info.Variable.t -> Info.Value.t
	
	val toString : t -> string  
end

(** Signature for dependency representation *)
module type DependencySig=
sig
	module Info : DDAInfo.Sig
	module DependenceDistance : DependenceDistanceSig with module Info = Info
	
	type t
	
	val from : t -> Info.Statement.t
	
	val _to : t -> Info.Statement.t
	
	val distance : t -> DependenceDistance.t
	
	val toString : t -> string
end

(** Loop statement dependency test signature *)
module type LoopStatementDependencyTestSig= 
sig
	module Info : DDAInfo.Sig
	module DStruct : DDGStructure.Sig with module Info = Info
	module DependenceDistance : DependenceDistanceSig with module Info = Info
	module Dependency : DependencySig with module Info = Info and module DependenceDistance = DependenceDistance
	
	(** [get_dependencies l] returns the list of dependencies between loop statements with corresponding dependency distance *)
	val get_dependencies : Info.Loop.t -> Dependency.t list 
end

(** Main signature for dependency analysis framework *)
module type Sig = 
sig
	module DDAI : DDAInfo.Sig
	module G : CFG.Sig with type Node.info = DDAI.NodeInfo.t and type Edge.info = DDAI.EdgeInfo.t
	module DG : DDG.Sig
	module DStruct : DDGStructure.Sig
	module DependenceDistance : DependenceDistanceSig with module Info = DDAI
	module Dependency : DependencySig with module Info = DDAI and module DependenceDistance = DependenceDistance
	module GCDLoopStatementDependencyTest : LoopStatementDependencyTestSig with module Info = DDAI and module DependenceDistance = DependenceDistance and module Dependency = Dependency
	
	(** [analyze g] analyzes CFG g and returns a data dependency graph *)
	val analyze : G.t -> (DDAI.Loop.t -> Dependency.t list) -> Dependency.t list (* TODO: Return the dependency graph instead of the dependencies list *)
end

(* Default implementation based on DDAInfo *)
module Make (D : DDAInfo.Sig)
						(CG : CFG.Sig with type Node.info = D.NodeInfo.t and type Edge.info = D.EdgeInfo.t)
						(DG : DDG.Sig)
						(DS : DDGStructure.Sig with module Info = D) : Sig 
						with module DDAI=D and module G=CG and module DG=DG