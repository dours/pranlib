(*
 * ProgramView: program abstraction module for DFA.
 * Copyright (C) 2007
 * Gennadiy Sych, St.Petersburg State University
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

type rd_sl = Bitv.t

type rd_node = {def : Bitv.t; kill : Bitv.t}

module type Sig =
  sig
    type t

    type sl_t

    module L : Semilattice.Sig with type t = sl_t
 
    val flow : t -> (sl_t -> sl_t)

    val init : t -> sl_t

  end


module CilToDefUseAdapter =
  struct 
      
    type nt = rd_node list 

    type gnt = string

    let convert s = match s with 
                    | "" -> [{def = Bitv.create 1 true; kill = Bitv.create 1 false}]
                    | _ -> [{def = Bitv.create 1 true; kill = Bitv.create 1 false}]

  end

module BitvRDSemilattice = 
  struct

    type t = rd_sl

    let top = Bitv.create 1 true

    let bottom = Bitv.create 1 false

    let cap x y = Bitv.bw_or x y

    let equal x y = x = y

  end

module RDMake =
  struct

    type t = rd_node

    type sl_t = rd_sl
    
    module L = BitvRDSemilattice
    
    let flow nd = fun x -> Bitv.bw_or (Bitv.bw_and x (Bitv.bw_not nd.kill)) nd.def

    let init _ = L.bottom    

  end 








