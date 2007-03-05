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

type rd_sl = Bvect.t

type rd_node = {def : Bvect.t; kill : Bvect.t}

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
      
    type nt = rd_node 

    type gnt = string

    let convert s = match s with 
                    | "" -> {def = Bvect.create 1 true; kill = Bvect.create 1 false}
                    | _ -> {def = Bvect.create 1 true; kill = Bvect.create 1 false}


    let a x y = 12 in
    let x = 12 in 
    a 12x
      
  end

module BitvRDSemilattice = 
  struct

    type t = rd_sl

    let top = Bvect.create 1 true

    let bottom = Bvect.create 1 false

    let cap x y = Bvect.bw_or x y

    let equal x y = x = y

  end

module RDMake =
  struct

    type t = rd_node

    type sl_t = rd_sl
    
    module L = BitvRDSemilattice
    
    let flow nd = fun x -> Bvect.bw_or (Bvect.bw_and x (Bvect.bw_not nd.kill)) nd.def

    let init _ = L.bottom    

  end 








