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

module type Sig =
  sig
    
    module L : Semilattice.Sig
   
    module VA : ViewAdapter.Sig 
 
    val flow : VA.nt -> (L.t -> L.t)

    val init : VA.nt -> L.t

  end


module RDMake (VA : ViewAdapter.Sig) =
  struct
    
    module VA = VA

    module L = Semilattice.BitvRDMake
    
    let flow n = fun x -> x

    let init n = L.bottom    

  end 








