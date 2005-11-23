(*
 * INTEGER wrapper.
 * Copyright (C) 2004
 * Serjic Shkredov, St.Petersburg State University
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

module INTEGER =
struct

  type t = int 

  let equal x y = (x=y)

  let hash x = Hashtbl.hash x
 
  let compare x y = if x<y then -1 else if x==y then 0 else 1 

end
