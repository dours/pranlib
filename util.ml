module INTEGER =

struct

type t = int 

let equal x y = (x=y)

let hash x = Hashtbl.hash x
 
let compare x y = if x<y then -1 else if x==y then 0 else 1 

end