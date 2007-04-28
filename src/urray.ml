type 'a t = 'a array array

let bs = Sys.max_array_length 

let index i = i / bs, i mod bs

let length a =  
  let i = Array.length a in
  if i = 0 then 0 else (i-1) * bs + (Array.length a.(i-1))

let get a i = 
  let i, j = index i in
  a.(i).(j)

let set a i x =
  let i, j = index i in
  a.(i).(j) <- x

let empty () = [||]

let make size x =
  let i, j = index size in
  let i, j = if j > 0 then i+1, j else i, bs in
  Array.init i (fun l -> Array.make (if l = i-1 then j else bs) x) 

let init size f =
  let i, j = index size in
  let i, j = if j > 0 then i+1, j else i, bs in
  Array.init i (fun l -> Array.init (if l = i-1 then j else bs) (fun k -> f (l * bs + k))) 

let iteri f x = Array.iteri (fun i a -> Array.iteri (fun j b -> f (i*bs+j) b) a) x
let iter  f x = Array.iter  (fun   a -> Array.iter  (fun   b -> f          b) a) x

let of_list l = 
  match l with
  | [] -> [||]
  | hd :: tl ->
      let a = make (List.length l) hd in
      ignore (List.fold_left (fun i x -> set a i x; i+1) 0 l);
      a

(* Modest regression test 

let _ =
  let a = init 35 (fun i -> i) in
  Printf.printf "Length: %d\n" (length a);
  for i = 0 to (length a) - 1
  do
    Printf.printf "a.(%d) = %d\n" i (get a i)
  done;
  for i = 0 to (length a) - 1
  do
    set a i (i+50);
    Printf.printf "a.(%d) = %d\n" i (get a i)
  done
*)

