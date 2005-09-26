open Printf

module type SIG =
sig

  type t

  module HSHT:Hashtbl.S with type key = t

  type info_out = 
  {

    get_childs : t -> t list;

    get_parent : t -> t option;

    is_child : t -> t -> bool;
     
    get_root: t

  }

  type info_in = 
  {

    iter : (t -> t -> unit) -> unit;

    root : t;

    size : int;

  }

  val create : info_in -> info_out

end


module type TYPE =
sig

  type t

  val equal : t -> t -> bool

  val hash : t -> int
  
  val toString : t -> string 

end

module TYPE_INT:(TYPE with type t = int) =
struct

  type t = int

  let equal x y = (x == y)

  let hash x = Hashtbl.hash x

  let toString x = string_of_int x

end


module Make (T:TYPE) = 
struct

  type t = T.t
  
  module HSHT = Hashtbl.Make(T)

  type info_out =
  {
    get_childs: t -> t list;
    get_parent: t -> t option;
    is_child: t -> t ->bool;
    get_root: t
  }

  type info_in =
  {
    iter: ( t -> t -> unit ) -> unit;   
    root: t;   
    size: int
  }

  let create info =
    let n = info.size in
    let tree_table = HSHT.create n in
    let tree_inverse = HSHT.create n in
    let itr el el_par =
      if (HSHT.mem tree_table el_par) then
        let cur_el = HSHT.find tree_table el_par in
        let cur_el = match cur_el with (c_e,_,_) -> c_e in
        HSHT.replace tree_table el_par (el::cur_el,-1,-1)
      else (
        HSHT.add tree_table el_par ([el],-1,-1);
        HSHT.add tree_inverse el el_par ) in
    info.iter itr;
    let rec build_index node pre post =
      if (HSHT.mem tree_table node) then (
        let cur_el = match (HSHT.find tree_table node) with (c_el,_,_) -> c_el in
          let ch_n = List.fold_left (
                fun cur_ch_n son -> cur_ch_n + build_index son (pre+1+cur_ch_n) (post - cur_ch_n)
              ) 0 cur_el in
          HSHT.replace tree_table node (cur_el, pre, post - ch_n);ch_n+1
      ) else (
        HSHT.add tree_table node ([],pre,post);1
      )
    in 
    let unused = build_index info.root 0 ( info.size - 1 ) in
    {

      get_childs = 
        ( fun el -> 
             if HSHT.mem tree_table el then 
              match (HSHT.find tree_table el) with
                (el_childs,_,_) -> el_childs
            else [] );

      get_parent = 
        ( fun el -> 
            if HSHT.mem tree_inverse el then 
              Some (HSHT.find tree_inverse el)
            else None );
   
      is_child  = 
        ( fun x y -> 
            let (_,x_pre,x_post) = HSHT.find tree_table x in
            let (_,y_pre,y_post) = HSHT.find tree_table y in
            ( x_pre > y_pre && x_post > y_post ) );

      get_root  = info.root 

    }

end





