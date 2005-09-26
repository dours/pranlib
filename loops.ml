exception Flyer_exp                                       

open Printf

open Treebuilder

module LOOP (G: Digraph.Sig) 
            (UFS: Unionfind.S with type elt=G.Node.t)  
            (UFSI:Unionfind.S with type elt=int) = 
struct

  module HSHND = Hashtbl.Make(G.Node)

  module HSHEG = Hashtbl.Make(G.Edge)

  module DOMM = Doms.DOM(G)

  module DFST = Cfa.DFST(G)

  module NODE_TREE_BUILDER = Treebuilder.Make(G.Node)

  module INT_TREE_BUILDER = Treebuilder.Make(TYPE_INT)
   
  type dfst = Cfa.DFST(G).info

  type info = NODE_TREE_BUILDER.info_out

  type info_i = INT_TREE_BUILDER.info_out

  let apply_fold from tooo f init = 
    let rec apply_n n=
      if n<=tooo then f (apply_n (n+1) ) n else init 
    in apply_n from
 
  let collapse uf li body header =
    printf "collapse: %s\n" (G.Node.toString header);
    HSHND.iter (fun x y -> printf "    with: %s\n" (G.Node.toString x);UFS.union header x uf; HSHND.add li x header) body

  let find_loop uf li st hdr = 
    let loop_body = HSHND.create 10 
    and worklist = 
      ( fun lst fi fu ->
          let t = HSHND.create 9 in
          List.iter (fun curr -> 
                let fuh = fu curr in
                if fi fuh && not (HSHND.mem t fuh) then HSHND.add t fuh fuh) lst; t )
                   (List.filter ( fun x -> st.DFST.sort x == DFST.Back) 
                                 (G.ins hdr) )
                   ( fun y ->  not (G.Node.equal y hdr) )
                   ( fun x ->  UFS.find (G.src x) uf ) in 
    let rec iter_front wl =   
      if HSHND.length wl > 0 then (
        HSHND.iter (fun x y -> HSHND.add loop_body x y) wl; 
        let new_wl = HSHND.create 10 
        in let proceed_node nd =
          let in_edges = List.filter (fun x -> (st.DFST.sort x <> DFST.Back)) (G.ins nd) in
          let proceed_node x = 
            let nd = ( UFS.find (G.src x) uf)
            in 
            if ((not (HSHND.mem loop_body nd          )) && 
               ( not( HSHND.mem new_wl nd             )) &&
               ((st.DFST.pre nd) > (st.DFST.pre hdr   )) &&
               ((st.DFST.post nd) > (st.DFST.post hdr))) then HSHND.add new_wl nd nd
          in
          List.iter (proceed_node) in_edges;
        in
        HSHND.iter (fun x y -> proceed_node x) wl;
        iter_front new_wl;
      ) in
    iter_front worklist; 
    if ( HSHND.length loop_body > 0 ) then collapse uf li loop_body hdr

(* Simple Halvak implementation *)
  let loops_halvak g df =
    let uf = UFS.init ( G.nodes g )
    and li = HSHND.create (G.nnodes g) in
    for i = (G.nnodes g)-1 downto 0 do
      find_loop uf li df (df.DFST.pre'1 i)
    done;
    let root_tree_node = df.DFST.pre'1 0 in
    NODE_TREE_BUILDER.create  
    {
      NODE_TREE_BUILDER.iter = 
      ( fun f ->
          List.iter 
            (fun x ->
               printf "iter for node %s\n" (G.Node.toString x);
               if not (G.Node.equal x root_tree_node) then (
                 printf "started\n";
                 if HSHND.mem li x then (
                   f x (HSHND.find li x)
                 )
                 else ( 
                   f x root_tree_node
                 )
               );
               printf "iter end\n";
            ) (G.nodes g)
      );  

      NODE_TREE_BUILDER.root = root_tree_node;

      NODE_TREE_BUILDER.size = (G.nnodes g);
    }
    

(* Improved Halvak implementation *)
  let loops_halvak_i g df  = 
  let li = HSHND.create (G.nnodes g) in
  let add_to_mult_hash hsh key el =  
    if (HSHND.mem hsh key) then 
      let lst = HSHND.find hsh key in 
        HSHND.replace hsh key (el::lst)
     else 
      HSHND.add hsh key [el] in
  let compute_reverse_tree dfs g = (* creates array of parents in tree (using post number!) *)
    let arr = HSHND.create (G.nnodes g) in
    HSHND.add arr dfs.DFST.start dfs.DFST.start;   
    List.iter (fun el -> if dfs.DFST.sort el == DFST.Tree then
                           HSHND.add arr (G.dst el) (G.src el)) (G.edges g);
    arr in
  let lca g df = (* least common ancettor implementation *)
    let uf = UFS.init ( G.nodes g )
    and ans = HSHND.create 99 
    and pred = compute_reverse_tree df g in
    ( apply_fold 0 ((G.nnodes g) - 1)
      (fun gr i -> 
        let nd = (df.DFST.post'1 i) in 
        let pr = HSHND.find pred nd in
        UFS.union pr nd uf; 
        let proceed_out_edge grf e =
          match (df.DFST.sort e) with
            DFST.Forward -> 
              add_to_mult_hash ans nd (nd, G.dst e); 
              G.deleteEdge grf e
          | DFST.Cross -> 
              let ca = UFS.find (G.dst e) uf in (
                add_to_mult_hash ans ca (nd, G.dst e); 
                G.deleteEdge grf e
              )
          | _ -> grf
        in
        List.fold_left (proceed_out_edge) gr (G.outs nd)) g, ans) in
  let (gr, lc) = lca g df in 
  let df = DFST.create (gr, (df.DFST.pre'1 0)) in
  let uf = UFS.init ( G.nodes gr ) in
  for i = (G.nnodes gr)-1 downto 0 do
    find_loop uf li df (df.DFST.pre'1 i);
  done;
  let root_tree_node = df.DFST.pre'1 0 in
  NODE_TREE_BUILDER.create  
  {
    NODE_TREE_BUILDER.iter = 
    ( fun f ->
        List.iter 
          (fun x ->
             printf "iter for node %s\n" (G.Node.toString x);
             if not (G.Node.equal x root_tree_node) then (
               printf "started\n";
               if HSHND.mem li x then (
                 f x (HSHND.find li x)
               )
               else ( 
                 f x root_tree_node
               )
             );
             printf "iter end\n";
          ) (G.nodes g)
    );  

    NODE_TREE_BUILDER.root = root_tree_node;

    NODE_TREE_BUILDER.size = (G.nnodes g);
  }


(*gao lee algorithm for loops*)
let loops_gao_lee g dfs = 
  let nnodes = G.nnodes g in
  let rec init_list n =
    if n = 0 then [0] else n::(init_list (n-1)) in 
  let uf = UFSI.init (init_list (nnodes+nnodes/2+1)) in
  let res_loops = Array.create (nnodes+nnodes/2+2) (nnodes+nnodes/2+2) in
  let processed = Array.create (nnodes+1) false in
  let dm = DOMM.create g dfs in
  let loops    = Array.create (nnodes+nnodes/2+1) [] in 
  let cur      = ref nnodes in
  let process  = Array.create ((nnodes*3)/2+1) false in
  let is_in_subtree hd nd=
    let hd_node = (dfs.DFST.pre'1 hd) in 
    let nd_node = (dfs.DFST.pre'1 nd) in
    let hd1 = (dfs.DFST.post hd_node) in 
    let nd1 = (dfs.DFST.post nd_node) in
  (*  printf "hd (%i) nd (%i) hd1 (%i) nd1 (%i)" hd nd hd1 nd1; *)
    ( hd <= nd && hd1 <= nd1) in  
  let create_node nd = 
    printf "    create node rep for %i \n" nd;
    printf "    cur =  %i \n" !cur;
    loops.(!cur) <- List.map (fun x -> printf "      incoming from %i \n"( UFSI.find  (dfs.DFST.pre (G.src x)) uf);UFSI.find (dfs.DFST.pre (G.src x)) uf ) (G.ins (dfs.DFST.pre'1 nd));
    cur:=!cur+1;
    !cur - 1 in
  let collapse body header =
    printf "Collapse for node %i with:\n" header ;
    List.iter ( fun x -> printf "    %i.\n" x ;process.(x)<-false; res_loops.(x) <- header; UFSI.union header x uf ) body;
    printf "    Incoming edges for new node come from: ";
    List.iter (fun x -> printf "%i," x) loops.(header);
    printf ".\n";
   in
  let find_loop hdr work_list =
    List.iter ( fun x -> process.(x) <- true ) work_list;
    if work_list <> []  then (
      printf "Work list not empty\n";
      let loop_rep = create_node hdr in
      let loop_body = [hdr] in 
        process.(hdr)<-true;
        let rec process_work_list lb = function
          | [] -> lb
          | hd::tl_wl -> (
            let lb = hd::lb in
            printf "    element %i " hd;
            if hd < nnodes then (printf "pocessed.\n";processed.(hd) <- true) else printf "\n";
            let precedors =
              if hd >= nnodes 
                then List.map (fun x -> UFSI.find x uf) loops.(hd)   
                else List.map (fun x -> UFSI.find (dfs.DFST.pre (G.src x)) uf) (G.ins (dfs.DFST.pre'1 hd)) in
            let for_func wl in_node =
              if in_node < nnodes && not (is_in_subtree hdr in_node) then (
                if List.for_all (fun x -> x<>in_node) loops.(loop_rep) then
                  loops.(loop_rep) <- (in_node::loops.(loop_rep) );
                wl
              ) else (
                if not process.(in_node) then (
                  printf "        predok added to work list %i\n " in_node;
                  process.(in_node) <- true;
                  in_node::wl )
                else wl
              ) in
            let wl = List.fold_left for_func tl_wl precedors in
            process_work_list lb wl
        ) in
        let lp_body = process_work_list loop_body work_list in
        collapse lp_body loop_rep;
        printf "    collapse done\n";
    ) in
  let f1 f_not nd = 
    if f_not true then 
      printf "red   find for node %i\n" nd 
    else
      printf "irred find for node %i\n" nd ;
    let w = List.fold_left (
              fun lst el_eg ->
                let el_nd = G.src el_eg in
                let el_nd_n = dfs.DFST.pre el_nd in
                printf "     incoming from %i\n" el_nd_n;
                if dfs.DFST.sort el_eg == DFST.Back &&
                   f_not (dm.DOMM.INT_TREE_BUILDER.is_child el_nd_n nd )  
                  then (UFSI.find el_nd_n uf)::lst 
                  else lst 
            ) [] (G.ins (dfs.DFST.pre'1 nd)) in
    find_loop nd w in
  let iter_levels f_1 f_2 = 
    let rec iter_levels_up_f lst =
      let new_lst = List.fold_left (fun ls el_ls -> ls@(dm.DOMM.INT_TREE_BUILDER.get_childs el_ls)) [] lst in
      match new_lst with
      | [] -> ( List.iter (fun nd -> f_1 (nd) ) lst;
                List.iter (fun nd -> f_2 (nd) ) lst; );
      | _  -> ( iter_levels_up_f new_lst; 
                List.iter (fun nd -> f_1 (nd) ) lst;
                List.iter (fun nd -> f_2 (nd) ) lst; ) in
      iter_levels_up_f [0] in
  iter_levels (f1 (fun x -> x)) (f1 (fun x -> not x));
  INT_TREE_BUILDER.create  
  {
    INT_TREE_BUILDER.iter = 
    ( fun f ->
        for i = 0 to !cur - 1 do
          f i res_loops.(i)
        done
    );  

    INT_TREE_BUILDER.root = nnodes+nnodes/2+2;

    INT_TREE_BUILDER.size = !cur - 1;
  }


end 
































