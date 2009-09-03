(*
 * DOT: basic graph printing interface.
 * Copyright (C) 2006
 * Dmitri Boulytchev, St.Petersburg State University
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

module type Info = 
  sig
    type t

    val label : t -> string  
    val attrs : t -> (string * string) list
  end

module type ExtInfo =
  sig
    include Info

    val name  : t -> string 
  end

module Empty =
  struct
    type t = unit
            
    let label _ = ""
    let attrs _ = []
    let name _ = ""
  end

module Clusters =
  struct
    type 'a t = 'a cluster list 
    and 'a cluster = Node of 'a * 'a t | Leaf of 'a
  end

module type Graph =
  sig
    module Node : ExtInfo
    module Edge :
      sig
        include Info
        
        val nodes : t -> Node.t * Node.t
      end

    include ExtInfo

    val kind : t -> [`Digraph | `Graph]
    val nodes : t -> Node.t list
    val edges : t -> Edge.t list
  end

module type ClusteredGraph = 
  sig
    include Graph

    module Cluster :
      sig
        include ExtInfo

        val nodes : t -> Node.t list
      end
  end
 
module type Sig =
  sig

    type parm

    val toDOT : parm ->  string

  end

module ClusteredPrinter (CG : ClusteredGraph) =
  struct

    type parm = CG.t * (CG.Cluster.t Clusters.t)

    open Ostap.Pretty

    let semicolon = string ";"

    let sboxed x = seq [string "["; x; string "]"]

    let quote = string "\""

    let quoted x = seq [quote; x; quote]

    let attributes label attrs =
             (List.fold_right (fun (attr, value) acc -> (seq [string attr; string "="; quoted (string value)]) :: acc
                              )
                              (("label", label) :: attrs)
                              []
             )

    let wrapped g f = function
    | [] -> string ""
    | l  -> f (g l)

    let wrappedByComma = wrapped listByComma 

    let wrappedBySemicolon = wrapped listBySemicolon

    let keyword= function
    | `Digraph ->  string "digraph"
    | `Graph   ->  string "graph"

    let shift = string  "  "

    let shifted x = seq [shift; x]

    let shSemicolonedNL p = seq [newline; shift; p; semicolon; newline]

    let header g = listBySpace [(keyword (CG.kind g));
                                 string (CG.name g);
                                 string "{";
                                 wrappedBySemicolon shSemicolonedNL (attributes (CG.label g) (CG.attrs g));
                               ]

   let cHeader c = listBySpace [ string "subgraph";
                                 string (CG.Cluster.name c);
                                 string "{";
                                 wrappedBySemicolon shSemicolonedNL (attributes (CG.Cluster.label c) (CG.Cluster.attrs c));
                               ]

    let footer = string "}"

    let node n = listBySpace [string (CG.Node.name n); wrappedByComma sboxed (attributes (CG.Node.label n) (CG.Node.attrs n))]

    let nodes = function
    | []   -> empty
    | list -> shifted (seq [vboxed (listBySemicolonBreak (List.map node list)); semicolon; newline; newline])

    let edge e =
      let src, dst = CG.Edge.nodes e in
       listBySpace [string (CG.Node.name src);
                    string "->";
                    string (CG.Node.name dst);
                    wrappedByComma sboxed (attributes (CG.Edge.label e) (CG.Edge.attrs e))
                   ]

    let edges = function
    | []   -> empty
    | list -> shifted (seq [vboxed (listBySemicolonBreak (List.map edge list)); semicolon; newline; newline;])

    let rec cluster = 
      let nodes list = listBySemicolon (List.map (fun node -> string (CG.Node.name node)) list)
      in function
    | Clusters.Leaf c        -> seq [cHeader c; nodes (CG.Cluster.nodes c); footer]
    | Clusters.Node (c, sub) -> vboxed (seq [cHeader c; break;
                                             (match (CG.Cluster.nodes c) with
                                                []    -> empty
                                              | list -> seq [shifted (nodes list) ; semicolon; break]
                                             );
                                             (clusters sub); break;
                                             footer
                                            ]
                                       )
    and clusters list = (vboxed (listBy (seq [break; break])
                                        (List.map (fun c -> shifted (cluster c)) list)
                                )
                        )
 
    let toDOTPrinter (graph, list) = 
      vboxed (seq [ header graph; newline;
                    (match list with 
                      [] -> empty
                     | _  -> seq [clusters list; newline]
                    );
                    nodes (CG.nodes graph);
                    edges (CG.edges graph);
                    footer
                  ]
             )

    let toDOT prm = toString (toDOTPrinter prm)
  end

module Printer (G : Graph) =
  struct
    type parm = G.t

    module CG =
      struct 
        include G
  
        module Cluster =
          struct
            include Empty
            let nodes _ = []
          end
         
      end

    module CP = ClusteredPrinter (CG)

    let toDOT g = CP.toDOT (g, [])
  end


