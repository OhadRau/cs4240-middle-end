open Graph

(*
  Imperative vs. Persistent: whether or not the operations
  modify the underlying graph or return a copy with the
  changes

  Graph vs. Digraph: Graph is undirected, Digraph is directed

  ConcreteBidirectionalLabeled: Means that edges will be
  labelled (which is useful for fallthrough, true, false edges)
*)

module Vertex = struct
  (* id, instr *)
  type t = int * Ir.instr list

  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash

  let to_string code =
    List.map Format.string_of_instr code |> String.concat "\n"
end

module Edge = struct
  type t = [`Fallthrough | `Branch | `Jump]
  let compare = compare
  let default = `Fallthrough

  let to_string = function
    | `Fallthrough -> "fallthrough"
    | `Branch -> "branch"
    | `Jump -> "jump"
end

module G =
  Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)

(* Decision: Should we try to create single-instr basic blocks?
  + Easy to build the graph because we don't have to find all "block leaders"
  + Very simple to perform optimizations on it
  + Pretty easy to merge to larger basic blocks (1 incoming, 1 outgoing edge)
  - A little slower to do graph operations
  - Local optimizations are harder because we need to make them superlocal
*)
let build instrs =
  (* Create our graph *)
  let g = G.create () in

  (* We need each vertex to be unique in order to stop multiple vertices with
     the same code from being merged into one vertex. We can force uniqueness
     by giving each vertex a unique number *)
  let counter = ref 0 in
  let unique () =
    let u = !counter in
    counter := u + 1;
    u in

  (* Create the set of vertices *)
  let vertex_of_instr instr =
    G.V.create (unique (), [instr]) in
  let vertices = List.map vertex_of_instr instrs in
  List.iter (G.add_vertex g) vertices;

  (* Hash table to let us look up vertices by their label *)
  let vertices_by_label = Hashtbl.create 10 in

  (* Populate the hash table *)
  let populate_table vertex =
    let _, code = G.V.label vertex in
    match List.hd code with (* Get vertex contents *)
    | Ir.Label lbl -> Hashtbl.add vertices_by_label lbl vertex
    | _ -> () in
  List.iter populate_table vertices;

  let populate_edges idx vertex =
    (* Get the contents of the vertex *)
    let _, instrs = G.V.label vertex in

    (* Adds the default fallthrough edge, i.e. an edge that
       continues to the next instruction *)
    let add_fallthrough () =
      (* If there's an edge that comes next in the vertex list
         then add it *)
      match List.nth_opt vertices (idx + 1) with
      | Some next ->
        let edge = G.E.create vertex `Fallthrough next in
        G.add_edge_e g edge
      | None -> () in

    let rec last_instr = function
      | [] -> failwith "Tried to get last item in empty list"
      | [last] -> last
      | _::tail -> last_instr tail in
    (* We base the edges on the last instruction in a block since it's
       the one that edges are "coming" from *)
    match last_instr instrs with
    (* For gotos, the we always jump so the "fallthrough" edge
       becomes the target of the jump *)
    | Ir.Goto lbl ->
      let target = Hashtbl.find vertices_by_label lbl in
      let edge = G.E.create vertex `Jump target in
      G.add_edge_e g edge
    (* For branches, there are always two edges: the next instruction
       (fallthrough) and the target of the branch (whenever the condition
      is true) *)
    | Ir.Breq (lbl, _, _)
    | Ir.Brneq (lbl, _, _)
    | Ir.Brlt (lbl, _, _)
    | Ir.Brgt (lbl, _, _)
    | Ir.Brgeq (lbl, _, _)
    | Ir.Brleq (lbl, _, _) ->
      let target = Hashtbl.find vertices_by_label lbl in
      let edge = G.E.create vertex `Branch target in
      G.add_edge_e g edge;
      add_fallthrough ()
    (* Return has no outgoing edges, since it marks the end of the
       procedure *)
    | Ir.Return _ -> ()
    (* For all other instructions, we only fallthrough into the next
       instruction *)
    | _ ->
      add_fallthrough () in
  List.iteri populate_edges vertices;

  g, List.hd vertices

let dump_graph g =
  let display_vertex v =
    let num, instrs = G.V.label v in
    let formatted =
      Vertex.to_string instrs in
    Printf.printf "Vertex [%d]:\n" num;
    print_endline formatted in
  G.iter_vertex display_vertex g;

  let display_edge e =
    let src_num, _ = G.E.src e |> G.V.label
    and dst_num, _ = G.E.dst e |> G.V.label
    and label = G.E.label e |> Edge.to_string in
    Printf.printf "Edge [%d -%s-> %d]\n" src_num label dst_num in
  G.iter_edges_e display_edge g

let get_code g hd =
  (* f: g: cfg -> instr list *)
  let visited = Hashtbl.create (G.nb_vertex g) in
  let pred_ft v =
    (* Return if any of v's predecessor edges is a fallthrough *)
    List.exists (fun e -> G.E.label e = `Fallthrough) (G.pred_e g v) in
  let fold_edges e =
    (* Handle each different type of edge/dst combo *)
    let dst = G.E.dst e in
    match G.E.label e with
      | `Fallthrough -> [dst]
      | _ when G.in_degree g dst = 1 -> [dst]
      | _ when pred_ft dst -> []
      | _ -> [dst] in
  let rec traverse v =
    (* Return empty list if the vertex has been visited *)
    if Hashtbl.mem visited v then []
    else begin
      (* Mark vertex as visited *)
      Hashtbl.add visited v ();
      let _, block = v in
      (* Get list of dst vertices to follow. Find and sort all successor edges,
         fold them to get the destination vertices *)
      let dsts = G.succ_e g v |> List.sort G.E.compare
        |> List.map fold_edges |> List.concat in
      block @ (List.map traverse dsts |> List.concat)
    end in
    (* Traverse from the root *)
  traverse hd

(* Use OCamlgraph's Graphviz module to generate a DOT file, which can
   then be used to render the graph as a PDF or image. *)
module Render = Graphviz.Dot(struct
  include G
  
  let edge_attributes (_a, e, _b) =
    [`Label (Edge.to_string e);
     `Color 4711;
     `Dir `Forward]

  let default_edge_attributes _ = []

  let get_subgraph _ = None
  
  let vertex_attributes (_num, code) =
    [`Shape `Box;
     `Label (Vertex.to_string code)]
  
  let vertex_name (num, _code) = string_of_int num
  
  let default_vertex_attributes _ = []
  
  let graph_attributes _ = []
end)
