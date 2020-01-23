open Graph

(*
  Imperative vs. Persistent: whether or not the operations
  modify the underlying graph or return a copy with the
  changes

  Graph vs. Digraph: Graph is undirected, Digraph is directed

  ConcreteBidirectionalLabelled: Means that edges will be
  labelled (which is useful for fallthrough, true, false edges)
*)

module Vertex = struct
  type t = Ir.instr list
  let compare = compare
  let equal = equal
  let hash = Hashtbl.hash
end

module Edge = struct
  type t = [`Fallthrough | `True | `False]
  let compare = compare
  let default = `Fallthrough
end

module G =
  Imperative.Digraph.ConcreteBidirectionalLabelled(Vertex)(Edge)

let build instrs =
  let g = G.create () in

  let v0 = G.V.create Ir.[Assign ("g", Int 0)]
  and v1 = G.V.create Ir.[Label "y"] in

  G.add_vertex g v0;
  G.add_vertex g v1;

  let e0 = G.E.create v0 `Fallthrough v1 in
  G.add_edge g e0;

  g