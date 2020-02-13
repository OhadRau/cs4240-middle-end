open Ir
open Cfg

type 'a dataflow_sets = {
  gen_set: 'a;
  kill_set: 'a;
  in_set: 'a;
  out_set: 'a
}

module VMap = Map.Make(G.V)

module type Dataflow = sig
  (* Type of an element in the VSet *)
  type t
  module VSet: Set.S with type elt = t
  (* Find all the definitions in a vertex *)
  val def: Vertex.t -> VSet.t
  (* Fold function for implementing folding the vertices to a map *)
  val init_fold: VSet.t -> Vertex.t -> (VSet.t dataflow_sets) VMap.t -> (VSet.t dataflow_sets) VMap.t
  (* Solve the data flow equations *)
  val solve_traverse: G.t * Vertex.t -> (VSet.t dataflow_sets) VMap.t -> (VSet.t dataflow_sets) VMap.t
  (* Convert t to a string *)
  val string_of_t: t list -> string
end

module Make(D: Dataflow) = struct
  module VSet = D.VSet
  type t = VSet.t dataflow_sets

  let sets_converged left right =
    let same_in_out a b =
      VSet.equal a.in_set b.in_set && VSet.equal a.out_set b.out_set in
    VMap.equal same_in_out left right

  let all_vars g =
    G.fold_vertex begin fun v set ->
      VSet.union (D.def v) set
    end g VSet.empty

  let rec fixpoint (g, entry) sets f =
    let sets' = f (g, entry) sets in
    if sets_converged sets' sets then sets
    else fixpoint (g, entry) sets' f
  
  let init g =
    let sets = VMap.empty in
    let vars = all_vars g in
    G.fold_vertex (D.init_fold vars) g sets

  let solve (g, entry) sets =
    fixpoint (g, entry) sets D.solve_traverse

  let string_of_vertex v vset =
    (*
      Vertex[id]:
        IN = {...}
        OUT = {...}
        GEN = {...}
        KILL = {...}
    *)
    let v_id, _ = v in
    let format_set set =
      D.string_of_t (VSet.elements set) in
    Printf.sprintf {|
      Vertex[%d]:
        IN = {%s}
        OUT = {%s}
        GEN = {%s}
        KILL = {%s}
    |} v_id (format_set vset.in_set) (format_set vset.out_set) (format_set vset.gen_set) (format_set vset.kill_set)
  
  let string_of_vertex_inline vset =
    (* Vertex[id]: IN={...} | OUT={...} | GEN={...} | KILL={...} *)
    let format_set set =
      D.string_of_t (VSet.elements set) in
    Printf.sprintf "(IN={%s} | OUT={%s} | GEN={%s} | KILL={%s})"
      (format_set vset.in_set) (format_set vset.out_set) (format_set vset.gen_set) (format_set vset.kill_set)

  let print_vmap vmap =
    let print_vset v vset =
      print_endline (string_of_vertex v vset) in
    VMap.iter print_vset vmap

  let render_cfg file vmap cfg =
    let module Render = RenderWith(struct
      let f v =
        let sets = string_of_vertex_inline (VMap.find v vmap) in
        let _, code = v in
        Vertex.to_string code ^ "\\n" ^ sets
    end) in
    Render.output_graph file cfg
end

let use v =
  let _, instrs = G.V.label v in
  let rec only_vars = function
    | (Ident s)::rest -> s::(only_vars rest)
    | _::rest -> only_vars rest
    | [] -> [] in
  let use_instr = function
    | Assign (_, op)
    | Return op
    | ArrayAssign (_, _, op) -> only_vars [op]

    | Add (_, op1, op2)
    | Sub (_, op1, op2)
    | Mult (_, op1, op2)
    | Div (_, op1, op2)
    | And (_, op1, op2)
    | Or (_, op1, op2)
    | Breq (_, op1, op2)
    | Brneq (_, op1, op2)
    | Brlt (_, op1, op2)
    | Brgt (_, op1, op2)
    | Brgeq (_, op1, op2)
    | Brleq (_, op1, op2)
    | ArrayStore (op1, _, op2) -> only_vars [op1; op2]

    | ArrayLoad (_, op1, op2) -> op1::(only_vars [op2])
  
    | Call (_, ops)
    | Callr (_, _, ops) -> only_vars ops
    
    | _ -> [] in
  List.map use_instr instrs |> List.concat
