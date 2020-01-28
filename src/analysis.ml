open Ir
open Cfg

module VSet = Set.Make(struct
  type t = int * string
  let compare = compare
end)

type t = {
  gen_set: VSet.t;
  kill_set: VSet.t;
  in_set: VSet.t;
  out_set: VSet.t
}

module VMap = Map.Make(G.V)

let sets_converged =
  let same_in_out a b =
    VSet.equal a.in_set b.in_set && VSet.equal a.out_set b.out_set in
  VMap.equal same_in_out

(* TODO: Make these use sets, not lists! *)
let def v =
  let n, instrs = G.V.label v in
  let def_instr set = function
    (* Q: Should call/callr assume that params are always
          read-only? *)
    | Assign (dst, _)
    | Add (dst, _, _)
    | Sub (dst, _, _)
    | Mult (dst, _, _)
    | Div (dst, _, _)
    | And (dst, _, _)
    | Or (dst, _, _)
    | Callr (dst, _, _)
    | ArrayStore (_, dst, _)
    | ArrayLoad (dst, _, _)
    | ArrayAssign (dst, _, _) -> VSet.add (n, dst) set
    | _ -> set in
  List.fold_left def_instr VSet.empty instrs

let use v =
  let n, instrs = G.V.label v in
  let rec only_vars = function
    | (Ident s)::rest -> s::(only_vars rest)
    | _::rest -> only_vars rest
    | [] -> []
  and add_to_set set vars =
    List.fold_left (fun set v -> VSet.add (n, v) set) set vars in
  let use_instr set = function
    | Assign (_, op)
    | Return op
    | ArrayAssign (_, _, op) -> add_to_set set (only_vars [op])

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
    | ArrayStore (op1, _, op2) -> add_to_set set (only_vars [op1; op2])

    | ArrayLoad (_, op1, op2) -> add_to_set set (op1::(only_vars [op2]))
  
    | Call (_, ops)
    | Callr (_, _, ops) -> add_to_set set (only_vars ops)
    
    | _ -> set in
  List.fold_left use_instr VSet.empty instrs

let all_vars g =
  G.fold_vertex begin fun v set ->
    VSet.union (def v) set
  end g VSet.empty

let defs vars var =
  VSet.filter (fun (_, name) -> name = var) vars

let init g =
  let sets = VMap.empty in
  let vars = all_vars g in
  G.fold_vertex begin fun v sets ->
    let def_set = def v in
    let defs_for_vars =
      let defed_vars = VSet.elements def_set in
      List.fold_left (fun set (_, var) -> VSet.union (defs vars var) set) VSet.empty defed_vars in
    let vset = {
      gen_set = def_set;
      kill_set = VSet.diff defs_for_vars def_set;
      in_set = VSet.empty;
      out_set = VSet.empty
    } in
    VMap.add v vset sets
  end g sets

let rec fixpoint (g, entry) sets f =
  let sets' = f (g, entry) sets in
  if sets_converged sets' sets then
    fixpoint (g, entry) sets f
  else sets'

let solve (g, entry) sets =
  let f (g, entry) sets =
    let visited = Hashtbl.create (G.nb_vertex g) in
    let rec traverse sets node =
      if not (Hashtbl.mem visited node) then begin
        Hashtbl.add visited node ();
        let { gen_set; kill_set; _ } = VMap.find node sets
        and pred_outs =
          let preds = G.pred g node in
          List.map begin fun pred ->
            let { out_set; _ } = VMap.find pred sets in
            out_set
          end preds in
        let in_set = List.fold_left VSet.union VSet.empty pred_outs in
        let out_set = VSet.union gen_set (VSet.diff in_set kill_set) in
        let sets' = VMap.add node { gen_set; kill_set; in_set; out_set } sets in
        let outgoing = G.succ g node in
        List.fold_left traverse sets' outgoing
      end else sets in
    traverse sets entry in
  fixpoint (g, entry) sets f

let string_of_vset_elts elts =
  (* {...} *)
  let string_of_vset_elt elt =
    (* (line, variable) *)
    let id, var = elt in
    Printf.sprintf "(%d, %s)" id var in
  String.concat ", " (List.map string_of_vset_elt elts)

let string_of_vertex v vset =
  (*
    Vertex[id]
      IN = {...}
      OUT = {...}
      GEN = {...}
      KILL = {...}
  *)
  let v_id, _ = v in
  let format_set set =
    string_of_vset_elts (VSet.elements set) in
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
    string_of_vset_elts (VSet.elements set) in
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
