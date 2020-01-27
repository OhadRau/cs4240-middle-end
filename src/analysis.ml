open Ir
open Cfg

module VSet = Set.Make(G.V)

type t = {
  gen_set: VSet.t;
  kill_set: VSet.t;
  in_set: VSet.t;
  out_set: VSet.t
}

module VMap = Map.Make(G.V)

(* TODO: Make these use sets, not lists! *)
let def v =
  let _, instrs = G.V.label v in
  let def_instr = function
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
    | ArrayAssign (dst, _, _) -> [dst]
    | _ -> [] in
  List.fold_left (fun defs instr -> (def_instr instr) @ defs) [] instrs

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
  List.fold_left (fun uses instr -> (use_instr instr) @ uses) [] instrs

let init g =
  let sets = VMap.empty in
  G.fold_vertex begin fun v sets ->
    let vset = {
      gen_set = VSet.empty;
      kill_set = VSet.empty;
      in_set = VSet.empty;
      out_set = VSet.empty
    } in
    VMap.add v vset sets
  end g sets

let rec fixpoint (g, entry) sets f =
  let sets' = f (g, entry) sets in
  if sets' <> sets then
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
