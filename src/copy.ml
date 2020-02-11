open Ir
open Cfg
open Analysis

type t = int * string * string * bool
module VSet = Set.Make(struct
  type t = int * string * string * bool
  let compare = compare
end)

(* Get all defs in vertex v *)
let def v =
  let n, instrs = G.V.label v in
  let def_instr set = function
    | Assign (dst, Ident op) -> VSet.add (n, dst, op, true) set
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
    | ArrayAssign (dst, _, _) -> VSet.add (n, dst, "", false) set
    | _ -> set in
  List.fold_left def_instr VSet.empty instrs

(* Find defs with given name *)
let defs vars name =
  VSet.filter (fun (_, var, _, _) -> var = name) vars

(* Filter out defs that are not Assigns *)
let is_assign = function
  | (_, _, _, true) -> true
  | _ -> false

(* Find all assignments of the form name := _ or _ := name *)
let assigns vars name =
  VSet.filter (fun ((_, x, y, _) as i) -> is_assign i && (x = name || y = name)) vars

let hashtbl_of_vset vset = 
  let hashtbl = Hashtbl.create (VSet.cardinal vset) in
  VSet.iter (fun (_, x, y, _) -> Hashtbl.add hashtbl x y) vset;
  hashtbl

let init_fold vars v sets =
  let def_set = def v in
  let defs_for_vars =
    let defed_vars = VSet.elements def_set in
    List.fold_left (fun set (_, var, _, _) -> VSet.union (assigns vars var) set) VSet.empty defed_vars in
  let vset = {
    gen_set = VSet.filter is_assign def_set;
    kill_set = VSet.diff defs_for_vars def_set;
    in_set = VSet.empty;
    out_set = VSet.empty;
  } in
  VMap.add v vset sets

let solve_traverse (g, entry) sets =
  let visited = Hashtbl.create (G.nb_vertex g) in
  (* Vars in scope *)
  (* let defined_vars = Hashtbl.create (Cfg.nb_vertex g) in *)
  let rec traverse sets node =
    (* if node has not been visited *)
    if not (Hashtbl.mem visited node) then begin
      (* Visit node *)
      Hashtbl.add visited node ();
      (* Get node's gen and kill set (these are constant) *)
      let { gen_set; kill_set; _ } = VMap.find node sets
      (* Get the out_set of the node's predecessors *)
      and pred_outs =
        let preds = G.pred g node in
        List.map begin fun pred ->
          let { out_set; _ } = VMap.find pred sets in
          out_set
        end preds in
      (* Local universal set is the union of the pred's outs *)
      let u_set = List.fold_left VSet.union VSet.empty pred_outs in
      (* In = /\{p in preds[v]: out[p]} *)
      let in_set = List.fold_left VSet.inter u_set pred_outs in
      (* Out = gen[v] U (in[v] - kill[v]) *)
      let out_set = VSet.union gen_set (VSet.diff in_set kill_set) in
      (* Update the data flow solutions *)
      let sets' = VMap.add node { gen_set; kill_set; in_set; out_set; } sets in
      let outgoing = G.succ g node in
      List.fold_left traverse sets' outgoing
    end else sets in
  traverse sets entry

let string_of_t elts =
  (* {...} *)
  let string_of_vset_elt elt =
    (* (line, dst, op) *)
    let line, dst, op, _ = elt in
    Printf.sprintf "(%d, %s, %s)" line dst op in
  String.concat ", " (List.map string_of_vset_elt elts)

(* In-place implementation of the copy propagation algorithm *)
let copy_prop cfg vmap =
  let checked_v = Hashtbl.create (G.nb_vertex cfg) in
  let replacements = ref [] in
  let op_list_of_string_list l =
    List.map (fun op -> Ident op) l in
  (* Get the rhs for an assignment with form x := y *)
  let rec get_rhs in_set var =
    match var with
      | Ident(var') ->
          let in_tbl = hashtbl_of_vset in_set in
          if Hashtbl.mem in_tbl var' then get_rhs in_set (Ident (Hashtbl.find in_tbl var'))
          else var
      | _ -> var in
  (* Check if the instruction is gonna be changed *)
  let change_instr in_set v =
    let ops = op_list_of_string_list (use v) in
    List.exists (fun op -> if op <> get_rhs in_set op then true else false) ops in
  (* Generate an instruction from the old instruction and ops list *)
  let gen_instr in_set v =
    let _, instrs = v in
    let make_instr = function
      | Assign(x, op) -> Assign(x, get_rhs in_set op)
      | Add(x, op1, op2) -> Add(x, get_rhs in_set op1, get_rhs in_set op2)
      | Sub(x, op1, op2) -> Sub(x, get_rhs in_set op1, get_rhs in_set op2)
      | Mult(x, op1, op2) -> Mult(x, get_rhs in_set op1, get_rhs in_set op2)
      | Div(x, op1, op2) -> Div(x, get_rhs in_set op1, get_rhs in_set op2)
      | And(x, op1, op2) -> And(x, get_rhs in_set op1, get_rhs in_set op2)
      | Or(x, op1, op2) -> Or(x, get_rhs in_set op1, get_rhs in_set op2)
      | Breq(x, op1, op2) -> Breq(x, get_rhs in_set op1, get_rhs in_set op2)
      | Brneq(x, op1, op2) -> Brneq(x, get_rhs in_set op1, get_rhs in_set op2)
      | Brlt(x, op1, op2) -> Brlt(x, get_rhs in_set op1, get_rhs in_set op2)
      | Brgt(x, op1, op2) -> Brgt(x, get_rhs in_set op1, get_rhs in_set op2)
      | Brgeq(x, op1, op2) -> Brgeq(x, get_rhs in_set op1, get_rhs in_set op2)
      | Brleq(x, op1, op2) -> Brleq(x, get_rhs in_set op1, get_rhs in_set op2)
      | Return(op) -> Return(get_rhs in_set op)
      | Call(x, op_list) ->
          let op_list' = List.map (fun op -> get_rhs in_set op) op_list in
          Call(x, op_list')
      | Callr(x, y, op_list) -> 
          let op_list' = List.map (fun op -> get_rhs in_set op) op_list in
          Callr(x, y, op_list')
      | ArrayStore(op1, x, op2) -> ArrayStore(get_rhs in_set op1, x, get_rhs in_set op2)
      | ArrayLoad(x, y, op) -> ArrayLoad(x, y, get_rhs in_set op)
      | ArrayAssign(x, y, op) -> ArrayAssign(x, y, get_rhs in_set op)
      | instr -> instr in
    if not (change_instr in_set v) then instrs
    else List.map make_instr instrs in
  (* Create a vertex with the given instruction *)
  G.iter_vertex begin fun v ->
    let id, instrs = v in
    if not (Hashtbl.mem checked_v id) then begin
      Hashtbl.add checked_v id ();
      let {in_set; _} = VMap.find v vmap in
      let instrs' = gen_instr in_set v in
      if instrs = instrs' then ()
      else
        let v' = G.V.create (id, instrs') in
        replacements := (v, v')::!replacements
    end
  end cfg;

  update_vertices cfg !replacements
