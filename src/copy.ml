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

(* Get a list of the operands in the vertex assuming single instruction *)
let ops v =
  let _, instrs = G.V.label v in
  let op = function
    | Assign(_, x)
    | Return(x) -> [x]
    | Add(_, x, y)
    | Sub(_, x, y)
    | Mult(_, x, y)
    | Div(_, x, y)
    | And(_, x, y)
    | Or(_, x, y)
    | Breq(_, x, y)
    | Brneq(_, x, y)
    | Brlt(_, x, y)
    | Brgt(_, x, y)
    | Brgeq(_, x, y)
    | Brleq(_, x, y)
    | ArrayStore(x, _, y) -> [x; y]
    | Call(_, x)
    | Callr(_, _, x) -> x
    | ArrayLoad(_, _, x)
    | ArrayAssign(_, _, x) -> [x]
    | _ -> [] in
  op (List.nth instrs 0)

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
  (* Get the rhs for an assignment with form x := y *)
  let get_rhs in_set var =
    let curr_var = ref var in
    let in_tbl = hashtbl_of_vset in_set in
    while Hashtbl.mem in_tbl !curr_var do
      curr_var := Hashtbl.find in_tbl var
    done;
    !curr_var in
  (* Generate an instruction from the old instruction and ops list *)
  let gen_instr instr ops =
    match instr with
      | Assign(x, _) -> Assign(x, (List.nth ops 0))
      | Add(x, _, _) -> Add(x, (List.nth ops 0), (List.nth ops 1))
      | Sub(x, _, _) -> Sub(x, (List.nth ops 0), (List.nth ops 1))
      | Mult(x, _, _) -> Mult(x, (List.nth ops 0), (List.nth ops 1))
      | Div(x, _, _) -> Div(x, (List.nth ops 0), (List.nth ops 1))
      | And(x, _, _) -> And(x, (List.nth ops 0), (List.nth ops 1))
      | Or(x, _, _) -> Or(x, (List.nth ops 0), (List.nth ops 1))
      | Breq(x, _, _) -> Breq(x, (List.nth ops 0), (List.nth ops 1))
      | Brneq(x, _, _) -> Brneq(x, (List.nth ops 0), (List.nth ops 1))
      | Brlt(x, _, _) -> Brlt(x, (List.nth ops 0), (List.nth ops 1))
      | Brgt(x, _, _) -> Brgt(x, (List.nth ops 0), (List.nth ops 1))
      | Brgeq(x, _, _) -> Brgeq(x, (List.nth ops 0), (List.nth ops 1))
      | Brleq(x, _, _) -> Brleq(x, (List.nth ops 0), (List.nth ops 1))
      | Return(x) -> Return(x)
      | Call(x, _) -> Call(x, ops) (* TODO: Check this *)
      | Callr(x, y, _) -> Callr(x, y, ops)
      | ArrayStore(_, x, _) -> ArrayStore((List.nth ops 0), x, (List.nth ops 1))
      | ArrayLoad(x, y, _) -> ArrayLoad(x, y, (List.nth ops 0))
      | ArrayAssign(x, y, _) -> ArrayAssign(x, y, (List.nth ops 0))
      | _ -> instr in
  (* Create a vertex with the given instruction *)
  G.iter_vertex begin fun v ->
    let id, instr = v in
    if not (Hashtbl.mem checked_v id) then begin
      Hashtbl.add checked_v id ();
      let {in_set; _} = VMap.find v vmap in
      let copies_found = ref false in
      (* For every op in the list, replace it if it has a copy *)
      let ops = List.map begin fun op ->
        match op with
          | Ident(name: string) -> begin
              copies_found := true;  
              Ident (get_rhs in_set name)
            end
          | _ -> op
      end (ops v) in
      (* If copies were propagated, then replace the vertex *)
      if !copies_found then begin
        (* Get the first element since blocks only have 1 instr *)
        let instr' = gen_instr (List.nth instr 0) ops in
        let v' = G.V.create (id, [instr']) in
        replacements := (v, v')::!replacements
      end else ()
    end
  end cfg;

  update_vertices cfg !replacements
