open Ir
open Cfg
open Analysis

type t = int * string * string * bool
module VSet = Set.Make(struct
  type t = int * string * string * bool
  let compare (_, lhs1, rhs1, b1) (_, lhs2, rhs2, b2) = compare (lhs1, rhs1, b1) (lhs2, rhs2, b2)
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

let string_of_t elts =
  (* {...} *)
  let string_of_vset_elt elt =
    (* (line, dst, op) *)
    let line, dst, op, _ = elt in
    Printf.sprintf "(%d, %s, %s)" line dst op in
  String.concat ", " (List.map string_of_vset_elt elts)

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
    string_of_t (VSet.elements set) in
  Printf.sprintf {|
    Vertex[%d]:
      IN = {%s}
      OUT = {%s}
      GEN = {%s}
      KILL = {%s}
  |} v_id (format_set vset.in_set) (format_set vset.out_set) (format_set vset.gen_set) (format_set vset.kill_set)

let init_fold vars v sets =
  let def_set = def v in
  let defs_for_vars =
    let defed_vars = VSet.elements def_set in
    List.fold_left (fun set (_, var, _, _) -> VSet.union (assigns vars var) set) VSet.empty defed_vars in
  let gen_set = VSet.filter is_assign def_set in
  let vset = {
    gen_set;
    kill_set = VSet.diff defs_for_vars def_set;
    in_set = VSet.empty;
    out_set = gen_set
  } in
  VMap.add v vset sets

let print_set set =
  let elts = VSet.elements set
    |> List.map (fun (line, a, b, _) -> Printf.sprintf "(%d, %s, %s)" line a b) in
  Printf.printf "{%s}\n" (String.concat "," elts)

let init_in_sets (g, entry) sets =
  (* Compute the universal set of all copies in the
     function body *)
  let universal = G.fold_vertex begin fun v set ->
    VSet.union set (def v |> VSet.filter is_assign)
  end g VSet.empty in

  (* Initialize the in_set of each vertex other than the
     entry vertex to the universal set of all copies *)
  let sets = G.fold_vertex begin fun v sets ->
    if v <> entry then begin
      let old_sets = VMap.find v sets in
      let in_set = universal in
      let out_set = VSet.union old_sets.gen_set (VSet.diff in_set old_sets.kill_set) in
      VMap.add v {old_sets with in_set; out_set} sets
    end else sets
  end g sets in

  G.iter_vertex begin fun v ->
    let set = VMap.find v sets in
    print_endline (string_of_vertex v set);
  end g;

  sets

let solve_traverse (g, entry) sets =
  print_endline "------------------[RESTART]--------------------";
  let visited = Hashtbl.create (G.nb_vertex g) in
  (* Vars in scope *)
  (* let defined_vars = Hashtbl.create (Cfg.nb_vertex g) in *)
  let rec traverse sets node =
    (* if node has not been visited *)
    print_endline "travis scartt";
    if not (Hashtbl.mem visited node) then begin
      print_endline "in the mainframe";
      Printf.printf "IN[9]: ";
      VMap.iter (fun (k, _) v -> if k = 9 then print_set v.in_set) sets;
      (* Visit node *)
      Hashtbl.add visited node ();
      let (n, _) = node in
      Printf.printf "Vertex[%d]\n" n;
      (* Get node's gen and kill set (these are constant) *)
      let { gen_set; kill_set; _ } = VMap.find node sets
      (* Get the out_set of the node's predecessors *)
      and pred_outs =
        let preds =
          G.pred_e g node
          |> List.filter (fun (_, lbl, _) -> lbl <> `Unreachable)
          |> List.map (fun (src, _, _) -> src) in
        Printf.printf "There are %d reachable preds\n" (List.length preds);
        List.map begin fun pred ->
          let { out_set; _ } = VMap.find pred sets in
          out_set
        end preds in
      print_endline "send the kill set to the hitman";
      (* Local universal set is the union of the pred's outs *)
      let u_set = List.fold_left VSet.union VSet.empty pred_outs in
      print_endline "u";
      (* In = /\{p in preds[v]: out[p]} *)
      print_endline "MAKING IN SET FROM:";
      List.iter print_set pred_outs;
      let in_set = List.fold_left VSet.inter u_set pred_outs in
      print_endline "in:";
      print_set in_set;
      print_endline "??";
      (* Out = gen[v] U (in[v] - kill[v]) *)
      let out_set = VSet.union gen_set (VSet.diff in_set kill_set) in
      print_endline "out";
      (* Update the data flow solutions *)
      let sets' = VMap.add node { gen_set; kill_set; in_set; out_set } sets in
      print_endline "prime time";
      let outgoing =
        G.succ_e g node
        |> List.filter (fun (_, lbl, _) -> lbl <> `Unreachable)
        |> List.map (fun (_, _, dst) -> dst) in
      print_endline "extroverted var";
      List.fold_left traverse sets' outgoing
    end else begin
      print_endline "nah mahn----\n";
      sets
    end in
  let res = traverse sets entry in
  Printf.printf "Final IN[9]: ";
  VMap.iter (fun (k, _) v -> if k = 9 then print_set v.in_set) res;
  res

(* In-place implementation of the copy propagation algorithm *)
let copy_prop cfg vmap =
  print_endline "cooby start";
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
    print_endline "iterating over a veenis";
    let id, instrs = v in
    if not (Hashtbl.mem checked_v id) then begin
      print_endline "not in kushtbl";
      Hashtbl.add checked_v id ();
      print_endline "added kush";
      let {in_set; _} = VMap.find v vmap in
      let instrs' = gen_instr in_set v in
      if instrs = instrs' then ()
      else
        let v' = G.V.create (id, instrs') in
        replacements := (v, v')::!replacements
    end
  end cfg;
  print_endline "cooby end";
  update_vertices cfg !replacements
