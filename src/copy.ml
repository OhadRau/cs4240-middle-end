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
let filter_assigns = function
  | (_, _, _, true) -> true
  | _ -> false

(* TODO: May be able to move this in Analysis.Make module *)
let init_fold vars v sets =
  let def_set = def v in
  let defs_for_vars =
    let defed_vars = VSet.elements def_set in
    List.fold_left (fun set (_, var, _, _) -> VSet.union (defs vars var) set) VSet.empty defed_vars in
  let vset = {
    gen_set = VSet.filter filter_assigns def_set;
    kill_set = VSet.diff defs_for_vars def_set;
    in_set = VSet.empty;
    out_set = VSet.empty
  } in
  VMap.add v vset sets

let solve_traverse (g, entry) sets =
  let visited = Hashtbl.create (G.nb_vertex g) in
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
      (* Also need to kill defs that the input map to each other *)
      let kill_defs = Hashtbl.create 10 in
      VSet.iter begin fun e ->
        let _, _, y, _ = e in
        Hashtbl.add kill_defs y e
      end in_set;
      let in_kill_set = VSet.fold begin fun v acc ->
        let _, y, _, _ = v in
        let e = Hashtbl.find_opt kill_defs y in
        match e with
          | Some(x) -> VSet.add x acc
          | None -> acc
      end kill_set VSet.empty in
      let kill_set' = VSet.union kill_set in_kill_set in
      let out_set = VSet.union gen_set (VSet.diff in_set kill_set') in
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
