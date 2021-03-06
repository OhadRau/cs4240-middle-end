open TigerIR.Ir
open TigerIR.Cfg
open Analysis

type t = int * string
module VSet = Set.Make(struct
  type t = int * string
  let compare = compare
end)

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

let defs vars name =
  (* Find defs with given name *)
  VSet.filter (fun (_, var) -> var = name) vars

let init_fold vars v sets =
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

let solve_traverse (g, entry) sets =
  let visited = Hashtbl.create (G.nb_vertex g) in
  let rec traverse sets node =
    if not (Hashtbl.mem visited node) then begin
      Hashtbl.add visited node ();
      let { gen_set; kill_set; _ } = VMap.find node sets
      and pred_outs =
        let preds =
          G.pred_e g node
          |> List.filter (fun (_, lbl, _) -> lbl <> `Unreachable)
          |> List.map (fun (src, _, _) -> src) in
        List.map begin fun pred ->
          let { out_set; _ } = VMap.find pred sets in
          out_set
        end preds in
      (* In = U{p in preds[v]}. out[p] *)
      let in_set = List.fold_left VSet.union VSet.empty pred_outs in
      (* Out = gen[v] U (in[v] - kill[v]) *)
      let out_set = VSet.union gen_set (VSet.diff in_set kill_set) in
      let sets' = VMap.add node { gen_set; kill_set; in_set; out_set } sets in
      let outgoing =
        G.succ_e g node
        |> List.filter (fun (_, lbl, _) -> lbl <> `Unreachable)
        |> List.map (fun (_, _, dst) -> dst) in
      List.fold_left traverse sets' outgoing
    end else sets in
  traverse sets entry

let string_of_t elts =
  (* {...} *)
  let string_of_vset_elt elt =
    (* (line, variable) *)
    let id, var = elt in
    Printf.sprintf "(%d, %s)" id var in
  String.concat ", " (List.map string_of_vset_elt elts)

let is_critical = function
  | Label(_)
  | Goto(_)
  | Breq(_)
  | Brneq(_)
  | Brlt(_)
  | Brgt(_)
  | Brgeq(_)
  | Brleq(_)
  | Return(_)
  | Call(_)
  | Callr(_)
  | ArrayStore(_)
  | ArrayAssign(_) -> true
  | _ -> false

let collect_dead_code cfg vmap = 
  (* Collect dead code. It is the caller's responsibility to remove it. *)
  let cfg_tbl = TigerIR.Cfg.hashtbl_of_cfg cfg in
  let marked_v = Hashtbl.create (G.nb_vertex cfg) in
  let mark_and_worklist v mlist wlist =
    if not (Hashtbl.mem mlist v) then begin
      Hashtbl.add mlist v v;
      Queue.add v wlist
    end else () in
  let mark () =
    (* Mark dead code *)
    let worklist = Queue.create () in
    (* Mark all critical instructions and add them to the worklist *)
    G.iter_vertex begin fun v ->
      let _, inst = v in
      if List.exists is_critical inst then begin
        mark_and_worklist v marked_v worklist
      end else ()
    end cfg;
    (* Iterate over each element in the worklist until it is empty *)
    while not (Queue.is_empty worklist) do
      let v = Queue.pop worklist in
      (* Get the ops used by the instruction and find there reaching defs *)
      let uses = use v in
      let {in_set; _} = VMap.find v vmap in
      let reaching_def ident =
        (* Mark reaching defs and add them to the queue *)
        let reaching_defs = VSet.filter begin fun elt ->
          let _, var = elt in
          var = ident
        end in_set in
        let mark_reaching_def elt =
          (* Get vertex from id then then mark and add it to the worklist *)
          let id, _ = elt in
          let reaching_v = Hashtbl.find cfg_tbl id in
          mark_and_worklist reaching_v marked_v worklist in
        VSet.iter mark_reaching_def reaching_defs in
      List.iter reaching_def uses
    done in
  let sweep () =
    (* Generate list of dead vertices *)
    let fold_dead_code v acc =
      if Hashtbl.mem marked_v v then
        acc
      else v::acc in
    G.fold_vertex fold_dead_code cfg [] in
  mark ();
  sweep ()