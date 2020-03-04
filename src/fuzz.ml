open TigerIR.Ir

type fuzzEnv = {
  ints: string list;
  floats: string list;
  intArrs: (string * int) list;
  floatArrs: (string * int) list;
  labels: string list
}

let dataSegment_of_fuzzEnv fuzzEnv =
  let intList = List.map (fun name -> Scalar name) fuzzEnv.ints
  and intArrList = List.map (fun (name, size) -> Array (name, size)) fuzzEnv.intArrs
  and floatList = List.map (fun name -> Scalar name) fuzzEnv.floats
  and floatArrList = List.map (fun (name, size) -> Array (name, size)) fuzzEnv.floatArrs in
  {
    intList = intList @ intArrList;
    floatList = floatList @ floatArrList
  }

let () = Random.self_init ()

let counter = ref 0

let uniq base_name =
  let n = !counter in
  counter := n + 1;
  base_name ^ (string_of_int n)

let pick_var list =
  let len = List.length list in
  let n = Random.int len in
  List.nth list n

let pick_int () = (Random.int 1000) - 500
let pick_float () = (Random.float 1000.) -. 500.

let pick_int_op fuzzEnv =
  if Random.bool () then
    Ident (pick_var fuzzEnv.ints)
  else Int (pick_int ())

let pick_float_op fuzzEnv =
  if Random.bool () then
    Ident (pick_var fuzzEnv.floats)
  else Float (pick_float ())

let pick_label fuzzEnv =
  let label = uniq "lbl_" in
  ({fuzzEnv with labels = label::fuzzEnv.labels}, Label label)

let pick_assign fuzzEnv =
  let ty = Random.int 4 in
  match ty with
  | 0 ->
    let dst = pick_var fuzzEnv.ints
    and src = pick_int () in
    (fuzzEnv, Assign (dst, Int src))
  | 1 ->
    let dst = pick_var fuzzEnv.floats
    and src = pick_float () in
    (fuzzEnv, Assign (dst, Float src))
  | 2 ->
    let (dst, size) = pick_var fuzzEnv.intArrs
    and src = pick_int () in
    (fuzzEnv, ArrayAssign (dst, size, Int src))
  | 3 ->
    let (dst, size) = pick_var fuzzEnv.floatArrs
    and src = pick_float () in
    (fuzzEnv, ArrayAssign (dst, size, Float src))
  | _ -> failwith "Invalid type generated"

let pick_math fuzzEnv =
  let ops = [
    (fun (dst, a, b) ->  Add (dst, a, b));
    (fun (dst, a, b) ->  Sub (dst, a, b));
    (fun (dst, a, b) -> Mult (dst, a, b));
    (fun (dst, a, b) ->  Div (dst, a, b));
    (fun (dst, a, b) ->  And (dst, a, b));
    (fun (dst, a, b) ->   Or (dst, a, b))
  ] in
  let op = pick_var ops in
  let ty = Random.int 2 in
  match ty with
  | 0 ->
    let dst = pick_var fuzzEnv.floats
    and a = pick_float_op fuzzEnv
    and b = pick_float_op fuzzEnv in
    (fuzzEnv, op (dst, a, b))
  | 1 ->
    let dst = pick_var fuzzEnv.ints
    and a = pick_int_op fuzzEnv
    and b = pick_int_op fuzzEnv in
    (fuzzEnv, op (dst, a, b))
  | _ -> failwith "Invalid type generated"

let pick_goto fuzzEnv =
  let label = pick_var fuzzEnv.labels in
  (fuzzEnv, Goto label)

let pick_branch fuzzEnv =
  let ops = [
    (fun (dst, a, b) ->  Breq (dst, a, b));
    (fun (dst, a, b) -> Brneq (dst, a, b));
    (fun (dst, a, b) ->  Brlt (dst, a, b));
    (fun (dst, a, b) ->  Brgt (dst, a, b));
    (fun (dst, a, b) -> Brgeq (dst, a, b));
    (fun (dst, a, b) -> Brleq (dst, a, b))
  ] in
  let op = pick_var ops in
  let ty = Random.int 2 in
  match ty with
  | 0 ->
    let dst = pick_var fuzzEnv.labels
    and a = pick_float_op fuzzEnv
    and b = pick_float_op fuzzEnv in
    (fuzzEnv, op (dst, a, b))
  | 1 ->
    let dst = pick_var fuzzEnv.labels
    and a = pick_int_op fuzzEnv
    and b = pick_int_op fuzzEnv in
    (fuzzEnv, op (dst, a, b))
  | _ -> failwith "Invalid type generated"

let pick_return fuzzEnv =
  let ty = Random.int 2 in
  match ty with
  | 0 ->
    let res = pick_float_op fuzzEnv in
    (fuzzEnv, Return res)
  | 1 ->
    let res = pick_int_op fuzzEnv in
    (fuzzEnv, Return res)
  | _ -> failwith "Invalid type generated"

let pick_arr_store fuzzEnv =
  let ty = Random.int 2 in
  match ty with
  | 0 ->
    let src = pick_float_op fuzzEnv
    and dst, _ = pick_var fuzzEnv.floatArrs
    and idx = pick_int_op fuzzEnv in
    (fuzzEnv, ArrayStore (src, dst, idx))
  | 1 ->
    let src = pick_int_op fuzzEnv
    and dst, _ = pick_var fuzzEnv.intArrs
    and idx = pick_int_op fuzzEnv in
    (fuzzEnv, ArrayStore (src, dst, idx))
  | _ -> failwith "Invalid type generated"

let pick_arr_load fuzzEnv =
  let ty = Random.int 2 in
  match ty with
  | 0 ->
    let dst = pick_var fuzzEnv.floats
    and src, _ = pick_var fuzzEnv.floatArrs
    and idx = pick_int_op fuzzEnv in
    (fuzzEnv, ArrayLoad (dst, src, idx))
  | 1 ->
    let dst = pick_var fuzzEnv.ints
    and src, _ = pick_var fuzzEnv.intArrs
    and idx = pick_int_op fuzzEnv in
    (fuzzEnv, ArrayLoad (dst, src, idx))
  | _ -> failwith "Invalid type generated"

let pick_instr fuzzEnv =
  let instr_pickers = [
    pick_label;
    pick_assign;
    pick_math;
    pick_goto;
    pick_branch;
    (*pick_return;*)
    (* don't fuzz function calls b/c it's hard & we don't wanna deal with input b/c can't auto-test *)
    pick_arr_store;
    pick_arr_load
  ] in
  let picker = pick_var instr_pickers in
  picker fuzzEnv

let fuzz_prog () =
  let make_var fuzzEnv =
    let ty = Random.int 4 in
    match ty with
    | 0 ->
      let name = uniq "int_" in
      {fuzzEnv with ints = name::fuzzEnv.ints}
    | 1 ->
      let name = uniq "float_" in
      {fuzzEnv with floats = name::fuzzEnv.floats}
    | 2 ->
      let name = uniq "int_arr_"
      and size = Random.int 100 in
      {fuzzEnv with intArrs = (name, size)::fuzzEnv.intArrs}
    | 3 ->
      let name = uniq "float_arr_"
      and size = Random.int 100 in
      {fuzzEnv with floatArrs = (name, size)::fuzzEnv.floatArrs}
    | _ -> failwith "Invalid type generated" in
  let rec setup_loop min fuzzEnv =
    if List.length fuzzEnv.ints < min
    || List.length fuzzEnv.floats < min
    || List.length fuzzEnv.intArrs < min
    || List.length fuzzEnv.floatArrs < min then
      let fuzzEnv' = make_var fuzzEnv in
      setup_loop min fuzzEnv'
    else fuzzEnv in
  let empty = {ints=[]; floats=[]; intArrs=[]; floatArrs=[]; labels=["lbl_start"; "lbl_end"]} in
  let vars = setup_loop 10 empty in

  let rec instrs_loop fuzzEnv buf =
    if Random.int 200 <> 100 then
      let fuzzEnv', instr = pick_instr fuzzEnv in
      instrs_loop fuzzEnv' (instr::buf)
    else buf in
  
  let body = (Label "lbl_start")::(instrs_loop vars [Label "lbl_end"]) in

  let main = {
    name = "main";
    returnType = None;
    params = [];
    data = dataSegment_of_fuzzEnv vars;
    body
  } in
  [main]
