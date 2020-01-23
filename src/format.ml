open Ir

let string_of_dataName = function
  | Scalar s -> s
  | Array (s, i) -> Printf.sprintf "%s[%d]" s i

let string_of_dataSegment = function
  {intList; floatList} ->
    let stringifyList list =
      List.map string_of_dataName list |> String.concat ", " in
    let ints = stringifyList intList
    and floats = stringifyList floatList in
    let intsText = match ints with
      | "" -> "int-list:"
      | list -> "int-list: " ^ list
    and floatsText = match floats with
      | "" -> "float-list:"
      | list -> "float-list: " ^ list in
    intsText ^ "\n" ^ floatsText

let string_of_operand = function
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Ident id -> id

let string_of_label label = label

let rec string_of_irType = function
  | TyInt -> "int"
  | TyFloat -> "float"
  | TyArray (ty, size) ->
    Printf.sprintf "%s[%d]" (string_of_irType ty) size

let string_of_instr = function
  | Label lbl ->
    lbl ^ ":"

  | Assign (var, value) ->
    Printf.sprintf "assign, %s, %s" var (string_of_operand value)
  | Add (var, op1, op2) ->
    Printf.sprintf "add, %s, %s, %s" var (string_of_operand op1) (string_of_operand op2)
  | Sub (var, op1, op2) ->
    Printf.sprintf "sub, %s, %s, %s" var (string_of_operand op1) (string_of_operand op2)
  | Mult (var, op1, op2) ->
    Printf.sprintf "mult, %s, %s, %s" var (string_of_operand op1) (string_of_operand op2)
  | Div (var, op1, op2) ->
    Printf.sprintf "div, %s, %s, %s" var (string_of_operand op1) (string_of_operand op2)
  | And (var, op1, op2) ->
    Printf.sprintf "and, %s, %s, %s" var (string_of_operand op1) (string_of_operand op2)
  | Or (var, op1, op2) ->
    Printf.sprintf "or, %s, %s, %s" var (string_of_operand op1) (string_of_operand op2)

  | Goto lbl ->
    Printf.sprintf "goto, %s" lbl

  | Breq (lbl, op1, op2) ->
    Printf.sprintf "breq, %s, %s, %s" lbl (string_of_operand op1) (string_of_operand op2)
  | Brneq (lbl, op1, op2) ->
    Printf.sprintf "brneq, %s, %s, %s" lbl (string_of_operand op1) (string_of_operand op2)
  | Brlt (lbl, op1, op2) ->
    Printf.sprintf "brlt, %s, %s, %s" lbl (string_of_operand op1) (string_of_operand op2)
  | Brgt (lbl, op1, op2) ->
    Printf.sprintf "brgt, %s, %s, %s" lbl (string_of_operand op1) (string_of_operand op2)
  | Brgeq (lbl, op1, op2) ->
    Printf.sprintf "brgeq, %s, %s, %s" lbl (string_of_operand op1) (string_of_operand op2)
  | Brleq (lbl, op1, op2) ->
    Printf.sprintf "brleq, %s, %s, %s" lbl (string_of_operand op1) (string_of_operand op2)

  | Return op ->
    Printf.sprintf "return, %s" (string_of_operand op)

  | Call (fn, []) ->
    Printf.sprintf "call, %s" fn
  | Call (fn, args) ->
    Printf.sprintf "call, %s, %s" fn (List.map string_of_operand args |> String.concat ", ")
  | Callr (ret, fn, []) ->
    Printf.sprintf "callr, %s, %s" ret fn
  | Callr (ret, fn, args) ->
    Printf.sprintf "callr, %s, %s, %s" ret fn (List.map string_of_operand args |> String.concat ", ")

  | ArrayStore (var, arr, idx) ->
    Printf.sprintf "array_store, %s, %s, %d" (string_of_operand var) arr idx
  | ArrayLoad (var, arr, idx) ->
    Printf.sprintf "array_load, %s, %s, %d" var arr idx
  | ArrayAssign (arr, size, value) ->
    Printf.sprintf "assign, %s, %d, %s" arr size (string_of_operand value)

let string_of_func = function
  {name; returnType; params; data; body} ->
    let header =
      let string_of_returnType = function
        | None -> "void"
        | Some ty -> string_of_irType ty in
      let string_of_params params =
        List.map (fun (id, ty) -> string_of_irType ty ^ " " ^ id) params |> String.concat ", " in
      Printf.sprintf "%s %s(%s):" (string_of_returnType returnType) name (string_of_params params)
    and dataSegment = string_of_dataSegment data
    and codeSegment =
      let indentAndFormat = function
        | Label lbl -> string_of_instr (Label lbl)
        | instr -> "    " ^ string_of_instr instr in
      List.map indentAndFormat body |> String.concat "\n" in
    "#start_function\n" ^
    header ^ "\n" ^
    dataSegment ^ "\n" ^
    codeSegment ^ "\n" ^
    "#end_function"

let string_of_program program =
  List.map string_of_func program |> String.concat "\n\n"
