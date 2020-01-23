type dataName =
  | Scalar of string
  | Array of string * int

type dataSegment = {
  intList: dataName list;
  floatList: dataName list
}

type operand =
  | Int of int
  | Float of float
  | Ident of string

type label = string

type irType =
  | TyInt
  | TyFloat
  | TyArray of irType * int

type instr =
  | Label of label

  | Assign of string * operand
  | Add of string * operand * operand
  | Sub of string * operand * operand
  | Mult of string * operand * operand
  | Div of string * operand * operand
  | And of string * operand * operand
  | Or of string * operand * operand

  | Goto of label

  | Breq of label * operand * operand
  | Brneq of label * operand * operand
  | Brlt of label * operand * operand
  | Brgt of label * operand * operand
  | Brgeq of label * operand * operand
  | Brleq of label * operand * operand

  | Return of operand

  | Call of string * operand list
  | Callr of string * string * operand list

  | ArrayStore of operand * string * operand
  | ArrayLoad of string * string * operand
  | ArrayAssign of string * int * operand

type func = {
  name: string;
  returnType: irType option;
  params: (string * irType) list;
  data: dataSegment;
  body: instr list
}

(* Should this be a hashmap of string -> func? *)
type program = func list
