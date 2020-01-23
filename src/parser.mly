%token <int>    INT
%token <string> IDENT

%token MINUS
%token DOT
%token COMMA
%token COLON
%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN

%token START_FUNCTION
%token END_FUNCTION

%token TY_VOID
%token TY_INT
%token TY_FLOAT

%token INT_LIST
%token FLOAT_LIST

%token ASSIGN

%token ADD
%token SUB
%token MULT
%token DIV
%token AND
%token OR

%token GOTO

%token BREQ
%token BRNEQ
%token BRLT
%token BRGT
%token BRGEQ
%token BRLEQ

%token RETURN

%token CALL
%token CALLR

%token ARRAY_STORE
%token ARRAY_LOAD

%token EOF

%{
open Ir
%}

%start<Ir.program> program
%%

program:
  | EOF                      { [] }
  | f = func; rest = program { f::rest }
;

func:
  | START_FUNCTION; hd = header; data = dataSegment; body = codeSegment; END_FUNCTION
    {
      let (name, returnType, params) = hd in
      { name; returnType; params; data; body }
    }
;

header:
  | TY_VOID; id = IDENT; LEFT_PAREN; p = params; RIGHT_PAREN; COLON
    { (id, None, p) }
  | ty = irType; id = IDENT; LEFT_PAREN; p = params; RIGHT_PAREN; COLON
    { (id, Some ty, p) }
;

params:
  | 
    { [] }
  | ty = irType; id = IDENT
    { [(id, ty)] }
  | ty = irType; id = IDENT; COMMA; rest = params
    { (id, ty)::rest }
;

irType:
  | ty = irType; LEFT_BRACKET; i = INT; RIGHT_BRACKET
    { TyArray (ty, i) }
  | TY_INT   { TyInt }
  | TY_FLOAT { TyFloat }
;

dataSegment:
  | INT_LIST; COLON; ints = dataNames; FLOAT_LIST; COLON; floats = dataNames
    { { intList = ints; floatList = floats } }
;

dataNames:
  |
    { [] }
  | d = dataName
    { [d] }
  | d = dataName; COMMA; rest = dataNames
    { d::rest }
;

dataName:
  | id = IDENT
    { Scalar id }
  | id = IDENT; LEFT_BRACKET; size = INT; RIGHT_BRACKET
    { Array (id, size) }

codeSegment:
  |
    { [] }
  | i = instr; rest = codeSegment
    { i::rest }
;

instr:
  | id = IDENT; COLON
    { Label id }

  | ASSIGN; COMMA; id = IDENT; COMMA; op = operand
    { Assign (id, op) }

  | ADD; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Add (id, op1, op2) }
  | SUB; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Sub (id, op1, op2) }
  | MULT; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Mult (id, op1, op2) }
  | DIV; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Div (id, op1, op2) }
  | AND; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { And (id, op1, op2) }
  | OR; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Or (id, op1, op2) }

  | GOTO; COMMA; id = IDENT
    { Goto id }

  | BREQ; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Breq (id, op1, op2) }
  | BRNEQ; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Brneq (id, op1, op2) }
  | BRLT; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Brlt (id, op1, op2) }
  | BRGT; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Brgt (id, op1, op2) }
  | BRGEQ; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Brgeq (id, op1, op2) }
  | BRLEQ; COMMA; id = IDENT; COMMA; op1 = operand; COMMA; op2 = operand
    { Brleq (id, op1, op2) }
  
  | RETURN; COMMA; op = operand
    { Return op }

  | CALL; COMMA; fn = IDENT
    { Call (fn, []) }
  | CALL; COMMA; fn = IDENT; COMMA; a = args
    { Call (fn, a) }

  | CALLR; COMMA; id = IDENT; COMMA; fn = IDENT
    { Callr (id, fn, []) }
  | CALLR; COMMA; id = IDENT; COMMA; fn = IDENT; COMMA; a = args
    { Callr (id, fn, a) }

  | ARRAY_STORE; COMMA; op = operand; COMMA; arr = IDENT; COMMA; i = INT
    { ArrayStore (op, arr, i) }
  | ARRAY_LOAD; COMMA; id = IDENT; COMMA; arr = IDENT; COMMA; i = INT
    { ArrayLoad (id, arr, i) }
  | ASSIGN; COMMA; arr = IDENT; COMMA; size = INT; COMMA; value = operand
    { ArrayAssign (arr, size, value) }
;

operand:
  | i = INT
    { Int i }
  | MINUS; i = INT
    { Int (-i) }
  | i = INT; DOT
    { Float (float_of_int i) }
  | DOT; p = INT
    { Float (float_of_string ("." ^ string_of_int p)) }
  | i = INT; DOT; p = INT
    { Float (float_of_string (string_of_int i ^ "." ^ string_of_int p)) }
  | MINUS; i = INT; DOT; p = INT
    { Float (float_of_string ("-" ^ string_of_int i ^ "." ^ string_of_int p)) }
  | id = IDENT
    { Ident id }
;

args:
  | o = operand
    { [o] }
  | o = operand; COMMA; rest=args
    { o::rest }
