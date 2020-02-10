%token <string> INT
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

%token EOL
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
  | START_FUNCTION; EOL+; hd = header; data = dataSegment; body = codeSegment; END_FUNCTION; EOL*
    {
      let (name, returnType, params) = hd in
      { name; returnType; params; data; body }
    }
;

header:
  | TY_VOID; id = ident_or_keyword; LEFT_PAREN; p = params; RIGHT_PAREN; COLON; EOL+
    { (id, None, p) }
  | ty = irType; id = ident_or_keyword; LEFT_PAREN; p = params; RIGHT_PAREN; COLON; EOL+
    { (id, Some ty, p) }
;

params:
  | 
    { [] }
  | ty = irType; id = ident_or_keyword
    { [(id, ty)] }
  | ty = irType; id = ident_or_keyword; COMMA; rest = params
    { (id, ty)::rest }
;

irType:
  | ty = irType; LEFT_BRACKET; i = int; RIGHT_BRACKET
    { TyArray (ty, i) }
  | TY_INT   { TyInt }
  | TY_FLOAT { TyFloat }
;

dataSegment:
  | INT_LIST; COLON; ints = dataNames; EOL+; FLOAT_LIST; COLON; floats = dataNames; EOL+
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
  | id = ident_or_keyword
    { Scalar id }
  | id = ident_or_keyword; LEFT_BRACKET; size = int; RIGHT_BRACKET
    { Array (id, size) }
;

codeSegment:
  |
    { [] }
  | i = instr; EOL; rest = codeSegment
    { i::rest }
;

instr:
  | id = ident_or_keyword; COLON
    { Label id }

  | ASSIGN; COMMA; id = ident_or_keyword; COMMA; op = operand
    { Assign (id, op) }

  | ADD; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Add (id, op1, op2) }
  | SUB; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Sub (id, op1, op2) }
  | MULT; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Mult (id, op1, op2) }
  | DIV; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Div (id, op1, op2) }
  | AND; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { And (id, op1, op2) }
  | OR; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Or (id, op1, op2) }

  | GOTO; COMMA; id = ident_or_keyword
    { Goto id }

  | BREQ; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Breq (id, op1, op2) }
  | BRNEQ; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Brneq (id, op1, op2) }
  | BRLT; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Brlt (id, op1, op2) }
  | BRGT; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Brgt (id, op1, op2) }
  | BRGEQ; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Brgeq (id, op1, op2) }
  | BRLEQ; COMMA; id = ident_or_keyword; COMMA; op1 = operand; COMMA; op2 = operand
    { Brleq (id, op1, op2) }
  
  | RETURN; COMMA; op = operand
    { Return op }

  | CALL; COMMA; fn = ident_or_keyword
    { Call (fn, []) }
  | CALL; COMMA; fn = ident_or_keyword; COMMA; a = args
    { Call (fn, a) }

  | CALLR; COMMA; id = ident_or_keyword; COMMA; fn = ident_or_keyword
    { Callr (id, fn, []) }
  | CALLR; COMMA; id = ident_or_keyword; COMMA; fn = ident_or_keyword; COMMA; a = args
    { Callr (id, fn, a) }

  | ARRAY_STORE; COMMA; op = operand; COMMA; arr = ident_or_keyword; COMMA; i = operand
    { ArrayStore (op, arr, i) }
  | ARRAY_LOAD; COMMA; id = ident_or_keyword; COMMA; arr = ident_or_keyword; COMMA; i = operand
    { ArrayLoad (id, arr, i) }
  | ASSIGN; COMMA; arr = ident_or_keyword; COMMA; size = int; COMMA; value = operand
    { ArrayAssign (arr, size, value) }
;

operand:
  | i = int
    { Int i }
  | MINUS; i = int
    { Int (-i) }
  | i = INT; DOT
    { Float (float_of_string i) }
  | DOT; p = INT
    { Float (float_of_string ("." ^ p)) }
  | i = INT; DOT; p = INT
    { Float (float_of_string (i ^ "." ^ p)) }
  | MINUS; i = INT; DOT; p = INT
    { Float (float_of_string ("-" ^ i ^ "." ^ p)) }
  | id = ident_or_keyword
    { Ident id }
;

args:
  | o = operand
    { [o] }
  | o = operand; COMMA; rest=args
    { o::rest }
;

ident_or_keyword:
  | i = IDENT
    { i }
  | ASSIGN
    { "assign" }
  | ADD
    { "add" }
  | SUB
    { "sub" }
  | MULT
    { "mult" }
  | DIV
    { "div" }
  | AND
    { "and" }
  | OR
    { "or" }
  | GOTO
    { "goto" }
  | BREQ
    { "breq" }
  | BRNEQ
    { "brneq" }
  | BRLT
    { "brlt" }
  | BRGT
    { "brgt" }
  | BRGEQ
    { "brgeq" }
  | BRLEQ
    { "brleq" }
  | RETURN
    { "return" }
  | CALL
    { "call" }
  | CALLR
    { "callr" }
  | ARRAY_LOAD
    { "array_load" }
  | ARRAY_STORE
    { "array_store" }
;

int:
  | i = INT
    { int_of_string i }
;
