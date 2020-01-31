{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
  }
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let int = ['0'-'9'] ['0'-'9']*

let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; EOL }

  | int as i    { INT i }

  | '.' { DOT }
  | ',' { COMMA }
  | ':' { COLON }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }

  | "#start_function" { START_FUNCTION }
  | "#end_function"   { END_FUNCTION }

  | "void"  { TY_VOID }
  | "int"   { TY_INT }
  | "float" { TY_FLOAT }

  | "int-list"   { INT_LIST }
  | "float-list" { FLOAT_LIST }

  | "assign" { ASSIGN }

  | "add"    { ADD }
  | "sub"    { SUB }
  | "mult"   { MULT }
  | "div"    { DIV }
  | "and"    { AND }
  | "or"     { OR }

  | "goto"   { GOTO }

  | "breq"   { BREQ }
  | "brneq"  { BRNEQ }
  | "brlt"   { BRLT }
  | "brgt"   { BRGT }
  | "brgeq"  { BRGEQ }
  | "brleq"  { BRLEQ }

  | "return" { RETURN }

  | "call"   { CALL }
  | "callr"  { CALLR }

  | "array_store" { ARRAY_STORE }
  | "array_load"  { ARRAY_LOAD }

  (* These need to come later to avoid conflicts *)
  | ident as id { IDENT id }
  | '-' { MINUS }

  | eof       { EOF }
  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }
