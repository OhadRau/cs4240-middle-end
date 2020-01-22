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

let int = '-'? ['0'-'9'] ['0'-'9']*

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = parse
  | white     { read lexbuf }
  | newline   { next_line lexbuf; read lexbuf }

  | int as i  { INT (int_of_string i) }

  | eof       { EOF }
  | _ as c    { raise (SyntaxError ("Unexpected char or sequence: " ^ (String.make 1 c))) }
