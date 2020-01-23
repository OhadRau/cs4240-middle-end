open Lexing

let string_of_position filename lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d"
    filename
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let read_file eval filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  match Parser.program Lexer.read lexbuf with
  | exception Lexer.SyntaxError msg ->
    Printf.fprintf stderr "Syntax error: %s at %s\n"
      (string_of_position filename lexbuf)
      msg
  | exception _ ->
    Printf.fprintf stderr "Syntax error at %s\n"
      (string_of_position filename lexbuf)
  | prog -> eval prog

let () =
  let eval basename prog =
    print_endline (Format.string_of_program prog);

    let print_function_cfg Ir.{name; body; _} =
      let cfg = Cfg.build body in
      (*Cfg.dump_graph cfg in*)
      let filename = Printf.sprintf "examples/%s-%s.dot" basename name in
      let file = open_out_bin filename in
      Cfg.Render.output_graph file cfg in
    
    List.iter print_function_cfg prog in

  read_file (eval "example") "examples/example.ir";
  read_file (eval "sqrt") "examples/sqrt.ir";
  read_file (eval "quicksort") "examples/quicksort.ir"
