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

let test_remove_vertices () =
  let display_graph name cfg =
    let file = open_out_bin name in
    Cfg.Render.output_graph file cfg in
  let g, _ = Cfg.build Ir.[
    Label "start";
      Assign ("a", Int 0);
    Label "loop";
      Brgeq ("done", Ident "a", Int 10);
      Call ("puti", [Ident "a"]);
      Add ("a", Ident "a", Int 1);
      Goto "loop";
    Label "done";
      Call ("puti", [Ident "a"])
  ] in
  let deletable = Ir.[
    Cfg.G.V.create (4, [Call ("puti", [Ident "a"])]);
    Cfg.G.V.create (5, [Add ("a", Ident "a", Int 1)])
  ] in

  display_graph "before.dot" g;
  Cfg.remove_vertices g deletable;
  display_graph "after.dot" g

let eval_verbose basename prog =
  let open Ir in
  print_endline (Format.string_of_program prog);

  let print_code ir cfg =
    let g, hd = cfg in
    let code = Cfg.get_code g hd in
    let prog' = { ir with body = code } in
    let text = Format.string_of_func prog' in
    print_endline text in

  let print_function_cfg ir =
    let cfg, init = Cfg.build ir.body in
    print_code ir (cfg, init);
    let filename = Printf.sprintf "examples/%s-%s.dot" basename ir.name in
    let file = open_out_bin filename in
    let vmap = Analysis.init cfg |> Analysis.solve (cfg, init) in
    Analysis.render_cfg file vmap cfg;
    let dead_code = Analysis.collect_dead_code cfg vmap in
    List.iter (fun (_, inst) -> print_endline (List.map Format.string_of_instr inst |> String.concat "\n")) dead_code;
    Cfg.remove_vertices cfg dead_code;
    (*Analysis.print_vmap vmap;*)
    (*Cfg.Render.output_graph file cfg*) in
  
  List.iter print_function_cfg prog

let eval basename prog =
  let open Ir in
  let out_filename = Printf.sprintf "%s-optimized.ir" basename in
  let out_file = open_out out_filename in
  let optimize_fn fn =
    let cfg, init = Cfg.build fn.body in
    let vmap = Analysis.init cfg |> Analysis.solve (cfg, init) in
    let dead_code = Analysis.collect_dead_code cfg vmap in
    Cfg.remove_vertices cfg dead_code;
    let new_init = Cfg.first_instr cfg in
    let optimized_body = Cfg.get_code cfg new_init in
    { fn with body = optimized_body } in
  let optimized_prog = List.map optimize_fn prog in
  let formatted = Format.string_of_program optimized_prog in
  output_string out_file formatted

let () =
  read_file (eval "example") "examples/example.ir";
  read_file (eval "sqrt") "examples/sqrt.ir";
  read_file (eval "quicksort") "examples/quicksort.ir"
