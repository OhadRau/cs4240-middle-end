open Lexing

module DCE = Analysis.Make(Dead)
module COPY = Analysis.Make(Copy)

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
    (* Dead code elimination *)
    (* let vmap = DCE.init cfg |> DCE.solve (cfg, init) in *)
    (* DCE.render_cfg file vmap cfg; *)
    (* let dead_code = Dead.collect_dead_code cfg vmap in *)
    (* List.iter (fun (_, inst) -> print_endline (List.map Format.string_of_instr inst |> String.concat "\n")) dead_code; *)
    (* Cfg.remove_vertices cfg dead_code; *)
    (* Copy propagation *)
    let vmap = COPY.init cfg |> COPY.solve (cfg, init) in
    COPY.render_cfg file vmap cfg;
    COPY.print_vmap vmap;
    Cfg.Render.output_graph file cfg in
  
  List.iter print_function_cfg prog

let eval out_filename ~gen_cfg ~gen_opt_cfg prog =
  let open Ir in
  let out_file = open_out out_filename
  and basename = Filename.remove_extension out_filename in
  let print_cfg cfg ext =
    let cfg_file = open_out_bin (basename ^ ext) in
    Cfg.Render.output_graph cfg_file cfg in
  let optimize_fn fn =
    let cfg, init = Cfg.build fn.body in

    if !gen_cfg then print_cfg cfg ".dot";

    (* let vmap = DCE.init cfg |> DCE.solve (cfg, init) in *)
    (* let dead_code = Dead.collect_dead_code cfg vmap in *)
    (* Cfg.remove_vertices cfg dead_code; *)
    let vmap = COPY.init cfg |> COPY.solve (cfg, init) in
    print_cfg cfg ".dot";
    (* COPY.print_vmap vmap; *)
    let cfg_file = open_out_bin (basename ^ ".dot") in
    (* COPY.render_cfg cfg_file vmap cfg; *)
    Copy.copy_prop cfg vmap;
    Cfg.Render.output_graph cfg_file cfg;

    if !gen_opt_cfg then print_cfg cfg ".opt.dot";

    let new_init = Cfg.first_instr cfg in
    let optimized_body = Cfg.get_code cfg new_init in
    { fn with body = optimized_body } in
  let optimized_prog = List.map optimize_fn prog in
  let formatted = Format.string_of_program optimized_prog in
  output_string out_file formatted

let () =
  let in_filename = ref ""
  and out_filename = ref ""
  and gen_cfg = ref false
  and gen_opt_cfg = ref false
  and fuzz = ref false in
  let arg_spec = Arg.[
    "-i", Set_string in_filename, "Input IR file";
    "-o", Set_string out_filename, "Output IR file";
    "--fuzz", Set fuzz, "Generate random IR programs to fuzz the optimizers";
    "--gen-cfg", Set gen_cfg, "Generate a CFG for the (unoptimized) program";
    "--gen-opt-cfg", Set gen_opt_cfg, "Generate a CFG for the (optimized) program"
  ] in
  let program_name = Sys.argv.(0) in
  Arg.parse arg_spec ignore (Printf.sprintf "Usage: %s -i <input_file> -o <output_file>\n" program_name);
  if (not !fuzz) && !in_filename = "" then begin
    Printf.eprintf "Error: No input file passed given!\n";
    exit 1
  end;
  if !out_filename = "" then begin
    Printf.eprintf "Error: No output file passed given!\n";
    exit 1
  end;

  if !fuzz then begin
    let formatted = Fuzz.fuzz_prog () |> Format.string_of_program
    and out_file = open_out !out_filename in
    output_string out_file formatted
  end else
    read_file (eval !out_filename ~gen_cfg ~gen_opt_cfg) !in_filename
