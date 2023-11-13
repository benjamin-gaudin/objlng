open Format
open Lexing

let file = Sys.argv.(1)

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  try
    let prog = Objlngparser.program Objlnglexer.token lb in
    close_in c;
    let tprog = Objlngtyper.type_program prog in
    let flatten = Preprocessing.preprocessing_program tprog in
    let imp = Objlng2imp.translate_program flatten in
    let imp_output_file = (Filename.chop_suffix file ".obj") ^ ".imp" in
    let imp_out = open_out imp_output_file in
    let imp_outf = formatter_of_out_channel imp_out in
    Imppp.print_program imp_outf imp;
    pp_print_flush imp_outf ();
    close_out imp_out;
    let asm = Imp2mips.translate_program imp in
    let output_file = (Filename.chop_suffix file ".obj") ^ ".asm" in
    let out = open_out output_file in
    let outf = formatter_of_out_channel out in
    Mips.print_program outf asm;
    pp_print_flush outf ();
    close_out out;
    exit 0
  with
    | Objlnglexer.Error s ->
       report (lexeme_start_p lb, lexeme_end_p lb);
       eprintf "lexical error: %s@." s;
       exit 1
    | Objlngparser.Error ->
       report (lexeme_start_p lb, lexeme_end_p lb);
       eprintf "syntax error@.";
       exit 1
    | e ->
       eprintf "Anomaly: %s\n@." (Printexc.to_string e);
       exit 2
