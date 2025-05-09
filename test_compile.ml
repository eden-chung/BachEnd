open Ast
open Semant
open Irgen

let () =
  let notes = [
    { pitch = "c"; octave = 4; length = 4 };
    { pitch = "c"; octave = 4; length = 4 };
    { pitch = "c"; octave = 4; length = 4 };
    { pitch = "c"; octave = 4; length = 4 };
  ] in

  let exprs = List.map (fun n -> Expr(NoteLit n)) notes in

  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [];
    body = exprs @ [ Return (Literal 0) ]
  } in

  let prog : program = ([], [dummy_func]) in

  let sast = Semant.check prog in

  let lilypond_code = Irgen.translate sast in

  let output_file = "output.ly" in
  let oc = open_out output_file in
  output_string oc lilypond_code;
  close_out oc;

  let cmd = Printf.sprintf "lilypond -o output %s" output_file in
  let _ = Sys.command cmd in

  print_endline "✅ PDF successfully generated as output.pdf"
