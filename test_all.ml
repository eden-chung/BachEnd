(* open Sast

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program in
  print_endline (string_of_sprogram sprogram)
  print_endline ("") *)

open Sast
open Irgen    (* your “lilypond IRgen” module *)

let () =
  (* 1) parse & semantically check *)
  let lexbuf   = Lexing.from_channel stdin in
  let program  = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program in

  (* 2) pretty-print the S-AST *)
  print_endline (string_of_sprogram sprogram);

  (*3) generate LilyPond code and print it *)
  let lilypond_code = translate sprogram in
  print_endline "--- Generated LilyPond Code ---";
  print_endline lilypond_code;
  print_endline "--- End of LilyPond Code ---"
