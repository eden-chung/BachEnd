open Sast
open Irgen
open Lexing
open Printf
open Sys

let usage = "Usage: test_all.native [-o basename] [file.bach]\n\
             If no file is given, reads from stdin.\n\
             -o basename\tset output basename (default \"output\")."

let () =
  let out_base = ref "output" in
  let speclist = [
    ("-o", Arg.String (fun s -> out_base := s),
            "basename for .ly/.pdf (default \"output\")");
  ] in
  let filenames = ref [] in

  Arg.parse speclist (fun f -> filenames := f :: !filenames) usage;
  let in_chan =
    match List.rev !filenames with
    | []    -> stdin
    | [fn]  -> open_in fn
    | _     -> eprintf "%s\n" usage; exit 1
  in

  let lexbuf   = Lexing.from_channel in_chan in
  let program  = Parser.program Scanner.token lexbuf in
  let sprogram = Semant.check program in

  (* print_endline (string_of_sprogram sprogram); *)

  let lilypond_code = translate sprogram in

  let ly_file = !out_base ^ ".ly" in
  let oc = open_out ly_file in
  output_string oc lilypond_code;
  close_out oc;
  printf "Written to: %s\n%!" ly_file;

  let cmd = sprintf "lilypond -o %s %s" !out_base ly_file in
  match Sys.command cmd with
  | 0 ->
      printf "Geenerated %s.pdf\n%!" !out_base
  | code ->
      eprintf "lilypond failed (exit %d)\n%!" code;
      exit code
