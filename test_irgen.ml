(* test_irgen.ml *)

open Ast
open Sast
open Irgen
open Llvm

let run_test name program =
  try
    let m = translate program in
    let ir = Llvm.string_of_llmodule m in
    print_endline (name ^ ": IR was successful");
    print_endline ir
  with e ->
    print_endline (name ^ ": IR generation failed: " ^ Printexc.to_string e)

(* Test 1: Empty program *)
let test1 () =
  print_endline "Test 1: Empty program";
  run_test "Empty program" ([], [])

(* Test 2: Single function that returns 0 *)
let test2 () =
  let dummy_func = {
    srtyp   = Int;
    sfname  = "main";
    sformals = [];
    slocals  = [];
    sbody   = [ SReturn (Int, SLiteral 0) ];
  } in
  print_endline "Test 2: Single function return 0";
  run_test "Return 0" ([], [dummy_func])

let test3 () =
  let note_value = { pitch = "c"; octave = 4; length = 4 } in
  let dummy_func = {
    srtyp    = Int;
    sfname   = "notes";
    sformals = [];
    slocals  = [];
    sbody    = [ SExpr (Note, SNoteLit note_value); SReturn (Int, SLiteral 0) ];
  } in
  print_endline "Test 3: Function with a note literal";
  run_test "Note literal" ([], [dummy_func])

  let test4 () =
    let notes = [
      { pitch = "c";  octave = 4; length = 4 };
      { pitch = "d#"; octave = 5; length = 2 };
      { pitch = "bb"; octave = 3; length = 1 };
      { pitch = "r";  octave = 0; length = 4 };
    ] in
    let exprs = List.map (fun n -> SExpr (Note, SNoteLit n)) notes in
    let dummy_func = {
      srtyp    = Int;
      sfname   = "melody";
      sformals = [];
      slocals  = [];
      sbody    = exprs @ [ SReturn (Int, SLiteral 0) ];
    } in
    print_endline "Test 4: Function with multiple notes and varying octaves";
    run_test "Melody" ([], [dummy_func])

let _ =
  test1 (); test2 (); test3(); test4()
