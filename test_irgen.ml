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

let _ =
  test1 (); test2 ()
