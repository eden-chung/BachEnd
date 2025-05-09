(* test_semant_irgen.ml *)

open Ast
open Semant
open Irgen
open Llvm

(* Helper function to run semantic check and IR generation, then print results *)
let run_test name program =
  print_endline ("Test " ^ name);
  try
    (* Semantic analysis *)
    let sast_prog = check program in
    print_endline "  Semantic check passed ✅";
    (* IR generation *)
    let llmod = translate sast_prog in
    let ir = string_of_llmodule llmod in
    print_endline "  IR generation passed ✅";
    print_endline "--- Generated LLVM IR ---";
    print_endline ir;
    print_endline "--- End of IR ---";
  with Failure msg ->
    print_endline ("  Test failed ❌: " ^ msg)

(* Test 1: Valid program with a simple function *)
let test1 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [(Int, "x")];
    locals = [(Int, "y")];
    body = [
      Expr(Assign("y", Literal 5));
      Return (Id "x")
    ]
  } in
  run_test "1: simple function" ([], [dummy_func])

(* Test 2: Invalid program with a type mismatch in assignment *)
let test2 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [(Int, "x")];
    locals = [(Int, "y")];
    body = [
      Expr(Assign("y", BoolLit true)); (* Type mismatch: assigning bool to int *)
      Return (Id "x")
    ]
  } in
  run_test "2: type mismatch assignment" ([], [dummy_func])

(* Test 3: Valid program with a repeat loop *)
let test3 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "i")];
    body = [
      Repeat(Literal 3, Block [ Expr(Assign("i", Binop(Id "i", Add, Literal 1))) ]);
      Return (Literal 0)
    ]
  } in
  run_test "3: repeat loop" ([], [dummy_func])

(* Test 4: Invalid program with non-integer repeat expression *)
let test4 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [];
    body = [
      Repeat(BoolLit true, Block []); (* Invalid: repeat requires integer *)
      Return (Literal 0)
    ]
  } in
  run_test "4: non-int repeat" ([], [dummy_func])

(* Test 5: Valid program with if-else *)
let test5 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
      If(Binop(Literal 5, Less, Literal 10),
        Expr(Assign("x", Literal 1)),
        Expr(Assign("x", Literal 0)));
      Return (Id "x")
    ]
  } in
  run_test "5: if-else" ([], [dummy_func])

(* Test 6: Invalid program with non-boolean if condition *)
let test6 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
      If(Literal 5, (* Invalid: if condition must be boolean *)
        Expr(Assign("x", Literal 1)),
        Expr(Assign("x", Literal 0)));
      Return (Id "x")
    ]
  } in
  run_test "6: non-bool if" ([], [dummy_func])

(* Test 7: Valid program with a while loop *)
let test7 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
      Expr(Assign("x", Literal 0));
      While(Binop(Id "x", Less, Literal 5), Expr(Assign("x", Binop(Id "x", Add, Literal 1))));
      Return (Id "x")
    ]
  } in
  run_test "7: while loop" ([], [dummy_func])

(* Test 8: Invalid program with non-boolean while condition *)
let test8 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
      While(Literal 5, (* Invalid: while condition must be boolean *) Expr(Assign("x", Binop(Id "x", Add, Literal 1))));
      Return (Id "x")
    ]
  } in
  run_test "8: non-bool while" ([], [dummy_func])

(* Test 9: Valid program with a single note literal *)
let test9 () =
  let note_value = { pitch = "c"; octave = 4; length = 4 } in
  let dummy_func = {
    rtyp = Int;
    fname = "notes";
    formals = [];
    locals = [];
    body = [
      Expr(NoteLit note_value);
      Return (Literal 0)
    ]
  } in
  run_test "Note literal" ([], [dummy_func])

(* Test 10: Valid program with multiple notes and varying octaves *)
let test10 () =
  let notes = [
    { pitch = "c";  octave = 4; length = 4 };
    { pitch = "d#"; octave = 5; length = 2 };
    { pitch = "bb"; octave = 3; length = 1 };
    { pitch = "r";  octave = 0; length = 4 };
  ] in
  let exprs = List.map (fun n -> Expr(NoteLit n)) notes in
  let dummy_func = {
    rtyp = Int;
    fname = "melody";
    formals = [];
    locals = [];
    body = exprs @ [ Return (Literal 0) ]
  } in
  run_test "Melody" ([], [dummy_func])

(* Execute all tests *)
let () =
  test1 (); test2 (); test3 (); test4 ();
  test5 (); test6 (); test7 (); test8 (); test9 (); test10 ();
