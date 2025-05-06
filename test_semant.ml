(* test_semant.ml *)

open Ast
open Semant

(* Helper function to run a semantic check and print results *)
let run_test program =
  try
    let _ = check program in
    print_endline "Semantic check passed ✅"
  with Failure msg ->
    print_endline ("Semantic check failed ❌: " ^ msg)

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
  let program = ([], [dummy_func]) in
  print_endline "Test 1: Valid program with a simple function";
  run_test program

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
  let program = ([], [dummy_func]) in
  print_endline "Test 2: Invalid program with a type mismatch in assignment";
  run_test program

(* Test 3: Valid program with a repeat loop *)
let test3 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "i")];
    body = [
      Repeat(Literal 3, Block [
        Expr(Assign("i", Binop(Id "i", Add, Literal 1)))
      ]);
      Return (Literal 0)
    ]
  } in
  let program = ([], [dummy_func]) in
  print_endline "Test 3: Valid program with a repeat loop";
  run_test program

(* Test 4: Invalid program with a non-integer repeat expression *)
let test4 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [];
    body = [
      Repeat(BoolLit true, Block []); (* Invalid: repeat requires an integer expression *)
      Return (Literal 0)
    ]
  } in
  let program = ([], [dummy_func]) in
  print_endline "Test 4: Invalid program with a non-integer repeat expression";
  run_test program

(* Test 5: Valid program with an if-else statement *)
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
  let program = ([], [dummy_func]) in
  print_endline "Test 5: Valid program with an if-else statement";
  run_test program

(* Test 6: Invalid program with a non-boolean if condition *)
let test6 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
      If(Literal 5, (* Invalid: if condition must be a boolean *)
        Expr(Assign("x", Literal 1)),
        Expr(Assign("x", Literal 0)));
      Return (Id "x")
    ]
  } in
  let program = ([], [dummy_func]) in
  print_endline "Test 6: Invalid program with a non-boolean if condition";
  run_test program

(* Test 7: Valid program with a while loop *)
let test7 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
        Expr(Assign("x", Literal 0));
      While(Binop(Id "x", Less, Literal 5),
        Expr(Assign("x", Binop(Id "x", Add, Literal 1))));
      Return (Id "x")
    ]
  } in
  let program = ([], [dummy_func]) in
  print_endline "Test 7: Valid program with a while loop";
  run_test program

(* Test 8: Invalid program with a non-boolean while condition *)
let test8 () =
  let dummy_func = {
    rtyp = Int;
    fname = "main";
    formals = [];
    locals = [(Int, "x")];
    body = [
      While(Literal 5, (* Invalid: while condition must be a boolean *)
        Expr(Assign("x", Binop(Id "x", Add, Literal 1))));
      Return (Id "x")
    ]
  } in
  let program = ([], [dummy_func]) in
  print_endline "Test 8: Invalid program with a non-boolean while condition";
  run_test program

(* Run all tests *)
let _ =
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  test7 ();
  test8 ()