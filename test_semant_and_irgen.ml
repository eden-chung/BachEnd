(* test_semant_irgen.ml *)

open Ast
(* open Sast *)
open Semant
open Irgen
open Llvm

(* Helper function to run semantic check and IR generation, then prINT results *)
(* let run_test name program =
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
    print_endline ("  Test failed ❌: " ^ msg) *)

let run_test name program =
  print_endline ("Test " ^ name);
  try
    let sast_prog = check program in
    print_endline "  Semantic check passed ✅";
    let lilypond_code = translate sast_prog in
    print_endline "  LilyPond generation passed ✅";
    print_endline "--- Generated LilyPond Code ---";
    print_endline lilypond_code;
    print_endline "--- End of LilyPond Code ---";

    (* Optional: write to file and invoke lilypond *)
    let output_file = name ^ ".ly" in
    let oc = open_out output_file in
    output_string oc lilypond_code;
    close_out oc;
    let cmd = Printf.sprintf "lilypond -o %s %s" name output_file in
    ignore (Sys.command cmd)
  with Failure msg ->
    print_endline ("  Test failed ❌: " ^ msg)

(* Test 1: Valid program with a simple function *)
let test1 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [(INT, "x")];
    locals = [(INT, "y")];
    body = [
      Expr(Assign("y", Literal 5));
      Return (Id "x")
    ]
  } in
  run_test "1: simple function" ([], [dummy_func])

(* Test 2: Invalid program with a type mismatch in assignment *)
let test2 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [(INT, "x")];
    locals = [(INT, "y")];
    body = [
      Expr(Assign("y", BoolLit true)); (* Type mismatch: assigning bool to INT *)
      Return (Id "x")
    ]
  } in
  run_test "2: type mismatch assignment" ([], [dummy_func])

(* Test 3: Valid program with a repeat loop *)
let test3 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [(INT, "i")];
    body = [
      Repeat(Literal 3, Block [ Expr(Assign("i", Binop(Id "i", ADD, Literal 1))) ]);
      Return (Literal 0)
    ]
  } in
  run_test "3: repeat loop" ([], [dummy_func])

(* Test 4: Invalid program with non-INTeger repeat expression *)
let test4 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [];
    body = [
      Repeat(BoolLit true, Block []); (* Invalid: repeat requires INTeger *)
      Return (Literal 0)
    ]
  } in
  run_test "4: non-INT repeat" ([], [dummy_func])

(* Test 5: Valid program with if-else *)
let test5 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [(INT, "x")];
    body = [
      If(Binop(Literal 5, LT, Literal 10),
        Expr(Assign("x", Literal 1)),
        Expr(Assign("x", Literal 0)));
      Return (Id "x")
    ]
  } in
  run_test "5: if-else" ([], [dummy_func])

(* Test 6: Invalid program with non-boolean if condition *)
let test6 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [(INT, "x")];
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
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [(INT, "x")];
    body = [
      Expr(Assign("x", Literal 0));
      While(Binop(Id "x", LT, Literal 5), Expr(Assign("x", Binop(Id "x", ADD, Literal 1))));
      Return (Id "x")
    ]
  } in
  run_test "7: while loop" ([], [dummy_func])

(* Test 8: Invalid program with non-boolean while condition *)
let test8 () =
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [(INT, "x")];
    body = [
      While(Literal 5, (* Invalid: while condition must be boolean *) Expr(Assign("x", Binop(Id "x", ADD, Literal 1))));
      Return (Id "x")
    ]
  } in
  run_test "8: non-bool while" ([], [dummy_func])

(* Test 9: Valid program with a single note literal *)
let test9 () =
  let note_value = { pitch = "c"; octave = 4; length = 4 } in
  let dummy_func = {
    rtyp = INT;
    fname = "main";
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
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [];
    body = exprs @ [ Return (Literal 0) ]
  } in
  run_test "Melody" ([], [dummy_func])

(* Test 11: Valid program with same note and varying octaves *)
let test11 () =
  let notes = [
    { pitch = "d";  octave = 3; length = 4 };
    { pitch = "d";  octave = 4; length = 4 };
    { pitch = "d";  octave = 5; length = 4 };
    { pitch = "d";  octave = 6; length = 4 };
    { pitch = "d";  octave = 7; length = 4 };
  ] in
  let exprs = List.map (fun n -> Expr(NoteLit n)) notes in
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [];
    body = exprs @ [ Return (Literal 0) ]
  } in
  run_test "C4 then C5" ([], [dummy_func])

  (* Test 12: Valid program with multiple notes and varying octaves *)
let test12 () =
  let notes = [
    { pitch = "c";  octave = 4; length = 4 };
    { pitch = "d";  octave = 5; length = 4 };
  ] in
  let exprs = List.map (fun n -> Expr(NoteLit n)) notes in
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [];
    body = exprs @ [ Return (Literal 0) ]
  } in
  run_test "C4 then D5" ([], [dummy_func])

    (* Test 12: Valid program with multiple notes and varying octaves *)
let test13 () =
  let notes = [
    { pitch = "c";  octave = 4; length = 4 };
    { pitch = "d";  octave = 5; length = 4 };
  ] in
  let exprs = List.map (fun n -> Expr(NoteLit n)) notes in
  let dummy_func = {
    rtyp = INT;
    fname = "main";
    formals = [];
    locals = [];
    body = exprs @ [ Return (Literal 0) ]
  } in
  run_test "C4 then D5" ([], [dummy_func])
  
  let test13_prime () =
    let note_value = { pitch = "c"; octave = 4; length = 4 } in
    let block_notes = [ note_value] in
    let exprs = List.map (fun n -> Expr(NoteLit n)) block_notes in
    let dummy_func = {
      rtyp = INT;
      fname = "main";
      formals = [];
      locals = [];
      body = exprs @ [ Return (Literal 0) ]
    } in
    run_test "13': REPEAT notes" ([], [dummy_func])
  
  let test13 () =
    let note_value = { pitch = "c"; octave = 4; length = 4 } in
    let block_notes = [ note_value ] in
    let exprs = List.map (fun n -> Expr(NoteLit n)) block_notes in
    let dummy_func = {
      rtyp = INT;
      fname = "main";
      formals = [];
      locals = [];
      body = [ Repeat(Literal 5, Block exprs); Return (Literal 0) ];
    } in
    run_test "13: REPEAT notes" ([], [dummy_func])


    (* let test_transpose_up () =
      let note = { pitch = "c"; octave = 4; length = 4 } in
      let transpose_call = Call("transpose", [Literal 1]) in
      let transpose_block =
        Block [
          Expr(NoteLit note);
          Expr(NoteLit note)
        ]
      in
      let dummy_func = {
        rtyp = INT;
        fname = "main";
        formals = [];
        locals = [];
        body = [
          Expr(transpose_call);  (* this sets up the transpose amount *)
          transpose_block;       (* this is the block to transpose *)
          Return (Literal 0)
        ]
      } in
      run_test "Transpose Up" ([], [dummy_func]) *)
(*     
      let test_transpose_up () =
        let note = { pitch = "c"; octave = 4; length = 4 } in
        let transpose_stmt =
          Transpose(Literal 1, Block [
            Expr(NoteLit note);
            Expr(NoteLit note);
            Expr(NoteLit note);
            Expr(NoteLit note)
          ])
        in
        let dummy_func = {
          rtyp = INT;
          fname = "main";
          formals = [];
          locals = [];
          body = [
            transpose_stmt;
            Return (Literal 0)
          ]
        } in
        run_test "Transpose Up" ([], [dummy_func])
      
    
     *)

     (* let test_transpose_up () =
      let note = { pitch = "c"; octave = 4; length = 4 } in
      let dummy_func = {
        rtyp   = INT;
        fname  = "main";
        formals= [];
        locals = [];
        body   = [
          Expr(Call("transpose", [Literal 1]));
          Block [
            Expr(NoteLit note);
            Expr(NoteLit note);
            Expr(NoteLit note);
            Expr(NoteLit note);
          ];
          Return(Literal 0)
        ]
      } in
      run_test "Transpose Up" ([], [dummy_func])
     *)

     let test_transpose_up () =
      let note = { pitch = "c"; octave = 4; length = 4 } in
      let dummy_func = {
        rtyp   = INT;
        fname  = "main";
        formals= [];
        locals = [];
        body   = [
          Transpose(Literal 1, Block [
            Expr (NoteLit note);
            Expr (NoteLit note);
            Expr (NoteLit note);
            Expr (NoteLit note);
          ]);
          Return (Literal 0)
        ]
      } in
      run_test "Transpose Up" ([], [dummy_func])
    



(* Execute all tests *)
let () =
  (* test1 (); 
  test3 ();
  test5 (); 
  test7 ();
  test8 ();
  test9 ();
  test10 ();
  test11();
  test12();
  test13_prime(); *)
  (* test13(); *)
  test_transpose_up();
