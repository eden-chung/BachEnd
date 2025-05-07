(* IR generation: translate takes a semantically checked AST and
   produces LLVM IR

   LLVM tutorial: Make sure to read the OCaml version of the tutorial

   http://llvm.org/docs/tutorial/index.html

   Detailed documentation on the OCaml LLVM library:

   http://llvm.moe/
   http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

(* input is ast and output is an llvm*)

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) = (* global variables and a list of functions is the input. the output is the LLVRM IR code*)
  let context    = L.global_context () in (* value of this context is immutable. it's a pointer (address) so we won't change the value of this pointer, but we will use the value to modify the LLVM object*)

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "MicroC" in (* module is top level container to store the IR instructions. also a pointer pointing to the module object*)

  (* Get types from the context *)
  let i32_t      = L.i32_type    context (* 32 bit will be used for an int*)
  and i8_t       = L.i8_type     context  (* 8 bit int will be used to describe a pointer*)
  and i1_t       = L.i1_type     context in (* the 1 bit int will be used for boolean*)

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Note -> L.pointer_type i8_t (* maybe modify this later depending on how we represent note*)
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = L.const_int (ltype_of_typ t) 0 (* 0 defined for ocaml int, 64 bits. *)
      in StringMap.add n (L.define_global n init the_module) m in (* allows us to use this map later*)
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype = (* we only have 1 external function. when we try to construct function definitions we use L.function_type, need to provide return type and types for formals*)
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in (*L.var_arg_funciton means there can be a different number of arguments. then the formals we provide an array*)
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in


    (* mapping between Bachend strings and lilypond*)
    let pitch_to_lilypond pitch =
      match String.lowercase_ascii pitch with
      | "c"  -> "c"
      | "c#" -> "cis"
      | "db" -> "des"
      | "d"  -> "d"
      | "d#" -> "dis"
      | "eb" -> "ees"
      | "e"  -> "e"
      | "f"  -> "f"
      | "f#" -> "fis"
      | "gb" -> "ges"
      | "g"  -> "g"
      | "g#" -> "gis"
      | "ab" -> "aes"
      | "a"  -> "a"
      | "a#" -> "ais"
      | "bb" -> "bes"
      | "b"  -> "b"
      | "r"  -> "r"
      | _ -> failwith ("Unknown pitch: " ^ pitch)
    in

    (* convert the input into lilypond *)
    (* let lilypond_of_body stmts =
      let note_strs =
        stmts
        |> List.filter_map (function
            | SExpr (_, SNote (pitch, octave, duration)) ->
                let p = pitch_to_lilypond pitch in
                let oct_suffix =
                  if pitch = "r" then ""
                  else if octave = 4 then "'"
                  else if octave > 4 then String.make (octave - 4) '\''
                  else String.make (4 - octave) ','
                in
                Some (Printf.sprintf "%s%d%s" p duration oct_suffix)
            | SExpr (_, SRest duration) ->
                Some (Printf.sprintf "%dr" duration)
            | _ -> None)
      in
      String.concat " " note_strs
    in *)
    let lilypond_of_body stmts =
      stmts
      |> List.filter_map (function
           | SExpr (_, SNoteLit note) ->
               (* unpack the record *)
               let p    = pitch_to_lilypond note.pitch in
               let dur  = note.length in
               (* for rests (pitch = "r") we omit the octave suffix *)
               let oct_suffix =
                 if note.pitch = "r" then ""
                 else if note.octave = 4 then "'"
                 else if note.octave > 4 then String.make (note.octave - 4) '\''
                 else String.make (4 - note.octave) ','
               in
               Some (Printf.sprintf "%s%d%s" p dur oct_suffix)
    
           | _ -> None)
      |> String.concat " "
    in
    

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in (* use this to as a cursor/pen to say that this is where we are in the block, let me insert IR instructions here*)
    (* blocks are entry, while, etc on the example.out*)

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in

    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
        let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
        StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
        let local_var = L.build_alloca (ltype_of_typ t) n builder
        in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in
    

    (* Construct code for an expression; return its value *)
    let rec build_expr builder ((_, e) : sexpr) = match e with
        SLiteral i  -> L.const_int i32_t i
      | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = build_expr builder e in
        ignore(L.build_store e' (lookup s) builder); e'
      (* | SNote (pitch, octave, duration) ->
        let pitch_str = pitch_to_lilypond pitch in
        let octave_str =
          if pitch = "r" then ""  (* rests don't get ticks *)
          else if octave = 4 then "'"
          else if octave > 4 then String.make (octave - 4) '\''
          else String.make (4 - octave) ',' in
        let lilypond_str = Printf.sprintf "%d%s%s" duration pitch_str octave_str in
        L.build_global_stringptr lilypond_str "note_str" builder  (* optional: for now *)
       (* we can change this later but for now default octave is octave 4*)
      | SRest duration -> Printf.sprintf "%dr" duration *)
      | SBinop (e1, op, e2) ->
        let e1' = build_expr builder e1
        and e2' = build_expr builder e2 in
        (match op with
           A.Add     -> L.build_add
         | A.Sub     -> L.build_sub
         | A.And     -> L.build_and
         | A.Or      -> L.build_or
         | A.Equal   -> L.build_icmp L.Icmp.Eq
         | A.Neq     -> L.build_icmp L.Icmp.Ne
         | A.Less    -> L.build_icmp L.Icmp.Slt
        ) e1' e2' "tmp" builder
      | SCall ("print", [e]) ->
        L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
          "printf" builder
      | SCall (f, args) ->
        let (fdef, fdecl) = StringMap.find f function_decls in
        let llargs = List.rev (List.map (build_expr builder) (List.rev args)) in
        let result = f ^ "_result" in
        L.build_call fdef (Array.of_list llargs) result builder
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function (* takes a builder as an argument. given the place to instruct the next IR instruction, then we will construct IR insturction for that statement.*)
        SBlock sl -> List.fold_left build_stmt builder sl (* if the statement is a block, we call the build_stmt, aggregated value is in this builder*)
      | SExpr e -> ignore(build_expr builder e); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e) builder); builder
      | SIf (predicate, then_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate in

        let then_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context then_bb) then_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context then_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val then_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    (* add_terminal func_builder (L.build_ret (L.const_int i32_t 0)) *)
    let _ = add_terminal func_builder (L.build_ret (L.const_int i32_t 0)) in


    (* this is all to output the lilypond output*)
    let raw_body = lilypond_of_body fdecl.sbody in

    (* hardcode the header with the staff and time signature for now*)
    let header =
      "\\version \"2.24.2\"\n\
       \\score { \\new Staff { \\clef treble \\time 4/4 \\tempo 4 = 100\n"
    in

    let footer =
      "\n  } }\n"
    in

    (* append sections *)
    let full_score = header ^ raw_body ^ footer in

    Printf.printf "%s\n" full_score

  in

  List.iter build_function_body functions;
  the_module
  (* returns module. is a container that stores llvm/ir code. returns a container for ir code*)
