(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST and generate an SAST from it, generate LLVM IR,
   and dump the module *)

type action = Ast | Sast | LLVM_IR

let () =
  let action = ref LLVM_IR in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-s", Arg.Unit (set_action Sast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
  ] in
  let usage_msg = "usage: ./bachend.native [-a|-s|-l] [file.mc]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;

  let lexbuf = Lexing.from_channel !channel in

  let ast = Parser.program_rule Scanner.token lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.check ast in
    match !action with
      Ast     -> ()
    | Sast    -> print_string (Sast.string_of_sprogram sast)
    (* | LLVM_IR -> print_string (Llvm.string_of_llmodule (Irgen.translate sast)) *)
    (*| LLVM_IR -> ignore (translate sast) *) (* since we don't want to print LLVM code, just the bachend notes*)
    | LLVM_IR ->
      let lilypond_code = Irgen.translate sast in
      let output_file = "output.ly" in
      let oc = open_out output_file in
      output_string oc lilypond_code;
      close_out oc;
      (* now invoke lilypond *)
      let cmd = Printf.sprintf "lilypond -o output %s" output_file in
      let _ = Sys.command cmd in
      ()
  
