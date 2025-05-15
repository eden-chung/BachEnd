open Ast  (* Reuse op, typ, and note definitions *)

(* A typed expression: includes the expression's type (typ) and the expression itself (sx) *)
type sexpr = typ * sx

(* semantically-checked expression *)
and sx =
  | SLiteral of int (* int literal *)
  | SBoolLit of bool (* boolean literal *)
  | SNoteLit of note  (* note literal *)
  | SNoteList of note list list
  | SChordLit  of note list
  | SId of string (* variable identifier *)
  | SAssign of string * sexpr (* variable assignment: string is var name, sexpr is expression *)
  | SBinop of sexpr * op * sexpr (* binary operator: left operand, operator, right operand *)
  | SCall of string * sexpr list (* function call: function name, list of arguments *)

(* A typed statement *)
type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SPrint of sexpr
  | SRepeat of sexpr * sstmt 
  | SWrite of sstmt
  | STranspose of sexpr * sstmt
  | SWriteAttrs of {
    name : string;
    tempo : int;
    clef : string option;
    timesig : (int * int) option;
    keysig : string option;
    body : sstmt;
  }
  (* return statement *)
  | SReturn of sexpr

(* Typed function declaration *)
type sfunc_decl = {
  srtyp : typ;
  sfname : string;
  sformals : bind list;
  slocals : bind list;
  sbody : sstmt list;
}

(* A typed program: globals + functions *)
type sprogram = bind list * sfunc_decl list


(* A small helper to print a note literal *)
let string_of_note n =
  (* e.g. "C#4/8" or "R/4" for a rest *)
  if String.lowercase_ascii n.pitch = "r" then
    "R/" ^ string_of_int n.length
  else
    let base = n.pitch ^
              (if n.octave = 0 then "" else string_of_int n.octave) in
    base ^ "/" ^ string_of_int n.length

let rec string_of_sexpr ((t, e) : typ * sx) : string =
  "(" ^ string_of_typ t ^ " : " ^
  match e with
  | SLiteral l       -> string_of_int l
  | SBoolLit true    -> "true"
  | SBoolLit false   -> "false"
  | SNoteLit n       -> string_of_note n
  (* | SNoteList notes ->
       "[" ^ String.concat " "
         (List.map (fun n -> string_of_note n) notes)
       ^ "]" *)
  | SNoteList seqs ->
      "[" ^ String.concat " "
         (List.map (fun notes ->
             if List.length notes = 1 then
               string_of_note (List.hd notes)            (* single note *)
             else
               "<" ^
               String.concat " " (List.map string_of_note notes) ^
               ">"                                       (* chord *)
          ) seqs)
      ^ "]"
  | SChordLit notes  ->
       "<" ^ String.concat " " (List.map string_of_note notes) ^ ">"
  | SId s            -> s
  | SAssign (v, e2)  -> v ^ " = " ^ string_of_sexpr e2
  | SBinop (e1,o,e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SCall (f, el)    ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  ^ ")"

let rec string_of_sstmt = function
  | SBlock stmts ->
      "{\n" ^
      String.concat "" (List.map string_of_sstmt stmts) ^
      "}\n"

  | SExpr expr ->
      string_of_sexpr expr ^ ";\n"

  | SReturn expr ->
      "return " ^ string_of_sexpr expr ^ ";\n"

  | SIf (cond, then_s, else_s) ->
      "if (" ^ string_of_sexpr cond ^ ")\n" ^
      string_of_sstmt then_s ^
      "else\n" ^
      string_of_sstmt else_s

  | SWhile (cond, body) ->
      "while (" ^ string_of_sexpr cond ^ ")\n" ^
      string_of_sstmt body

  | SFor (init, cond, post, body) ->
      "for (" ^ string_of_sexpr init ^ "; "
             ^ string_of_sexpr cond ^ "; "
             ^ string_of_sexpr post ^ ")\n" ^
      string_of_sstmt body

  | SPrint expr ->
      "print(" ^ string_of_sexpr expr ^ ");\n"

  | SRepeat (count, body) ->
      "repeat (" ^ string_of_sexpr count ^ ")\n" ^
      string_of_sstmt body

  (* | SWrite stmt ->
      "write " ^ 
      (match stmt with
       | SExpr _ | SReturn _ -> String.trim (string_of_sstmt stmt) ^ ";\n"
       | _ -> "\n" ^ string_of_sstmt stmt) *)

  | SWriteAttrs({ name; tempo; clef; timesig; keysig; body }) ->
    let meta =
      Printf.sprintf "NAME=\"%s\", TEMPO=%d" name tempo ^
      (match clef with Some c -> ", CLEF=" ^ c | None -> "") ^
      (match timesig with Some (a,b) -> Printf.sprintf ", TIMESIGNATURE=(%d,%d)" a b | None -> "") ^
      (match keysig with Some k -> ", KEYSIGNATURE=\"" ^ k ^ "\"" | None -> "")
    in
    "WRITE(" ^ meta ^ ") " ^ string_of_sstmt body



  | STranspose (expr, body) ->
      "transpose(" ^ string_of_sexpr expr ^ ")\n" ^
      string_of_sstmt body

let string_of_sfdecl (f : sfunc_decl) : string =
  string_of_typ f.srtyp ^ " " ^ f.sfname ^
  "(" ^ String.concat ", " (List.map snd f.sformals) ^ ")\n" ^
  "{\n" ^
  String.concat "" (List.map (fun (t,v) -> string_of_typ t ^ " " ^ v ^ ";\n") f.slocals) ^
  String.concat "" (List.map string_of_sstmt f.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) : string =
  "\n\nSemantically checked program:\n\n" ^
  String.concat "" (List.map (fun (t,v) -> string_of_typ t ^ " " ^ v ^ ";\n") vars) ^
  "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)