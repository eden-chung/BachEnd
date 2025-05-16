(* Abstract Syntax Tree and functions for printing it *)

type op = ADD | SUB | EQUAL | NEQ | LT | GT | LEQ | GEQ | AND | OR | TIMES | DIVIDE

type unop = NOT

type typ = INT | NOTE | STRING | BOOL

type note = {
pitch : string;
octave : int;
length : int;
}

type expr =
    Literal of int
   | BoolLit   of bool
   | StringLit of string
   | NoteLit   of note
   (* | NoteList  of note list *)
   | NoteList  of note list list
   | ChordLit  of note list
   | Id        of string
   | Binop     of expr * op * expr
   | Unop      of unop * expr
   | Assign    of string * expr
   | TraitAssign    of string * string * expr
   | Call      of string * expr list

 type stmt =
     Block    of stmt list
   | VDecl    of typ * string * expr
   | Expr     of expr
   | If       of expr * stmt * stmt
   | While    of expr * stmt
   | For      of string * expr * stmt  (* for loop *)
   | Print    of expr
   | Repeat   of expr * stmt               (* repeat n times *)
   | Return   of expr
   | Transpose of expr * stmt
   | Write of stmt
   | WriteAttrs of {
    name : string;
    tempo : int;
    clef : string option;
    timesig : (int * int) option;
    keysig : string option;
    body : stmt;
  }
   | Continue 
   | Break    

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
   rtyp   : typ;
   fname  : string;
   formals: bind list;
   locals : bind list;
   body   : stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    ADD -> "+"
  | SUB -> "-"
  | EQUAL -> "=="
  | NEQ -> "!="
  | LEQ -> "<="
  | GEQ -> ">="
  | LT -> "<"
  | GT -> ">"
  | TIMES -> "*"
  | DIVIDE -> "/"
  | AND -> "&&"
  | OR -> "||"
  (* | AND -> "AND" which one was it?
  | OR -> "OR" *)
let string_of_unop = function
  | NOT -> "NOT"

let rec string_of_expr = function
     Literal l      -> string_of_int l
   | BoolLit true  -> "TRUE"
   | BoolLit false -> "FALSE"
   | StringLit s -> s
   | NoteLit n -> string_of_int n.length ^ n.pitch ^string_of_int n.octave
   | Id s          -> s
   | Binop (e1, o, e2) ->
     string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
   | Unop (u, e) -> string_of_unop u ^ " " ^ string_of_expr e
   | Assign (v, e) -> v ^ " = " ^ string_of_expr e
   | TraitAssign (n, t, e) -> n ^ "." ^ t ^ " = " ^ string_of_expr e
   | Call (f, el)  ->
       f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
   | NoteList groups ->
    "["
    ^ String.concat " "
        (List.map
           (fun notes ->
             if List.length notes = 1 then
               (* single note group *)
               let n = List.hd notes in
               string_of_int n.length ^ n.pitch ^ string_of_int n.octave
             else
               (* chord group *)
               "<"
               ^ String.concat " "
                   (List.map (fun n ->
                        string_of_int n.length ^ n.pitch ^ string_of_int n.octave
                     ) notes)
               ^ ">"
           )
           groups
        )
    ^ "]"
  | ChordLit notes ->
    "<"
    ^ String.concat " "
        (List.map (fun n ->
             string_of_int n.length ^ n.pitch ^ string_of_int n.octave
         ) notes)
    ^ ">"


let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "RETURN " ^ string_of_expr expr ^ "!\n"
  | Break -> "BREAK!\n"
  | Continue -> "CONTINUE!\n"
  | If(e, s1, s2) ->  "IF (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "ELSE\n" ^ string_of_stmt s2
  | While(e, s) -> "WHILE (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For(x, y, z) -> "FOR (" ^ x ^ "IN " ^ string_of_expr y ^ ") " ^ string_of_stmt z
  | Repeat(x, s) -> "REPEAT (" ^string_of_expr x ^ ") " ^ string_of_stmt s
  | Print(x) -> "PRINT (" ^ string_of_expr x ^ ")!"
  | WriteAttrs { name; tempo; clef; timesig; keysig; body } ->
    let meta =
      Printf.sprintf "NAME=\"%s\", TEMPO=%d" name tempo ^
      (match clef with Some c -> ", CLEF=" ^ c | None -> "") ^
      (match timesig with Some (a, b) -> Printf.sprintf ", TIMESIGNATURE=(%d,%d)" a b | None -> "") ^
      (match keysig with Some k -> ", KEYSIGNATURE=\"" ^ k ^ "\"" | None -> "")
    in
    "WRITE(" ^ meta ^ ") " ^ string_of_stmt body

let string_of_typ = function
    INT -> "INT"
  | BOOL -> "BOOL"
  | NOTE -> "NOTE"
  | STRING -> "STRING"

(*
type vdecl =
  | Vdecl     of typ * string
  | Vinitialize of typ * string * string
*)
(*
let string_of_vdecl = function
  | Vdecl (t, id) ->
      string_of_typ t ^ " " ^ id ^ "!\n"
  | Vinitialize (t, id, init) ->
      string_of_typ t ^ " " ^ id ^ " = " ^ init ^ "!\n"
*)

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ "!\n"


let string_of_vinitialize (t, id, initialize) = string_of_typ t ^ " " ^ id ^ "=" ^ initialize ^ "!\n"


let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

(*
let string_of_program (vars, funcs, initialize) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ String.concat "" (List.map string_of_vinitialize initialize) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
*)

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^
  String.concat "\n" (List.map string_of_fdecl funcs)
