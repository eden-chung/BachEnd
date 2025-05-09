(* MODIFIED FROM CLASS CODE. Abstract Syntax Tree and functions for printing it*)

type op = Add | Sub | Times | Divide | Equal | Neq | Less | More | And | Or

type unaryop = Not | Neg

type typ = Int | Bool | Note

(* define how note is stored*)
type note = {
  pitch : string;
  octave : int;
  length : int;
}

type expr =
    Literal of int
  | BoolLit of bool
  | NoteLit of note
  | StringLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of unaryop * expr
  | Assign of string * expr
  (* function call *)
  | Call of string * expr list (* Function call is a new type of express in microC. List of expressions passed into the function*)

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt (* for loop*)
  | Print of expr
  | Repeat of expr * stmt (* repeat n times loop*)
  | Write of stmt
  (* return *)
  | Return of expr (* we need a return statement, it returns an expression*)

(* int x: name binding *)
type bind = typ * string

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ; (* return type *)
  fname: string; (* function name *)
  formals: bind list; (* list of arguments, for each we need a type and a string, which is a bind*)
  locals: bind list; (* then we have a program, which is locals and body*)
  body: stmt list;
}

type program = bind list * func_def list (* but now a program is a list of locals and body and function definitions*)

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | More -> ">"
  | Times -> "*"
  | Divide -> "/"
  | And -> "&&"
  | Or -> "||"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Write(stmt) -> "write " ^ string_of_stmt stmt ^ ";\n" (* Add pretty-printing for Write *)

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
