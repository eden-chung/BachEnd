(* Abstract Syntax Tree and functions for printing it *)

type op = ADD | SUB | REPEAT | CONTINUE | BREAK | EQUAL | NEQ | LT | GT | LEQ | GEQ | AND | OR | TIMES | DIVIDE

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
   | Id        of string
   | Binop     of expr * op * expr
   | Unop      of unop * expr
   | Assign    of string * expr
   | Call      of string * expr list

 type stmt =
     Block    of stmt list
   | Expr     of expr
   | If       of expr * stmt * stmt
   | While    of expr * stmt
   | For      of expr * expr * expr * stmt  (* for loop *)
   | Print    of expr
   | Repeat   of expr * stmt               (* repeat n times *)
   | Return   of expr
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

 let rec string_of_expr = function
     Literal l      -> string_of_int l
   | BoolLit true  -> "true"
   | BoolLit false -> "false"
   | Id s          -> s
   | Binop (e1, o, e2) ->
     string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
   | Assign (v, e) -> v ^ " = " ^ string_of_expr e
   | Call (f, el)  ->
       f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "RETURN " ^ string_of_expr expr ^ "!\n"
  | Break -> "BREAK!\n"
  | Continue -> "CONTINUE!\n"
  | If(e, s1, s2) ->  "IF (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "ELSE\n" ^ string_of_stmt s2
  | While(E, S) -> "WHILE (" ^ string_of_expr E ^ ") " ^ string_of_stmt S
  | For(X, Y, Z) -> "FOR (" ^ X ^ "IN " ^ string_of_expr Y ^ ") " ^ string_of_stmt Z
  | Repeat(X, S) -> "REPEAT (" ^ X ^ ") " ^ string_of_stmt S
  | Print(X) -> "PRINT (" ^ X ^ ")!"
  



let STRING_OF_TYP = function
    INT -> "INT"
  | BOOL -> "BOOL"
  | NOTE -> "NOTE"
  | STRING -> "STRING"

let STRING_OF_VDECL (T, ID) = STRING_OF_TYP T ^ " " ^ ID ^ "!\n"


let STRING_OF_VINITIALIZE (T, ID, INITIALIZE) = STRING_OF_TYP T ^ " " ^ ID ^ "=" ^ INITIALIZE ^ "!\n"


let STRING_OF_FDECL FDECL =
  STRING_OF_TYP FDECL.RTYP ^ " " ^
  FDECL.FNAME ^ "(" ^ String.concat ", " (List.map snd FDECL.FORMALS) ^
  ")\n{\n" ^
  String.concat "" (List.map STRING_OF_VDECL FDECL.LOCALS) ^
  String.concat "" (List.map STRING_OF_STMT FDECL.BODY) ^
  "}\n"


let STRING_OF_PROGRAM (VARS, FUNCS) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map STRING_OF_VDECL VARS) ^ String.concat "" (List.map STRING_OF_VINITIALIZE INITIALIZE) ^ "\n" ^
  String.concat "\n" (List.map STRING_OF_FDECL FUNCS)
