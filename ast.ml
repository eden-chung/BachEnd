(* Abstract Syntax Tree and functions for printing it *)

type op = ADD | SUB | REPEAT | CONTINUE | BREAK | EQUAL | NEQ | LT | GT | LEQ | GEQ | AND | OR | TIMES | DIVIDE

type unop = NOT

type typ = INT | NOTE | STRING | BOOL

type note = {
PITCH : STRING;
OCTAVE : INT;
LENGTH : INT;
}

type expr =
    LITEARL of INT
  | BOOLLIT of BOOL
  | STRINGLIT of STRING
  | NOTELIT of NOTE
  | ID of STRING
  | BINOP of EXPR * OP * EXPR
  | UNOP of UNOP * EXPR
  | ASSIGN of STRING * EXPR
  (* function call *)
  | CALL of STRING * EXPR LIST

type stmt =
    BLOCK of STMT LIST
  | EXPR of EXPR
  | IF of EXPR * STMT * STMT
  | WHILE of EXPR * STMT
  (* return *)
  | FOR of EXPR * EXPR * EXPR * STMT (* for loop*)
  | PRINT of EXPR
  | REPEAT of EXPR * STMT (* repeat n times loop*)
  | RETURN of EXPR

(* int x: name binding *)
type bind = TYP * STRING

(* func_def: ret_typ fname formals locals body *)
type func_def = {
  RTYP: TYP;
  FNAME: STRING;
  FORMALS: BIND LIST;
  LOCALS: BIND LIST;
  BODY: STMT LIST;
}

type program = BIND LIST * FUNC_DEF LIST

(* Pretty-printing functions *)
let STRING_OF_OP = function
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

let rec STRING_OF_EXPR = function
    LITEARL(l) -> STRING_OF_INT l
  | BOOLLIT(TRUE) -> "TRUE"
  | BOOLLIT(FALSE) -> "FALSE"
  | ID(S) -> S
  | BINOP(E1, O, E2) ->
    STRING_OF_EXPR E1 ^ " " ^ STRING_OF_OP O ^ " " ^ STRING_OF_EXPR E2
  | ASSIGN(V, E) -> V ^ " = " ^ STRING_OF_EXPR E
  | CALL(F, EL) ->
      F ^ "(" ^ String.concat ", " (List.map STRING_OF_EXPR EL) ^ ")"

let rec STRING_OF_STMT = function
    BLOCK(STMTS) ->
    "{\n" ^ String.concat "" (List.map STRING_OF_STMT STMTS) ^ "}\n"
  | EXPR(EXPR) -> STRING_OF_EXPR EXPR ^ ";\n"
  | RETURN(EXPR) -> "RETURN " ^ STRING_OF_EXPR EXPR ^ "!\n"
  | BREAK() -> "BREAK!\n"
  | CONTINUE() -> "CONTINUE!\n"
  | IF(E, S1, S2) ->  "IF (" ^ STRING_OF_EXPR E ^ ")\n" ^
                      STRING_OF_STMT S1 ^ "ELSE\n" ^ STRING_OF_STMT S2
  | WHILE(E, S) -> "WHILE (" ^ STRING_OF_EXPR E ^ ") " ^ STRING_OF_STMT S
  | FOR(X, Y, Z) -> "FOR (" ^ X ^ "IN " ^ STRING_OF_EXPR Y ^ ") " ^ STRING_OF_STMT Z
  | REPEAT(X, S) -> "REPEAT (" ^ X ^ ") " ^ STRING_OF_STMT S
  | PRINT(X) -> "PRINT (" ^ X ^ ")!"
  



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
