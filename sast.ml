open Old_ast  (* Reuse op, typ, and note definitions *)

(* A typed expression: includes the expression's type (typ) and the expression itself (sx) *)
type sexpr = typ * sx

(* semantically-checked expression *)
and sx =
  | SLiteral of int (* int literal *)
  | SBoolLit of bool (* boolean literal *)
  | SNoteLit of note  (* note literal *)
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
