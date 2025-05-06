{ open Parser }

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']
let whitespace = [' ' '\t' '\r']

let alpha_lower = ['a'-'z']
let id = alpha_lower alphanumeric+ (* must be at least 2 characters long*)

let NUMBER = '-'? ('0' | (['1'-'9'] digit*))
let string_body = ['a'-'z' 'A'-'Z' '0'-'9']*
let STRING = '"' string_body '"'

let NOTE_BASE = ['A'-'G']
let NOTE = NOTE_BASE ('#' | 'b')? | 'R' (* R for rest note *)

rule token = parse
    whitespace { token lexbuf }
    | "//" {comment lexbuf}
    | "!" {EXCLAMATION}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "{" {LBRACE}
    | "}" {RBRACE}
    | "[" {LBRACKET}
    | "]" {RBRACKET}
    | "," {COMMA}
    | "." {DOT}
    
    | '=' {ASSIGN}
    | '+' {PLUS}
    | '-' {MINUS}
    | '*' {TIMES}
    | '/' {DIVIDE}
    
    | "==" { EQUAL }
    | "!=" { NEQ }
    | "<=" { LEQ }
    | ">=" { GEQ }
    | "<" { LT }
    | ">" { GT }

    | "TRUE" {TRUE}
    | "FALSE" {FALSE}
    | "AND" {AND}
    | "OR" {OR}
    | "ELSE IF" {ELSE_IF}
    | "IF" {IF}
    | "ELSE" {ELSE}
    | "WHILE" {WHILE}
    | "FOR" {FOR}
    | "IN" {IN}
    | "NOT" {NOT}
    | "RETURN" {RETURN}
    | "BREAK" {BREAK}
    | "CONTINUE" {CONTINUE}
    | "REPEAT" {REPEAT}

    | "CLEF" {CLEF}
    | "TEMPO" {TEMPO}
    | "TIMESIGNATURE" {TIMESIGNATURE}
    | "KEYSIGNATURE" {KEYSIGNATURE}
    | "TREBLE" {TREBLE}
    | "BASS" {BASS}

    | NUMBER as lxm {NUMBER(int_of_string lxm)}
    | STRING as lxm {STRING lxm}
    | NOTE as lxm {NOTE lxm}
    | id as lxm {ID lxm}
    | eof {EOF}


and comment = parse
    '\n' { token lexbuf }
    | _ { comment lexbuf }