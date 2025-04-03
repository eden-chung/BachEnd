let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']
let whitespace = [' ' '\t' '\r']

let id = ['a'-'z'] (alpha | digit)*
let NUMBER = '0' | (('1'-'9') digit)*
let STRING = '"' (alphanumeric)* '"'
let NOTE = ['A'-'G']

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

    | "NONE" {NONE}
    | "TRUE" {TRUE}
    | "FALSE" {FALSE}
    | "AND" {AND}
    | "OR" {OR}
    | "IF" {IF}
    | "ELSE" {ELSE}
    | "ELSE IF" {ELSE_IF}
    | "WHILE" {WHILE}
    | "FOR" {FOR}
    | "IN" {IN}
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

    | "#" {SHARP}
    | "b" {FLAT}

    | NUMBER as lxm {NUMBER(int_of_string lxm)}
    | STRING as lxm {STRING lxm}
    | NOTE as lxm {NOTE lxm}
    | id as lxm {ID lxm}
    | eof {EOF}


and comment = parse
    '\n' { token lexbuf }
    | _ { comment lexbuf }