{ open Parser }

let alpha = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let alphanumeric = ['a'-'z' 'A'-'Z' '0'-'9']
let whitespace = [' ' '\t' '\r' '\n']

let alpha_lower = ['a'-'z']
let id = alpha_lower alphanumeric+ (* must be at least 2 characters long*)

let INT = '-'? ('0' | (['1'-'9'] digit*))
let string_body = ['a'-'z' 'A'-'Z' '0'-'9']*
let STRING = '"' string_body '"'

(*
let NOTE_BASE = ['A'-'G']
let NOTE = NOTE_BASE ('#' | 'b')? | 'R' (* R for rest note *) *)


let number      = ['0'-'9']+
let octave_digit = ['1'-'8']
let accidental  = ['#' 'b']
let base_note   = ['A'-'G']
let note_char   = base_note accidental? | 'R'
let NOTE        = number? note_char octave_digit?



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
    | "NUMBER" {INT}
    | "BOOL" {BOOL}
    | "STRING" as lxm {STRING lxm}
    | "NOTE"   {NOTE}
    | "TRUE"   { BLIT(true)  }
    | "FALSE"  { BLIT(false) }
    | "BREAK" {BREAK}
    | "CONTINUE" {CONTINUE}
    | "REPEAT" {REPEAT}

    | "CLEF" {CLEF}
    | "TEMPO" {TEMPO}
    | "TIMESIGNATURE" {TIMESIGNATURE}
    | "KEYSIGNATURE" {KEYSIGNATURE}
    | "TREBLE" {TREBLE}
    | "BASS" {BASS}
(*
    | INT as lxm {LITERAL (int_of_string lxm)}
    | STRING as lxm {lxm}
    | NOTE as lxm {NOTE lxm}
    | id as lxm {ID lxm}
    | eof {EOF}
*)

(*    | NOTE as lxm {  
        (* parse lxm into your Ast.note record; e.g.: *)
       let n : Ast.note = {
          pitch  = String.sub lxm 0 1;
          octave = 4;    (* or default / parse from lxm if you include octave *)
          length = 1;
        } in
        NOTELIT n
    }
*)

    | NOTE as lxm {
        (* break out prefix digits, pitch+accidental, and suffix digit *)
        let len, rest =
          let buf = Buffer.create 3 in
          let i = ref 0 in
          while !i < String.length lxm && lxm.[!i] >= '0' && lxm.[!i] <= '9' do
            Buffer.add_char buf lxm.[!i];
            incr i
          done;
          Buffer.contents buf, !i
        in
        let pitch_acc_oct = String.sub lxm rest (String.length lxm - rest)
        in
        (* if last char is digit, that’s the octave *)
        let pitch, oct_str =
          let len_pao = String.length pitch_acc_oct in
          if len_pao > 1 && pitch_acc_oct.[len_pao-1] >= '1'
             && pitch_acc_oct.[len_pao-1] <= '8'
          then
            ( String.sub pitch_acc_oct 0 (len_pao-1)
            , String.make 1 pitch_acc_oct.[len_pao-1] )
          else
            ( pitch_acc_oct, "" )
        in
        let length = if len = "" then 4 else int_of_string len in
        let octave = if oct_str = "" then 4 else int_of_string oct_str in
        let n = { Ast.pitch; octave; length } in
        NOTELIT n
    }
    | STRING as lxm { STRING lxm }
    | INT as lxm {LITERAL (int_of_string lxm)}
    | id as lxm {ID lxm}
    | eof {EOF}


and comment = parse
    '\n' { token lexbuf }
    | _ { comment lexbuf }
