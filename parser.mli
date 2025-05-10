type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | ASSIGN
  | EXCLAMATION
  | LBRACKET
  | RBRACKET
  | DOT
  | EQUAL
  | NEQ
  | LT
  | AND
  | OR
  | GT
  | LEQ
  | GEQ
  | NOT
  | IF
  | ELSE
  | WHILE
  | INT
  | BOOL
  | ELSE_IF
  | FOR
  | IN
  | NOTE
  | STRING of (string)
  | PLUS
  | TIMES
  | MINUS
  | DIVIDE
  | RETURN
  | COMMA
  | BREAK
  | CONTINUE
  | REPEAT
  | CLEF
  | TEMPO
  | TIMESIGNATURE
  | KEYSIGNATURE
  | TREBLE
  | BASS
  | LITERAL of (int)
  | BLIT of (bool)
  | ID of (string)
  | NOTELIT of (Ast.note)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
