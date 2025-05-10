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

open Parsing;;
let _ = parse_error;;
# 4 "parser.mly"
open Ast
# 57 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* ASSIGN *);
  263 (* EXCLAMATION *);
  264 (* LBRACKET *);
  265 (* RBRACKET *);
  266 (* DOT *);
  267 (* EQUAL *);
  268 (* NEQ *);
  269 (* LT *);
  270 (* AND *);
  271 (* OR *);
  272 (* GT *);
  273 (* LEQ *);
  274 (* GEQ *);
  275 (* NOT *);
  276 (* IF *);
  277 (* ELSE *);
  278 (* WHILE *);
  279 (* INT *);
  280 (* BOOL *);
  281 (* ELSE_IF *);
  282 (* FOR *);
  283 (* IN *);
  284 (* NOTE *);
  286 (* PLUS *);
  287 (* TIMES *);
  288 (* MINUS *);
  289 (* DIVIDE *);
  290 (* RETURN *);
  291 (* COMMA *);
  292 (* BREAK *);
  293 (* CONTINUE *);
  294 (* REPEAT *);
  295 (* CLEF *);
  296 (* TEMPO *);
  297 (* TIMESIGNATURE *);
  298 (* KEYSIGNATURE *);
  299 (* TREBLE *);
  300 (* BASS *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  285 (* STRING *);
  301 (* LITERAL *);
  302 (* BLIT *);
  303 (* ID *);
  304 (* NOTELIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\003\000\006\000\
\006\000\006\000\006\000\004\000\007\000\007\000\009\000\009\000\
\008\000\008\000\010\000\010\000\010\000\010\000\010\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\011\000\011\000\013\000\013\000\012\000\012\000\
\014\000\014\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\000\000\003\000\003\000\001\000\
\001\000\001\000\001\000\009\000\000\000\001\000\001\000\003\000\
\000\000\002\000\002\000\003\000\007\000\005\000\003\000\001\000\
\001\000\001\000\001\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\002\000\
\003\000\003\000\004\000\003\000\001\000\002\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\008\000\009\000\010\000\011\000\051\000\000\000\
\000\000\000\000\000\000\001\000\003\000\004\000\000\000\000\000\
\007\000\000\000\000\000\000\000\014\000\000\000\000\000\000\000\
\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\025\000\000\000\026\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\012\000\018\000\
\019\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\042\000\020\000\046\000\
\044\000\000\000\000\000\023\000\000\000\000\000\048\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\000\000\029\000\000\000\000\000\000\000\000\000\043\000\
\000\000\022\000\050\000\000\000\021\000"

let yydgoto = "\002\000\
\007\000\008\000\009\000\010\000\028\000\019\000\020\000\041\000\
\021\000\042\000\043\000\078\000\048\000\079\000"

let yysindex = "\001\000\
\050\255\000\000\000\000\000\000\000\000\000\000\000\000\010\000\
\050\255\050\255\227\254\000\000\000\000\000\000\002\255\050\255\
\000\000\243\254\250\254\061\255\000\000\050\255\046\255\064\255\
\000\000\050\255\070\255\047\255\050\255\042\255\047\255\034\255\
\042\255\095\255\097\255\042\255\000\000\000\000\006\255\000\000\
\098\255\047\255\081\000\000\000\008\000\109\255\034\255\111\255\
\141\000\042\255\042\255\108\000\042\255\042\255\000\000\000\000\
\000\000\042\255\042\255\042\255\042\255\042\255\042\255\042\255\
\042\255\042\255\042\255\042\255\042\255\000\000\000\000\000\000\
\000\000\031\000\054\000\000\000\116\000\099\255\000\000\141\000\
\085\255\085\255\113\255\172\000\149\000\113\255\085\255\085\255\
\000\000\024\255\000\000\024\255\047\255\047\255\042\255\000\000\
\100\255\000\000\000\000\047\255\000\000"

let yyrindex = "\000\000\
\122\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\122\000\122\000\000\000\000\000\000\000\000\000\000\000\126\255\
\000\000\127\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\038\255\000\000\128\255\038\255\000\000\128\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\093\255\000\000\
\000\000\128\255\000\000\000\000\000\000\000\000\123\255\000\000\
\045\255\000\000\000\000\000\000\131\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\144\255\000\000\000\000\056\255\
\209\255\225\255\174\255\012\255\010\255\190\255\238\255\251\255\
\000\000\124\255\000\000\149\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\066\000\241\255\000\000\119\000\061\000\000\000\230\255\
\127\000\019\000\226\255\000\000\103\000\056\000"

let yytablesize = 461
let yytable = "\045\000\
\018\000\001\000\049\000\016\000\046\000\052\000\018\000\053\000\
\017\000\012\000\027\000\054\000\039\000\027\000\038\000\056\000\
\039\000\015\000\038\000\074\000\075\000\022\000\077\000\080\000\
\039\000\038\000\038\000\081\000\082\000\083\000\084\000\085\000\
\086\000\087\000\088\000\089\000\090\000\091\000\092\000\005\000\
\023\000\005\000\005\000\030\000\039\000\005\000\038\000\040\000\
\030\000\032\000\031\000\040\000\017\000\066\000\032\000\068\000\
\005\000\005\000\041\000\005\000\033\000\011\000\041\000\024\000\
\077\000\033\000\034\000\026\000\035\000\011\000\011\000\005\000\
\003\000\004\000\013\000\014\000\029\000\005\000\006\000\040\000\
\036\000\047\000\005\000\005\000\005\000\005\000\037\000\038\000\
\039\000\040\000\041\000\037\000\038\000\039\000\040\000\027\000\
\050\000\060\000\051\000\027\000\063\000\096\000\055\000\027\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\097\000\
\098\000\071\000\066\000\067\000\068\000\069\000\101\000\073\000\
\100\000\002\000\027\000\027\000\027\000\027\000\030\000\027\000\
\013\000\015\000\030\000\045\000\017\000\047\000\030\000\030\000\
\030\000\030\000\030\000\030\000\030\000\030\000\066\000\067\000\
\068\000\069\000\049\000\044\000\025\000\072\000\099\000\031\000\
\000\000\000\000\030\000\031\000\030\000\000\000\030\000\031\000\
\031\000\031\000\031\000\031\000\031\000\031\000\031\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\036\000\000\000\000\000\031\000\036\000\031\000\000\000\031\000\
\036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
\037\000\000\000\000\000\000\000\037\000\000\000\000\000\000\000\
\037\000\037\000\037\000\037\000\037\000\037\000\037\000\037\000\
\036\000\000\000\000\000\032\000\000\000\000\000\000\000\032\000\
\000\000\000\000\000\000\032\000\032\000\000\000\032\000\032\000\
\037\000\032\000\032\000\033\000\000\000\000\000\000\000\033\000\
\000\000\000\000\000\000\033\000\033\000\000\000\033\000\033\000\
\034\000\033\000\033\000\032\000\034\000\000\000\000\000\000\000\
\034\000\034\000\000\000\034\000\034\000\035\000\034\000\034\000\
\000\000\035\000\000\000\033\000\000\000\035\000\035\000\000\000\
\035\000\035\000\070\000\035\000\035\000\000\000\000\000\000\000\
\034\000\000\000\058\000\059\000\060\000\061\000\062\000\063\000\
\064\000\065\000\000\000\000\000\000\000\035\000\000\000\000\000\
\000\000\093\000\000\000\000\000\000\000\066\000\067\000\068\000\
\069\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\094\000\000\000\000\000\000\000\066\000\067\000\068\000\069\000\
\058\000\059\000\060\000\061\000\062\000\063\000\064\000\065\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\066\000\067\000\068\000\069\000\057\000\
\000\000\000\000\000\000\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\066\000\067\000\
\068\000\069\000\076\000\000\000\000\000\000\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\058\000\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\000\000\000\000\
\000\000\066\000\067\000\068\000\069\000\000\000\000\000\000\000\
\000\000\066\000\067\000\068\000\069\000\000\000\095\000\058\000\
\059\000\060\000\061\000\062\000\063\000\064\000\065\000\058\000\
\059\000\060\000\061\000\000\000\063\000\064\000\065\000\000\000\
\000\000\000\000\066\000\067\000\068\000\069\000\000\000\000\000\
\000\000\000\000\066\000\067\000\068\000\069\000\058\000\059\000\
\060\000\000\000\000\000\063\000\064\000\065\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\066\000\067\000\068\000\069\000"

let yycheck = "\030\000\
\016\000\001\000\033\000\002\001\031\000\036\000\022\000\002\001\
\007\001\000\000\026\000\006\001\003\001\029\000\003\001\042\000\
\007\001\047\001\007\001\050\000\051\000\035\001\053\000\054\000\
\015\001\014\001\015\001\058\000\059\000\060\000\061\000\062\000\
\063\000\064\000\065\000\066\000\067\000\068\000\069\000\002\001\
\047\001\004\001\005\001\002\001\035\001\008\001\035\001\003\001\
\002\001\008\001\004\001\007\001\007\001\030\001\008\001\032\001\
\019\001\020\001\003\001\022\001\019\001\001\000\007\001\003\001\
\095\000\019\001\020\001\004\001\022\001\009\000\010\000\034\001\
\023\001\024\001\009\000\010\000\007\001\028\001\029\001\035\001\
\034\001\048\001\045\001\046\001\047\001\048\001\045\001\046\001\
\047\001\048\001\035\001\045\001\046\001\047\001\048\001\003\001\
\002\001\013\001\002\001\007\001\016\001\003\001\005\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\093\000\
\094\000\005\001\030\001\031\001\032\001\033\001\100\000\009\001\
\021\001\000\000\030\001\031\001\032\001\033\001\003\001\035\001\
\003\001\003\001\007\001\009\001\005\001\003\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\030\001\031\001\
\032\001\033\001\003\001\029\000\022\000\047\000\095\000\003\001\
\255\255\255\255\031\001\007\001\033\001\255\255\035\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\003\001\255\255\255\255\031\001\007\001\033\001\255\255\035\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\003\001\255\255\255\255\255\255\007\001\255\255\255\255\255\255\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\035\001\255\255\255\255\003\001\255\255\255\255\255\255\007\001\
\255\255\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\035\001\017\001\018\001\003\001\255\255\255\255\255\255\007\001\
\255\255\255\255\255\255\011\001\012\001\255\255\014\001\015\001\
\003\001\017\001\018\001\035\001\007\001\255\255\255\255\255\255\
\011\001\012\001\255\255\014\001\015\001\003\001\017\001\018\001\
\255\255\007\001\255\255\035\001\255\255\011\001\012\001\255\255\
\014\001\015\001\003\001\017\001\018\001\255\255\255\255\255\255\
\035\001\255\255\011\001\012\001\013\001\014\001\015\001\016\001\
\017\001\018\001\255\255\255\255\255\255\035\001\255\255\255\255\
\255\255\003\001\255\255\255\255\255\255\030\001\031\001\032\001\
\033\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\003\001\255\255\255\255\255\255\030\001\031\001\032\001\033\001\
\011\001\012\001\013\001\014\001\015\001\016\001\017\001\018\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\030\001\031\001\032\001\033\001\007\001\
\255\255\255\255\255\255\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\030\001\031\001\
\032\001\033\001\007\001\255\255\255\255\255\255\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\011\001\012\001\
\013\001\014\001\015\001\016\001\017\001\018\001\255\255\255\255\
\255\255\030\001\031\001\032\001\033\001\255\255\255\255\255\255\
\255\255\030\001\031\001\032\001\033\001\255\255\035\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\011\001\
\012\001\013\001\014\001\255\255\016\001\017\001\018\001\255\255\
\255\255\255\255\030\001\031\001\032\001\033\001\255\255\255\255\
\255\255\255\255\030\001\031\001\032\001\033\001\011\001\012\001\
\013\001\255\255\255\255\016\001\017\001\018\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\030\001\031\001\032\001\033\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  ASSIGN\000\
  EXCLAMATION\000\
  LBRACKET\000\
  RBRACKET\000\
  DOT\000\
  EQUAL\000\
  NEQ\000\
  LT\000\
  AND\000\
  OR\000\
  GT\000\
  LEQ\000\
  GEQ\000\
  NOT\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  INT\000\
  BOOL\000\
  ELSE_IF\000\
  FOR\000\
  IN\000\
  NOTE\000\
  PLUS\000\
  TIMES\000\
  MINUS\000\
  DIVIDE\000\
  RETURN\000\
  COMMA\000\
  BREAK\000\
  CONTINUE\000\
  REPEAT\000\
  CLEF\000\
  TEMPO\000\
  TIMESIGNATURE\000\
  KEYSIGNATURE\000\
  TREBLE\000\
  BASS\000\
  EOF\000\
  "

let yynames_block = "\
  STRING\000\
  LITERAL\000\
  BLIT\000\
  ID\000\
  NOTELIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 38 "parser.mly"
            ( _1)
# 367 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 41 "parser.mly"
                 ( ([], [])               )
# 373 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 43 "parser.mly"
               ( ((_1 :: fst _2), snd _2) )
# 381 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'fdecl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'decls) in
    Obj.repr(
# 44 "parser.mly"
               ( (fst _2, (_1 :: snd _2)) )
# 389 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 46 "parser.mly"
              ( [] )
# 395 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl_list) in
    Obj.repr(
# 47 "parser.mly"
                                  (  _1 :: _3 )
# 403 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 51 "parser.mly"
                                                        ( (_1, _2) )
# 411 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser.mly"
          ( INT   )
# 417 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser.mly"
          ( BOOL  )
# 423 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
          ( NOTE  )
# 429 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "parser.mly"
            ( STRING  )
# 436 "parser.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 66 "parser.mly"
  (
    {
      rtyp= _1;
      fname= _2;
      formals=_4;
      locals=_7;
      body=_8
    }
  )
# 455 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 78 "parser.mly"
              ( [] )
# 461 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 79 "parser.mly"
                 ( _1 )
# 468 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 82 "parser.mly"
        ( [_1] )
# 475 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formals_list) in
    Obj.repr(
# 83 "parser.mly"
                             ( _1::_3 )
# 483 "parser.ml"
               : 'formals_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                ( [] )
# 489 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt_list) in
    Obj.repr(
# 87 "parser.mly"
                    ( _1::_2 )
# 497 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                                                   ( Expr _1      )
# 504 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 91 "parser.mly"
                                            ( Block _2 )
# 511 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "parser.mly"
                                            ( If(_3, _5, _7) )
# 520 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "parser.mly"
                                            ( While (_3, _5)  )
# 528 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                                                   ( Return _2      )
# 535 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 100 "parser.mly"
                     ( Literal(_1)            )
# 542 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 101 "parser.mly"
                     ( BoolLit(_1)            )
# 549 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.note) in
    Obj.repr(
# 102 "parser.mly"
                     ( NoteLit(_1)            )
# 556 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
                     ( Id(_1)                 )
# 563 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 104 "parser.mly"
                     ( Binop(_1, ADD,   _3)   )
# 571 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 105 "parser.mly"
                     ( Binop(_1, SUB,   _3)   )
# 579 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 106 "parser.mly"
                     ( Binop(_1, TIMES,   _3)   )
# 587 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 107 "parser.mly"
                      ( Binop(_1, DIVIDE,   _3)   )
# 595 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                        ( Binop(_1, EQUAL, _3)   )
# 603 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                     ( Binop(_1, NEQ, _3)     )
# 611 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 110 "parser.mly"
                     ( Binop(_1, LEQ, _3)     )
# 619 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 111 "parser.mly"
                     ( Binop(_1, GEQ, _3)     )
# 627 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 112 "parser.mly"
                     ( Binop(_1, LT,  _3)   )
# 635 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 113 "parser.mly"
                     ( Binop(_1, GT,  _3)   )
# 643 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "parser.mly"
                     ( Binop(_1, AND,   _3)   )
# 651 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "parser.mly"
                     ( Binop(_1, OR,    _3)   )
# 659 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "parser.mly"
                     ( Unop(NOT, _2)          )
# 666 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "parser.mly"
                     ( Assign(_1, _3)         )
# 674 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 118 "parser.mly"
                       ( _2                   )
# 681 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 120 "parser.mly"
                              ( Call (_1, _3)  )
# 689 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'note_list) in
    Obj.repr(
# 121 "parser.mly"
                                ( NoteList(_2)  )
# 696 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.note) in
    Obj.repr(
# 124 "parser.mly"
                           ( [_1] )
# 703 "parser.ml"
               : 'note_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.note) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'note_list) in
    Obj.repr(
# 125 "parser.mly"
                            ( _1 :: _2 )
# 711 "parser.ml"
               : 'note_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 129 "parser.mly"
              ( [] )
# 717 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 130 "parser.mly"
         ( _1 )
# 724 "parser.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 133 "parser.mly"
        ( [_1] )
# 731 "parser.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'args) in
    Obj.repr(
# 134 "parser.mly"
                    ( _1::_3 )
# 739 "parser.ml"
               : 'args))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
