%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE ASSIGN EXCLAMATION LBRACKET RBRACKET DOT
%token EQUAL NEQ LT AND OR GT LEQ GEQ NOT
%token IF ELSE WHILE INT BOOL ELSE_IF FOR IN NOTE WRITE TRANSPOSE
%token <string> STRING
%token PLUS TIMES MINUS DIVIDE
/* return, COMMA token */
%token RETURN COMMA BREAK CONTINUE REPEAT
%token NAME CLEF TEMPO TIMESIGNATURE KEYSIGNATURE TREBLE BASS
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token <Ast.note> NOTELIT
%token <Ast.note list> CHORDLIT
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%left NOT
%left OR
%left AND
%left EQUAL NEQ LEQ GEQ
%left LT GT
%left TIMES DIVIDE
%left PLUS MINUS


%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 /*| vdecl EXCLAMATION decls { (($1 :: fst $3), snd $3) }*/
 | vdecl decls { (($1 :: fst $2), snd $2) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }
vdecl_list:
  /*nothing*/ { [] }
  // | vdecl EXCLAMATION vdecl_list  {  $1 :: $3 }
  | vdecl vdecl_list               {  $1 :: $2 }

/* int x */
vdecl:
  typ ID EXCLAMATION                                    { ($1, $2) }
  /*
  | typ ID ASSIGN STRING_LITERAL            { Vinitialize($1, $2, $4) }
  */


typ:
    INT   { INT   }
  | BOOL  { BOOL  }
  | NOTE  { NOTE  }
  | STRING  { STRING  }

/* fdecl */
fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp= $1;
      fname= $2;
      formals=$4;
      locals=$7;
      body=$8
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
  | typ ID ASSIGN expr EXCLAMATION { VDecl($1, $2, $4) }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr EXCLAMATION                        { Return $2      }
  | REPEAT LPAREN expr RPAREN stmt       { Repeat($3, $5) }
  | TRANSPOSE LPAREN expr RPAREN stmt       { Transpose($3, $5) }
  | WRITE LPAREN NAME ASSIGN STRING COMMA TEMPO ASSIGN LITERAL write_optional_args RPAREN stmt
    {
      let raw = $5 in
      let name = String.sub raw 1 (String.length raw - 2) in
      let (clef_opt, ts_opt, ks_opt) = $10 in
      WriteAttrs {
        name = name;
        tempo = $9;
        clef = clef_opt;
        timesig = ts_opt;
        keysig = ks_opt;
        body = $12;
      }
    }
  | expr EXCLAMATION                               { Expr $1      }


write_optional_args:
    /* no extra args */ { (None, None, None) }

  | COMMA CLEF ASSIGN clef_val write_optional_args {
      let (clef, ts, ks) = $5 in
      (Some $4, ts, ks)
    }

  | COMMA TIMESIGNATURE ASSIGN LPAREN LITERAL COMMA LITERAL RPAREN write_optional_args {
      let (clef, ts, ks) = $9 in
      (clef, Some ($5, $7), ks)
    }

  | COMMA KEYSIGNATURE ASSIGN STRING write_optional_args {
      let raw = $4 in
      let key = String.sub raw 1 (String.length raw - 2) in
      let (clef, ts, _) = $5 in
      (clef, ts, Some key)
    }
    
clef_val:
    TREBLE { "treble" }
  | BASS { "bass" }


expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | NOTELIT          { NoteLit($1)            }
  | CHORDLIT         { ChordLit($1)           }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, ADD,   $3)   }
  | expr MINUS  expr { Binop($1, SUB,   $3)   }
  | expr TIMES  expr { Binop($1, TIMES,   $3)   }
  | expr DIVIDE  expr { Binop($1, DIVIDE,   $3)   }
  | expr EQUAL     expr { Binop($1, EQUAL, $3)   }
  | expr NEQ    expr { Binop($1, NEQ, $3)     }
  | expr LEQ    expr { Binop($1, LEQ, $3)     }
  | expr GEQ    expr { Binop($1, GEQ, $3)     }
  | expr LT     expr { Binop($1, LT,  $3)   }
  | expr GT     expr { Binop($1, GT,  $3)   }
  | expr AND    expr { Binop($1, AND,   $3)   }
  | expr OR     expr { Binop($1, OR,    $3)   }
  | NOT expr         { Unop(NOT, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | LBRACKET note_list RBRACKET { NoteList($2)  }

note_list:
  //   NOTELIT                { [$1] }
  // | CHORDLIT               { $1}
  // | NOTELIT note_list       { $1 :: $2 }
  // | CHORDLIT note_list     { $1 @ $2 }
   NOTELIT                { [[ $1 ]]                }  
  | CHORDLIT               { [ $1 ]                  } 
  | NOTELIT note_list      { [ $1 ] :: $2            } 
  | CHORDLIT note_list     { $1      :: $2            }

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
