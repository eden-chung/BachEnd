/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE ASSIGN EXCLAMATION LBRACKET RBRACKET DOT
%token EQUAL NEQ LT AND OR GT LEQ GEQ NOT
%token IF ELSE WHILE INT BOOL ELSE_IF FOR IN STRING NOTE
%token PLUS TIMES MINUS DIVIDE
/* return, COMMA token */
%token RETURN COMMA BREAK CONTINUE REPEAT
%token CLEF TEMPO TIMESIGNATURE KEYSIGNATURE TREBLE BASS
%token <int> LITERAL
%token <bool> BLIT
%token <string> ID
%token <Ast.note> NOTELIT
%token EOF

%start program
%type <Ast.program> program
%type <Ast.typ> typ
%type <Ast.vdecl> vdecl
%type <Ast.vdecl list> vdecl_list
%type <Ast.fdecl> fdecl
%type <Ast.expr> expr
%type <Ast.stmt> stmt
%type <Ast.stmt list> stmt_list
%type <Ast.vdecl list> formals_list
%type <Ast.vdecl list option> formals_opt
%type <Ast.expr list> args
%type <Ast.expr list option> args_opt
%type <Ast.fdecl list> decls

%right ASSIGN
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
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

/* int x */
vdecl:
  typ ID                                    { ($1, $2) }
  /*
  | typ ID ASSIGN STRING_LITERAL            { Vinitialize($1, $2, $4) }
  */


typ:
    INT   { INT   }
  | BOOL  { BOOL  }
  | NOTE  { NOTE  }
  | ID  { STRING  }

/* fdecl */
fdecl:
  vdecl LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=fst $1;
      fname=snd $1;
      formals=$3;
      locals=$6;
      body=$7
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
    expr SEMI                               { Expr $1      }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }

expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | NOTELIT          { NoteLit($1)            }
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

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
