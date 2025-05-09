/* Parser */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET PLUS MINUS ASSIGN
/* %token EQ NEQ AND OR 
%token IF ELSE WHILE FOR */
%token COMMA DOT
%token <string> ID

%start program_rule
%type <Ast.program> program_rule

%token EQ EQUAL NEQ /*LEQ GEQ LT GT*/ MORE LESS
%token AND OR

%token IF ELSE ELSE_IF WHILE FOR IN NOT
%token RETURN BREAK CONTINUE REPEAT

%token CLEF TEMPO TIMESIGNATURE KEYSIGNATURE TREBLE BASS
%token INT BOOL

%token <int> NUMBER
%token <string> STRING
%token <string> NOTE


%token TRUE FALSE
%token EXCLAMATION

/*%token EQUAL NEQ
%token LT
%token PLUS MINUS*/
%token TIMES DIVIDE
/*%token NOT*/


%right ASSIGN
%left OR
%left AND
%left EQUAL NEQ
%left LT
%left PLUS MINUS
%left TIMES DIVIDE
%left NOT
%right NOT
%nonassoc LOWER_THAN_ELSE


%token EOF

%%

program_rule:
  vdecl_list_rule stmt_list_rule EOF { ($1, [{ rtyp = Int; fname = "main"; formals = []; locals = []; body = $2 }]) }

vdecl_list_rule:
    /* nothing */                   { []       }
  | vdecl_rule vdecl_list_rule      { $1 :: $2 }

vdecl_rule:
  typ_rule ID SEMI                  { ($1, $2) }

typ_rule:
    INT                             { Int }
  | BOOL                            { Bool }

stmt_list_rule:
    /* nothing */                   { [] }
  | stmt_rule stmt_list_rule        { $1 :: $2 }

stmt_rule:
    expr_rule SEMI                                      { Expr $1         }
  | LBRACE stmt_list_rule RBRACE                        { Block $2        }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule { If ($3, $5, $7) }
  | IF LPAREN expr_rule RPAREN stmt_rule %prec LOWER_THAN_ELSE { If ($3, $5, Block []) }
  | IF LPAREN expr_rule RPAREN stmt_rule ELSE stmt_rule { If ($3, $5, $7) }
  | WHILE LPAREN expr_rule RPAREN stmt_rule             { While ($3, $5)  }
  | FOR LPAREN expr_rule SEMI expr_rule SEMI expr_rule RPAREN stmt_rule
                                                        { For ($3, $5, $7, $9) }

expr_rule:
    NUMBER                       { Literal $1 }
  | STRING                       { StringLit $1 }
  | NOTE                         { NoteLit $1 }
  | TRUE                         { BoolLit true }
  | FALSE                        { BoolLit false }
  | ID                           { Id $1 }
  | expr_rule PLUS expr_rule     { Binop ($1, Add, $3) }
  | expr_rule MINUS expr_rule    { Binop ($1, Sub, $3) }
  | expr_rule EQ expr_rule       { Binop ($1, Equal, $3) }
  | expr_rule NEQ expr_rule      { Binop ($1, Neq, $3) }
  | expr_rule LESS expr_rule       { Binop ($1, Less, $3) }
/*  | expr_rule LEQ expr_rule      { Binop ($1, Leq, $3) } */
  | expr_rule MORE expr_rule       { Binop ($1, More, $3) }
/*  | expr_rule GEQ expr_rule      { Binop ($1, Geq, $3) }*/
  | expr_rule AND expr_rule      { Binop ($1, And, $3) }
  | expr_rule OR expr_rule       { Binop ($1, Or, $3) }
  | EXCLAMATION expr_rule        { Unop(Not, $2) }
  | NOT expr_rule                { Unop(Not, $2) }
  | ID ASSIGN expr_rule          { Assign ($1, $3) }
  | LPAREN expr_rule RPAREN      { $2 }
  | ID LPAREN args_list_opt RPAREN { Call ($1, $3) }
  
args_list_opt:
    /* nothing */                { [] }
  | arg_list                    { $1 }

arg_list:
    expr_rule                   { [$1] }
  | expr_rule COMMA arg_list   { $1 :: $3 }

