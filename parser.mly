/* PARSER.MLY for BLUR */

%{ open Ast %}

%token LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK
%token SEMI COMMA FUNC
%token INT DOUBLE STRING CHAR BOOL
%token IF ELSE FOR WHILE VOID RETURN TRUE FALSE BREAK CONTINUE
%token PLUS MINUS TIMES DIVIDE ASSIGN
%token EQUAL NEQUAL LT LEQ GT GEQ AND OR NOT
%token <string> ID
%token <int> LITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQUAL NEQUAL
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT

%start program
%type <Ast.program> program

%%

program:
decls EOF { $1 }

decls:
/* nothing */ { [], [] }
| decls vardecl { ($2 :: fst $1), snd $1 } 
| decls funcdecl { fst $1, ($2 :: snd $1) }

funcdecl:
    typ ID LPAREN formals_opt RPAREN LBRACE vardecl_list stmt_list RBRACE
    {
        {
            typ = $1;
            fname = $2;
            formals = $4;
            locals = List.rev $7; 
            body = List.rev $8
        }
    }

formals_opt:
    /* nothing */ { [] }
  | formal_list { List.rev $1 }	

formal_list:
    typ ID 	             { [($1, $2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

/* maybe canvas goes here later? */
typ:
    INT { Int }
  | DOUBLE { Double }
  | CHAR { Char }
  | STRING { String }
  | BOOL { Bool }
  | VOID { Void }

vardecl_list:
    /* nothing */ { [] } 
  | vardecl_list vardecl { $2 :: $1 }

vardecl:
    typ ID SEMI { ($1, $2) }

stmt_list:
    /* nothing */ { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }

expr_opt:
  expr { $1 }

expr:
    LITERAL { Lit($1) }
  | ID                { Id($1) }
  | expr PLUS expr    { Binop($1, Add, $3) }
  | expr MINUS expr   { Binop($1, Sub, $3) }
  | expr TIMES expr   { Binop($1, Mult, $3) }
  | expr DIVIDE expr  { Binop($1, Div, $3) }
  | ID ASSIGN expr    { Asn ($1, $3) }

