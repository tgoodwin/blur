%{ open Ast %}

%token SEMI COMMA LPAREN RPAREN LBRACE RBRACE FUNC
%token INT BOOL VOID
%token <string> ID
%token <int> LITERAL
%token EOF

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
    /* nothing */ { [], [] }
  | decls funcdecl { fst $1, ($2 :: snd $1) }

funcdecl:
  FUNC typ ID LPAREN formals_opt RPAREN LBRACE vardecl_list stmt_list RBRACE
    { { typ = $2;
        fname = $3;
        formals = $5;
        locals = List.rev $8; 
	body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list { List.rev $1 }	

formal_list:
    typ ID 	             { [($1, $2)] }
  | formal_list COMMA typ ID { ($3, $4) :: $1 }

typ:
    INT { Int }
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

expr:
    LITERAL { Literal($1) }  
