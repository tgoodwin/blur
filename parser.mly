%{ open Ast %}

%token SEMI COMMA LPAREN RPAREN LBRACE RBRACE FUNC
%token INT BOOL VOID
%token <string> ID
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
  FUNC typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list RBRACE
    { { typ = $2;
        fname = $3;
        formals = $5;
        locals = List.rev $8; } }

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

vdecl_list:
    /* nothing */ { [] } 
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
    typ ID SEMI { ($1, $2) }   
