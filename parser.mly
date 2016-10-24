%{ open Ast %}

%token EOF

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
  /* nothing */ { [], [] }
