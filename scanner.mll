{ open Parser }

let character = ['a'-'z' 'A'-'Z']
let number = ['0'-'9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"	{ comment lexbuf }
| '('	{ LPAREN }
| ')'	{ RPAREN }
| '{'	{ LBRACE }
| '}'	{ RBRACE }
| ';'	{ SEMI }
| ','	{ COMMA }
| '+'	{ PLUS }
| '-'	{ MINUS }
| '*'	{ TIMES }
| '/'	{ DIVIDE }
| '='	{ ASSIGN }
| "=="	{ EQ }
| "!+"	{ NEQ }
| '<'	{ LT }
| "<="	{ LEQ }
| '>'	{ GT }
| ">="	{ GEQ }
| "&&"	{ AND }
| "||"	{ OR }
| '!'	{ NOT }
| "char"	{ CHAR }
| "int" 	{ INT }
| "boolean"	{ BOOL }
| "struct"	{ STRUCT }
| "double"	{ DOUBLE }
| "string"	{ STRING }
| "null"	{ NULL }
| "for"		{ FOR }
| "while"	{ WHILE }
| "if"		{ IF }
| "else"	{ ELSE }
| "void"	{ VOID }
| "return"	{ RETURN }
| "true"	{ TRUE }
| "false"	{ FALSE }
| character as id { Id(id) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| '_'?['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }

and comment = parse
 "/*"	{ token lexbuf }
| _	{ comment lexbuf }

