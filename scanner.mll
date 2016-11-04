(* Ocammlex scanner for Blur lang *)
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
| '['   { LBRACK }
| ']'   { RBRACK }
| ';'	{ SEMI }
| ','	{ COMMA }
| '+'	{ PLUS }
| '-'	{ MINUS }
| '*'	{ TIMES }
| '/'	{ DIVIDE }
| '='	{ ASSIGN }
| "=="  { EQUAL }
| "!="  { NEQUAL }
| '<'   { LT }
| "<="  { LEQ }
| '>'   { GT }
| ">="  { GEQ }
| "&&"  { AND }
| "||"  { OR }
| '!'   { NOT }
| "int" 	{ INT }
| "double"      { DOUBLE }
| "string"      { STRING }
| "char"        { CHAR }
| "boolean"	{ BOOL }
| "for"		{ FOR }
| "while"	{ WHILE }
| "if"		{ IF }
| "else"	{ ELSE }
| "void"	{ VOID }
| "return"	{ RETURN }
| "true"        { TRUE }
| "false"       { FALSE }
| "break"       { BREAK }
| "continue"    { CONTINUE }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| '_'?['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }

and comment = parse
 "*/"	{ token lexbuf }
| _	{ comment lexbuf }

