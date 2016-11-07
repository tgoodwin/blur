(* Ocammlex scanner for Blur lang *)
{ open Parser }

let character = ['a'-'z' 'A'-'Z']
let number = ['0'-'9']
let double = ((number+ '.' number*) | ('.' number+))

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
| "arr"		{ ARR }

(* literals for each data type *)
| "true"        { BOOL_LITERAL(true) }
| "false"       { BOOL_LITERAL(false) }
| number+ as lxm { INT_LITERAL(int_of_string lxm) }
| number* '.' number+ as lxm { DOUBLE_LITERAL(float_of_string lxm) }
| '"' (([^ '"'] | "\\\"")* as lxm) '"' { STRING_LITERAL(lxm) }
| '\'' ([' '-'&' '('-'[' ']'-'~'] as lxm) '\'' { CHAR_LITERAL(lxm) }

| "break"       { BREAK }
| "continue"    { CONTINUE }
| '_'?['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }

and comment = parse
 "*/"	{ token lexbuf }
| _	{ comment lexbuf }

