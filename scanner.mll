(* Ocammlex scanner for Blur lang
 * authored by Melissa Kaufman-Gomez-mhk2149, Dexter Callender-dec2148, Timothy Goodwin-tlg2132, Daniel Hong-sh3266 *)
 
{ open Parser }

let character = ['a'-'z' 'A'-'Z' '$' '@' '%' '&' '#' '*' '/' '|' '(' ')' '{' '}' '[' ']' '?' '-' '_' '+' '~' '<' '>' '!' ';' ':' '^' ',' '.' ' ' ]
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
| '%'   { MOD }
| '='	{ ASSIGN }
| "=="  { EQUAL }
| "!="  { NEQUAL }
| '<'   { LT }
| "<="  { LEQ }
| '>'   { GT }
| ">="  { GEQ }
| "and"  { AND }
| "or"  { OR }
| '!'   { NOT }
| '|'	{ BAR }
| "int" 	{ INT }
| "double"      { DOUBLE }
| "string"      { STRING }
| "char"        { CHAR }
| "bool"	{ BOOL }
| "for"		{ FOR }
| "while"	{ WHILE }
| "if"		{ IF }
| "else"	{ ELSE }
| "void"	{ VOID }
| "return"	{ RETURN }

(* literals for each data type *)
| "true"        { BOOL_LITERAL(true) }
| "false"       { BOOL_LITERAL(false) }
| number+ as lxm { INT_LITERAL(int_of_string lxm) }
| number* '.' number+ as lxm { DOUBLE_LITERAL(float_of_string lxm) }
| '"' (([^ '"'] | "\\\"")* as lxm) '"' { STRING_LITERAL(lxm) }
| '\'' ([' '-'&' '('-'[' ']'-'~'] as lxm) '\'' { CHAR_LITERAL(lxm) }

| '_'?['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }

and comment = parse
 "*/"	{ token lexbuf }
| _	{ comment lexbuf }

