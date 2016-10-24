(* TODO: change this. This is the calculator code. *)
{ open Parser }

let character = ['a'-'z' 'A'-'Z']
let number = ['0'-'9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| ';'	{ SEMI }
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
| character as id { Id(id) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
