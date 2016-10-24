{ open Parser }

let character = ['a'-'z' 'A'-'Z']
let number = ['0'-'9']

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "func" { FUNC }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| character as id { Id(id) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
