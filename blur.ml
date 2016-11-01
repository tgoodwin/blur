open Ast
open Prettyprint

let lexbuf = Lexing.from_channel stdin;;
let ast = Parser.program Scanner.token lexbuf;
print_endline ("pretty print blur");;