open Ast
open Prettyprint

let lexbuf = Lexing.from_channel stdin;;
let ast = Parser.program Scanner.token lexbuf;;
Prettyprint.string_of_prog;;