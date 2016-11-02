open Ast
open Prettyprint

let lexbuf = Lexing.from_channel (open_in Sys.argv.(1);;
let ast = Parser.program Scanner.token lexbuf;;
(* print_endline (Prettyprint.string_of_program ast *)
Prettyprint.string_of_prog;;
