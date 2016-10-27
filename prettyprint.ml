open Ast

(* Pretty-printing functions *)

let string_of_op = function
    Add -> '+'
  | Sub -> '-'
  | Mul -> '*'
  | Div -> '/'

let string_of_expr = function
	Lit(l) -> string_of_int l

(*let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	let result = (* pretty printing function name *) expr in 
	print_endline (string_of_int result) *)
