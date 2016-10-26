open Ast

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let expr = Parser.expr Scanner.token lexbuf in
	let result = (* pretty printing function name *) expr in 
	print_endline (string_of_int result)
