open Ast
open Prettyprint

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
	Semantic_analyzer.check_prog ast;
	print_endline (Prettyprint.string_of_prog ast);;
