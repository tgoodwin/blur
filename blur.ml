open Ast
open Prettyprint

(* open Llvm *)
(* open Llvm_analysis *)

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in
        let sprogram = Semantic_analyzer.check_prog ast in
	print_endline (Prettyprint.string_of_prog ast);;
