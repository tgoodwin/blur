open Prettyprint
open Ast
open Generator

(* open Llvm *)
(* open Llvm_analysis *)

type action = Pretty | Llvm | Checked_Llvm

let _ =
	let action = if Array.length Sys.argv > 1 then
		List.assoc Sys.argv.(1) [ ("-p", Pretty);
			("-l", Llvm);
			("-c", Checked_Llvm) ]
		else Checked_Llvm in
	let lexbuf = Lexing.from_channel stdin in
	let ast = Parser.program Scanner.token lexbuf in 
	Semantic_analyzer.check_prog ast;
	match action with
	Pretty -> print_endline (Prettyprint.string_of_prog ast)
	| Llvm -> print_string (Llvm.string_of_llmodule (Generator.translate ast))
	| Checked_Llvm -> let m = Generator.translate ast in
		Llvm_analysis.assert_valid_module m;
		print_string (Llvm.string_of_llmodule m)
