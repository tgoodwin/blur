open Ast

open Sast 

type symbol_table = {
    parent: symbol_table option;
    variables: vardecl list
}

(* see slide 73 of types lecture *)
type func_type = {
    name: string;
    input_types: typ list;
    return_type: typ
}

type translation_env = {
    scope: symbol_table;
    functions: func_type list;
    return_type: typ option;
}

let check_prog (globals, functions) = 
	(* Raise exception if given list has a duplicate *)
	let report_duplicate exceptf list =
		print_endline ("hi")
	in

	report_duplicate (fun n -> "duplicate global " ^ n) (globals);
	