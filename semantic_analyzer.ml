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
	
	let check_not_void (vdecl : vardecl) = 
		(* Get the types of the globals *)
		let global_typ = (fun v -> v.declTyp) vdecl in
				if global_typ = Void then raise (Failure ("illegal global var"))
				else () in

	List.iter check_not_void globals;