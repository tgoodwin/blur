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
	
	(* There may not be duplicate variable names. *)
	let report_duplicate exceptf globals =
		(* Get the names of the globals *)
		let global_names = List.map (fun v -> v.declID) globals in
			let rec helper = function	
					n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
				| _ :: t -> helper t
				| [] -> ()
			in helper (List.sort compare global_names)
	in


	(* A global variable cannot have type void. *)
	let check_not_void (vdecl : vardecl) = 
		(* Get the types of the globals *)
		let global_typ = (fun v -> v.declTyp) vdecl in
				if global_typ = Void then raise (Failure ("illegal global var"))
				else () in

	List.iter check_not_void globals;

	report_duplicate (fun n -> "duplicate global " ^ n) globals;