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
	let report_duplicate_var exceptf globals =
		(* Get the names of the globals *)
		let global_names = List.map (fun v -> v.declID) globals in
			let rec helper = function	
					n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
				| _ :: t -> helper t
				| [] -> ()
			in helper (List.sort compare global_names)
	in

	(* There may not be duplicate function names. *)
	let report_duplicate_func exceptf globals =
		(* Get the names of the globals *)
		let global_names = List.map (fun f -> f.fname) globals in
			let rec helper = function	
					n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
				| _ :: t -> helper t
				| [] -> ()
			in helper (List.sort compare global_names)
	in

	(* There may not be duplicate arg names. *)
	let report_duplicate_arg exceptf args =
		(* Get the names of the args *)
		let arg_names = List.map (fun a -> snd a) args in
			let rec helper = function	
					n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
				| _ :: t -> helper t
				| [] -> ()
			in helper (List.sort compare arg_names)
	in

	(* A global variable cannot have type void. *)
	let check_not_void (vdecl : vardecl) = 
		(* Get the types of the globals *)
		let global_typ = (fun v -> v.declTyp) vdecl in
				if global_typ = Void then raise (Failure ("illegal var"))
				else () 
	in

	(* An argument cannot have type void. *)
	let check_not_void_arg exceptf = function
			(Void, n) -> raise (Failure (exceptf n))
		| _ -> ()
	in

	(* Check assignment - type error *)


	(**** Check global vars ****)

	List.iter check_not_void globals;

	report_duplicate_var (fun n -> "duplicate global " ^ n) globals;

	(*if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function print may not be defined")) else ()*)

	(**** Check functions ****)

	report_duplicate_func (fun n -> "duplicate function " ^ n) functions;

	(* Function declaration for named function. *)
	let check_function func =
		List.iter (check_not_void_arg (fun n -> "illegal void argument " ^ n ^
      " in " ^ func.fname)) func.args;

		report_duplicate_arg (fun n -> "duplicate argument " ^ n) func.args;

	in
	List.iter check_function functions