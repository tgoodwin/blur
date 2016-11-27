open Ast

module A = Ast

(*open Sast*) 

module StringMap = Map.Make(String)

type symbol_table = {
    parent: symbol_table option;
    variables: vardecl list
}

(* see slide 73 of types lecture *)
type func_entry = {
    name: string;
    arg_types: datatype list;
    return_type: datatype
}

type env = {
    symtab: symbol_table;
    funcs: func_entry list;
    return_type: datatype option;
}

let check_prog (globals, functions) = 
	(* Add global variable declarations to the symbol table *)
	let check_global_var (env : env) = 
		let new_symbol_table =
			{
				(env.symtab)
				with variables = [];
			} in
		let new_env = { (env) with symtab = new_symbol_table; }
		in new_env 
	in

	(* Establish initial environment *)
	let env = 
		{
			symtab = { parent = None; variables = []; };
			funcs = []; (*built-in *)
			return_type = None;
		} in
	let (_, decl_list) = 
		(env, [])
	in decl_list

	(*(* Make a map of global variables *)
	let global_vars = 
		let global_var map (vdecl : A.vardecl) =
			let name = vdecl.declID in
			let typ = vdecl.declTyp in
		 	StringMap.add name typ map in 
		List.fold_left global_var StringMap.empty globals in

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
		let arg_names = List.map (fun a -> a.argdeclID) args in
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
				if global_typ = Datatype(Void) then raise (Failure ("illegal var"))
				else () 
	in

	(* An argument cannot have type void. *)
	let check_not_void_arg (adecl : argdecl) = 
		let arg_typ = (fun a -> a.argdeclType) adecl in
				if arg_typ = Datatype(Void) then raise (Failure ("illegal void arg"))
				else ()
	in

	(* Check assignment - type error *)
	let check_assign lval_type rval_type err = 
			if lval_type == rval_type then lval_type 
			else raise err 
	in 

	(**** Check global vars ****)

	List.iter check_not_void globals;

	report_duplicate_var (fun n -> "duplicate global " ^ n) globals;

	(**** Check functions ****)
 
	if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  	then raise (Failure ("function print may not be defined")) else ();

	report_duplicate_func (fun n -> "duplicate function " ^ n) functions;

	(* Function declaration for named function. *)
	(* TODO: Modify when we write these function in llvm. *)
	let built_in_decls =  StringMap.add "print"
     { typ = Datatype(Void); fname = "print"; args = [{argdeclType = Datatype(Int); argdeclID = "x"}];
       body = [] } (StringMap.singleton "printb"
     { typ = Datatype(Void); fname = "printb"; args = [{argdeclType = Datatype(Bool); argdeclID = "x"}];
       body = [] })
   in

  let function_decls = List.fold_left (fun map fd -> StringMap.add fd.fname fd map)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = function_decl "main" in (* Ensure "main" is defined. *)

	let check_function func env =
		List.iter check_not_void_arg func.args;

		report_duplicate_arg (fun n -> "duplicate argument " ^ n) func.args;

		let local_vars = StringMap.empty in

		let add_arg map (adecl: A.argdecl) =
			let name = adecl.argdeclID in
			let typ = adecl.argdeclType in
			StringMap.add name typ map 
		in
		let add_local map (vdecl: A.vardecl) = 
			let name = vdecl.declID in
			let typ = vdecl.declTyp in
			StringMap.add name typ map
		in

		(* Add function arguments to local_vars map. *)
		let local_vars = List.fold_left add_arg local_vars func.args in

		let type_of_var v =
			print_endline("decl?");
			try StringMap.find v local_vars
			with Not_found -> raise (Failure ("undeclared identifier " ^ v))
		in
		
		let rec binop typed_e1 op typed_e2 = 
			check_assign typed_e1 typed_e2 (Failure ("illegal assignment"))
		in

		let check_local_vardecl vdecl map = 
			let local_vars = add_local map vdecl in

      let init_expr = vdecl.declInit in
      match init_expr with
        A.Noexpr      -> local_vars
      | e             -> local_vars
		in

		(* Return the type of an expression or throw exception. *)
		let rec expr = function
				IntLit _ -> Datatype(Int)
			| DoubleLit _ -> Datatype(Double)
			| CharLit _ -> Datatype(Char) 
			| StrLit _ -> Datatype(String)
			| BoolLit _ -> Datatype(Bool)
			| Id t -> type_of_var t
			| Binop (e1, op, e2) -> 
					let checked_e1 = expr e1
					and checked_e2 = expr e2
					in binop checked_e1 op checked_e2
		in

		let rec stmt = function
				Expr e -> ignore (expr e) ; local_vars
			| Decl vardecl -> 
				(*let local_vars = check_local_vardecl vardecl map
				in local_vars*)check_local_vardecl vardecl local_vars ; local_vars
			|	Block stmtlst -> let rec check_block = function
						[Return _ as s] -> stmt s
					| Return _ :: _ -> raise (Failure "nothing may follow a return") 
					| Block sl :: ss -> check_block (sl @ ss) 
					| s :: ss -> stmt s ; check_block ss 
					| [] -> local_vars
				in check_block stmtlst

		in

		(* Account for local var decls being part of func body stmt *)
		let _ = stmt (Block func.body) in ()


	in
	List.iter check_function functions*)