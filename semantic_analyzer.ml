open Ast

module A = Ast

(*open Sast*) 

module StringMap = Map.Make(String)

type symbol_table = {
    parent: symbol_table option;
    args: argdecl list;
    variables: vardecl list
}

type func_entry = {
    name: string;
    arg_types: datatype list;
    return_type: datatype
}

type env = {
    symtab: symbol_table;
    funcs: func_entry list;
    return_type: datatype option
}

let string_of_typ = function
    Int -> "int"
  | Double -> "double"
  | Char -> "char"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"

let string_of_datatype = function 
  | Datatype(t) -> string_of_typ t

let check_prog (globals, functions) = 
	(* Add global variable declarations to the symbol table *)
	print_endline("; checking prog");

	let built_in_functions = 
		(* TODO: fix types in general. *)
		(* print and println actually take expr, not a Datatype. We deal with this in check_func_call*)
		[ {name = "print"; arg_types = [Datatype(String)]; return_type = Datatype(Void);};
			{name = "println"; arg_types = [Datatype(String)]; return_type = Datatype(Void);};
			{name = "len"; arg_types = [Datatype(String)]; return_type = Datatype(Int);};
			{name = "readGrayscaleImage"; arg_types = [Datatype(String)]; return_type = Datatype(Int);};
			{name = "readColorImage"; arg_types = [Datatype(String)]; return_type = Datatype(Int);};
			{name = "charToIntensity"; arg_types = [Datatype(Char)]; return_type = Datatype(Int);};
			{name = "intensityToChar"; arg_types = [Datatype(Int)]; return_type = Datatype(Char);};
			{name = "intcast"; arg_types = [Datatype(Double)]; return_type = Datatype(Int);};
			{name = "doublecast"; arg_types = [Datatype(Int)]; return_type = Datatype(Double);}; ]
	in

	(* A global variable cannot have type void. *)
	let check_not_void (vdecl : vardecl) = 
		(* Get the types of the globals *)
		let global_typ = (fun v -> v.declTyp) vdecl in
				if global_typ = Datatype(Void) then raise 
					(Failure ("illegal void variable " ^  vdecl.declID))
				else () 
	in
	List.iter check_not_void globals;

	let rec get_variable_decl (symtab : symbol_table) (id : string) :datatype =
		print_endline(";getting var decl");
		print_endline(";" ^ string_of_int(List.length symtab.args));
		print_endline(";" ^ string_of_int(List.length symtab.variables));
		try 
			let decl = List.find (fun vdecl -> vdecl.declID = id) symtab.variables in decl.declTyp
		  with
		  | Not_found -> print_endline(";try again"); try let decl = List.find (fun adecl -> adecl.argdeclID = id) symtab.args in decl.argdeclType
		  			with
		  			| Not_found -> (match symtab.parent with 
		  				| Some parent -> print_endline(";look at parent"); get_variable_decl parent id 
		  				| _ -> raise Not_found) in

	(* Returns type of expression. *)
	let rec check_expr (env : env) (expr : expr) = 
		print_endline(";checking expr");
		match expr with 
			IntLit i -> print_endline(";int"); Datatype(Int)
		| DoubleLit d -> print_endline(";double"); Datatype(Double)
		| CharLit c -> print_endline(";char"); Datatype(Char)
		| StrLit s -> print_endline(";str"); Datatype(String)
		| BoolLit b -> print_endline(";bool"); Datatype(Bool)
		| Noexpr -> print_endline(";noexpr"); Datatype(Void)
		| Id s -> print_endline(";id"); 
				(try get_variable_decl env.symtab s 
				with | Not_found -> raise (Failure ("undeclared identifier " ^ s))
				) 
			(*Datatype(Int)*) (* Get type of var*)
		| FuncCall (s, arglist) -> check_func_call s arglist env
		| Binop (e1, op, e2) -> print_endline(";expr is binop");
			let t1 = check_expr env e1 
			and t2 = check_expr env e2 in
			match op with 
			| Add | Sub | Mult | Div | Lt | Leq | Gt | Geq -> print_endline(";arith");
			ignore(print_endline(";" ^ string_of_datatype t1));
			ignore(print_endline(";" ^ string_of_datatype t1));
			if t1 <> t2 then raise (Failure ("illegal operation")) 
			else t1
			| Eq | Neq | And | Or -> print_endline(";eq neq and or"); Datatype(Int)
			(* TODO: fail if type is not int or double *)
			| Asn -> print_endline(";asn");
				if t1 = t2 then t1
				else raise (Failure ("illegal assignment")) 
	

	(* Checking function call returns the type of the function. *)
	and check_func_call (id : string) (args : expr list) (env : env) = 
		print_endline(";checking function call");
		print_endline(";" ^ string_of_int(List.length env.funcs));
		try
			let func_entry = List.find (fun f -> f.name = id) env.funcs in
			(* Get the types of the arg expressions. *)
			let arg_types = List.map(fun arg -> check_expr env arg) args in
			(* Ensure that arguments match. *)
			if List.length func_entry.arg_types <> List.length args then
			raise (Failure ("Incorrect number of args for function call " ^ id ^ 
				". Expecting " ^ (string_of_int (List.length func_entry.arg_types)) ^ " args but got "
				^ (string_of_int (List.length args)))) else 
			if id <> "print" && id <> "println" && arg_types <> func_entry.arg_types then
			raise (Failure("unexpected arg types")) else
			Datatype(Int)
		with | Not_found -> (*Datatype(Int)*) raise (Failure ("undeclared function " ^ id))
	in	  		

	let check_variable_declaration (env : env) (decl: vardecl) = 
		print_endline(";checking var decls");

		(* A variable cannot have type void. *)
		let check_not_void_var (decl : vardecl) = 
			print_endline(";checking void vars");
			let var_typ = (fun v -> decl.declTyp) decl in
					if var_typ = Datatype(Void) then raise (Failure ("illegal void variable " ^ decl.declID))
					else ()
		in 
		ignore(check_not_void_var (decl));

		let etype = check_expr env decl.declInit in 

		(* Ensure that declInit and declType match using check_expr *)
		(try
			let _ = 
				(* Error out if local variable with same name already exists. *)
				List.find 
					(fun vdecl -> vdecl.declID = decl.declID) env.symtab.variables
			in raise (Failure ("Duplicate variable " ^ decl.declID))
		with
		| Not_found -> 
			(* TODO: use same symbol table as symbol table from arg *)
			let new_symbol_table = 
				{
					(env.symtab)
					with 
					variables = decl :: env.symtab.variables;
				} in
			let new_env = { (env) with symtab = new_symbol_table; }
			and vdecl = 
				{
					declTyp = decl.declTyp;
					declID = decl.declID;
					declInit = decl.declInit;
				}
			in (new_env, vdecl))
	in	

	(* Return env and stmt tuple. *)
	let rec check_stmt (env : env) (stmt : stmt) :(env * stmt) = 
		print_endline(";checking stmt");
		match stmt with 
			Expr e -> print_endline(";stmt is expr"); ignore(check_expr env e); (env, stmt) (* Expression cannot mutate the environment. *)
		(* Return current env since Blocks have their own scope. *)
		| Block stmt_list -> print_endline(";block");
			let new_symbol_table = { parent = Some env.symtab; variables = []; args = []; } in
			let (_, checked_stmts) = check_stmt_list { (env) with symtab = new_symbol_table; } stmt_list in
			(env, stmt)
		| Decl vdecl -> print_endline(";checking decl"); (* Return new env*)
			let (new_env, vdecl) = check_variable_declaration env vdecl
			in (new_env, stmt)
		| If (e, s1, s2) -> print_endline(";IF");
			let checked_expr = check_expr env e
			and (_, checked_s1) = check_stmt env s1
			and (_, checked_s2) = check_stmt env s2 in
			(env, stmt)
		| For (e1, e2, e3, s) -> 
			let checked_e1 = check_expr env e1
			and checked_e2 = check_expr env e2
			and checked_e3 = check_expr env e3 in
			(env, stmt)
		| While (e, s) -> 
			let checked_expr = check_expr env e 
			and (_, checked_stmt) = check_stmt env s in
			(env, stmt)
		| Return e -> let e_type = check_expr env e in
			(match env.return_type with
				| Some return_type ->
					if e_type = return_type then (env, stmt)
					else raise (Failure ("incorrect return type"))
					| None -> (env, stmt))(*raise (Failure ("no return")))*)
	(* Each statement takes the environment updated from the previous statement. *)
	and check_stmt_list (env : env) ( slist : stmt list ) :(env * stmt list) = 
		print_endline(";checking stmt list");
		let(new_env, stmts) = 
			List.fold_left (fun acc stmt ->
				let (nenv, s) = check_stmt (fst acc) stmt
			  in (nenv, (s :: (snd acc)))) (env, []) slist
		in (new_env, List.rev stmts) 
	in

	(* Check arguments *)
	let check_argdecl (env : env) (adecl : argdecl) = 
		print_endline(";checking arg decl");

		(* An argument cannot have type void. *)
		let check_not_void_arg (adecl : argdecl) = 
			print_endline(";checking void args");
			let arg_typ = (fun a -> a.argdeclType) adecl in
					if arg_typ = Datatype(Void) then raise (Failure ("illegal void arg"))
					else ()
		in 
		ignore(check_not_void_arg (adecl));

		(try
			let _ = 
				(* Error out if local variable with same name already exists. *)
				List.find 
					(fun argdecl -> argdecl.argdeclID = adecl.argdeclID) env.symtab.args
			in raise (Failure (";Duplicate variable " ^ adecl.argdeclID))
		with
		| Not_found -> 
			let new_symbol_table = 
				{
					(env.symtab)
					with 
					args = adecl :: env.symtab.args;
				} in
			print_endline(";arg ct");
			print_endline(";" ^ string_of_int(List.length new_symbol_table.args));
			let new_env = { (env) with symtab = new_symbol_table; }
			and arg = 
				{
					argdeclType = adecl.argdeclType;
					argdeclID = adecl.argdeclID;
				}
			in (new_env, adecl))
	in

	(* Add function declaration to the environment. *)
	let add_function_declaration (env : env) (fdecl : funcdecl) :(env * funcdecl) = 
		print_endline(";adding function declaration to env");
		print_endline(";" ^ fdecl.fname);
		if (List.mem fdecl.fname (List.map (fun f -> f.name) built_in_functions)) then
		raise (Failure ("Cannot overwrite built-in function!!")) else
		(* Get the types of the function's arguments. *)
		let a_types = List.map (fun adecl -> adecl.argdeclType) fdecl.args in
		(* Make a function entry for the function. *)
		let func_entry = 
			{
				name = fdecl.fname;
				arg_types = a_types;
				return_type = fdecl.typ;
			} in
		let new_funcs = func_entry :: env.funcs in
		(* Make a new symbol table for the function scope. *)
		let new_symbol_table = 
			{
				parent = Some env.symtab;
				args = [];
				variables = [];
			} in
		(* Add the function to the environment 
		For now, the symbol table and return type have empty local scope. *)
		let new_env = 
		{
			(env)
			with
			symtab = new_symbol_table;
			funcs =  new_funcs;
			return_type = Some fdecl.typ;
		} in
		print_endline(";func count:");
		print_endline(";" ^ string_of_int(List.length new_env.funcs));
		(* Add the args to the function scope. *)
		let (env_with_args, argdecl_list) = 
			List.fold_left (fun acc argdecl ->
				let (nenv, arg) = check_argdecl (fst acc) argdecl 
				in (nenv, (arg :: (snd acc)))) (new_env, []) fdecl.args in
		let f = 
		{
			typ = fdecl.typ;
			fname = fdecl.fname;
			args = List.rev argdecl_list;
			body = fdecl.body;
		} in
		print_endline(";env with args");
		print_endline(";" ^ string_of_int(List.length env_with_args.symtab.args));
		(* Return the environment with this added function. *)
		({ (env_with_args) with funcs = new_funcs; }, f) 
 	in



	(* Check function declaration and return new environment. *)
	let check_function_declaration (env : env) (fdecl : funcdecl) : (env * funcdecl) =
		print_endline(";checking func decl");
		ignore(print_endline(";func arg count:"));
		ignore(print_endline(";" ^ string_of_int(List.length env.symtab.args)));
		(* No need to keep track of environment outside the scope of the function. *)
		let (_, func_body) = 
			check_stmt_list env fdecl.body in 
		let func_body = func_body in
		(* Return the environment with this added function. *)
		(env, fdecl) 
  	in


	(* Establish initial environment *)
	let env = 
		print_endline("; est init env");
		{
			symtab = { parent = None; variables = []; args = []; };
			funcs = built_in_functions; 
			return_type = None;
		} in

	(* Add global variables to the environment. *)
	let check_global_var (env : env) (vdecl : vardecl) = 
		print_endline(";checking global vars");
		(try
			let _ =	
				(* Error out if global variable with same name already exists. *)
				List.find 
					(fun v -> v.declID = vdecl.declID) env.symtab.variables
			in raise (Failure ("Duplicate variable " ^ vdecl.declID))
		with
			| Not_found -> 
				print_endline(";add global to symbol tab");
				let new_symbol_table = 
					print_endline(";" ^ string_of_int(List.length env.symtab.variables));
					{
						(env.symtab)
						with 
						variables = vdecl :: env.symtab.variables;
					} in
				let new_env = { (env) with symtab = new_symbol_table; }
				and vardecl = 
					{
						declTyp = vdecl.declTyp;
						declID = vdecl.declID;
						declInit = vdecl.declInit;
					}
				in (new_env, vardecl))
	in

	(* Add globals to env. *)
	let(new_env, vars) = 
		print_endline(";globals loop");
		List.fold_left (fun acc v ->
			let (nenv, v) = check_global_var (fst acc) v
			in (nenv, (v :: (snd acc)))) (env, []) globals 
	in 

	print_endline(";after globals run");
	print_endline(";" ^ string_of_int(List.length new_env.symtab.variables));

	ignore(print_endline(";how many fns"));
	ignore(print_endline(";" ^ string_of_int(List.length functions)));

	(* Adding func decl to env, which also adds args to env.*)
	let (new_env, funcs) = 
		print_endline(";ADDING FUNCS");
		print_endline(";" ^ string_of_int(List.length new_env.symtab.variables));
		List.fold_left (fun acc f -> 
			let(nenv, f) = add_function_declaration (fst acc) f
			in (nenv, (f :: (snd acc)))) (new_env, []) functions 
	in
	ignore(print_endline(";after adding funcs"));
	ignore(print_endline(";" ^ string_of_int(List.length env.symtab.args)));


	print_endline(";after adding fns and ARGS");
	print_endline(";" ^ string_of_int(List.length new_env.symtab.variables));

	let (_, fdecl_list) = 
		print_endline(";another one");
		List.fold_left (fun acc fdecl ->
			print_endline(";folding");
			print_endline(";" ^ fdecl.fname);
			let (new_env, f) = check_function_declaration (fst acc) fdecl
			in (new_env, (f :: (snd acc)))) (new_env, []) functions
	in fdecl_list;

	let check_function functions =  
		print_endline(";checking functions");

		(* Return list of functions after checking functions. *)
		functions in

		(* After semantically checking, we return the program -
		a tuple of a list of globals and a list of functions. *)
		(globals, functions);

		(*in List.iter check_function functions*)


