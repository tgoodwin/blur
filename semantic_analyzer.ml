
open Ast

module A = Ast

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
    return_type: datatype
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

	let built_in_functions = 
		(* print and println actually take expr, not a Datatype. We deal with this in check_func_call*)
		[ {name = "print"; arg_types = [Datatype(String)]; return_type = Datatype(Void);};
			{name = "println"; arg_types = [Datatype(String)]; return_type = Datatype(Void);};
			{name = "len"; arg_types = [Datatype(String)]; return_type = Datatype(Int);};
			{name = "readGrayscaleImage"; arg_types = [Datatype(String)]; return_type = UnsizedArray(Int, 2);};
			{name = "readColorImage"; arg_types = [Datatype(String)]; return_type = UnsizedArray(Int, 2);};
			{name = "charToIntensity"; arg_types = [Datatype(Char)]; return_type = Datatype(Int);};
			{name = "intensityToChar"; arg_types = [Datatype(Int)]; return_type = Datatype(Char);};
			{name = "intcast"; arg_types = [Datatype(Double)]; return_type = Datatype(Int);};
			{name = "doublecast"; arg_types = [Datatype(Int)]; return_type = Datatype(Double);};
			{name = "canvas"; arg_types = [Datatype(String)]; return_type = UnsizedArray(Char, 2);};
			{name = "dither"; arg_types = [Datatype(String)]; return_type = UnsizedArray(Char, 2);}; ]
	in

	let is_arith (t : datatype) :bool =
		match t with
			| Datatype(Int) | Datatype(Double) -> true
			| _ -> false
	in 

	let is_logical (t : datatype) :bool =
		match t with
		| Datatype(Int) | Datatype(Double) | Datatype(Char) | Datatype(String) | Datatype(Bool) -> true
		| _ -> false
	in

	let is_bool (t : datatype) :bool =
		match t with
		Datatype(Bool) -> true
		| _ -> false
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
		try 
			let decl = List.find (fun vdecl -> vdecl.declID = id) symtab.variables in decl.declTyp
		  with
		  | Not_found -> try let decl = List.find (fun adecl -> adecl.argdeclID = id) symtab.args in decl.argdeclType
		  			with
		  			| Not_found -> (match symtab.parent with 
		  				| Some parent -> get_variable_decl parent id 
		  				| _ -> raise Not_found) in

        let check_arr_access_type symtab s elist =
            let num_dims = (List.length elist) in
            let arr_type =
                (try get_variable_decl symtab s with Not_found -> raise (Failure ("undeclared identifier " ^ s)))
            in
            match arr_type with
              UnsizedArray(p, d) ->
                  if (d > num_dims) then
                      UnsizedArray(p, d - num_dims)
                  else if (d < num_dims) then
                      raise (Failure ("Array accessing more dimensions than exist"))
                  else
                      Datatype(p)
            | SizedArray(p, dl) ->
                    let tot_dims = (List.length dl) in
                    if (tot_dims > num_dims) then
                        let result_dim = (List.nth dl (tot_dims - num_dims)) in
                        SizedArray(p, [result_dim])
                    else if (tot_dims < num_dims) then
                        raise (Failure ("Array accessing more dimensions than exist"))
                    else
                        Datatype(p)
                            
        in

	(* Returns datatype of expression. *)
	let rec check_expr (env : env) (expr : expr) = 
		match expr with 
		  IntLit i -> Datatype(Int)
		| DoubleLit d -> Datatype(Double)
		| CharLit c -> Datatype(Char)
		| StrLit s -> Datatype(String)
		| BoolLit b -> Datatype(Bool)
		| Noexpr -> Datatype(Void)
		| ArrayListInit elist -> check_arr_literal env elist
		| ArrayAccess (s, elist) -> check_arr_access_type env.symtab s elist
			(* Check that you're accessing something available*)
		| Id s ->
				(* This gets the type of the variable. *)
				(try get_variable_decl env.symtab s 
				with | Not_found -> raise (Failure ("undeclared identifier " ^ s))
				) 
		| Unop (op, e) ->
			let t = check_expr env e in
			(match op with 
			| Mag -> 
				if (t <> Datatype(Char) && t <> Datatype(Int)) then raise (Failure("illegal operation")) 
				else 
					(if t = Datatype(Int) then Datatype(Char)
					else Datatype(Int)) 
			| Not -> if t <> Datatype(Bool) then raise (Failure("illegal operation"))
				else Datatype(Bool)
			| Neg -> if (t <> Datatype(Int) && t <> Datatype(Double)) then raise (Failure("illegal operation"))
				else t
			| _ -> raise(Failure("illegal unop")))
		| FuncCall (s, arglist) -> check_func_call s arglist env
		| Binop (e1, op, e2) -> 
			let t1 = check_expr env e1 
			and t2 = check_expr env e2 in
			match op with 
			| Add | Sub | Mult | Div | Mod -> 
				if is_arith t1 && t1 = t2 then t1
				else raise (Failure ("illegal operation")) 
			| Lt | Leq | Gt | Geq | Eq | Neq | And | Or ->
				if is_logical t1 && t1 = t2 then Datatype(Bool)
				else raise (Failure("invalid operands"))
			| Asn ->
				if t1 = t2 then t1
				else raise (Failure ("illegal assignment")) 	

        (* get type of an expr list, potentailly nested. Blur supports up to 2D arrays. *)
        and check_arr_literal env elist =
            let tot_dims = match (List.hd elist) with
              ArrayListInit(el) -> 2
            | _                 -> 1
            in let data_typ = match (List.hd elist) with
              ArrayListInit(el) -> check_expr env (List.hd el)
            | e                 -> check_expr env e
            in let prim_typ = match data_typ with Datatype(p) -> p in UnsizedArray(prim_typ, tot_dims)

	(* Checking function call returns the type of the function. *)
	and check_func_call (id : string) (args : expr list) (env : env) = 
		try
			let func_entry = List.find (fun f -> f.name = id) env.funcs in
			(* Get the types of the arg expressions. *)
			let arg_types = List.map(fun arg -> check_expr env arg) args in
			(* Ensure that arguments match. *)
			if List.length func_entry.arg_types <> List.length args then
			raise (Failure ("Incorrect number of args for function call " ^ id ^ 
				". Expecting " ^ (string_of_int (List.length func_entry.arg_types)) ^ " args but got "
				^ (string_of_int (List.length args)))) else 
			if id <> "print" && id <> "println" && id <> "len" && arg_types <> func_entry.arg_types then
			raise (Failure("unexpected arg types")) else
			func_entry.return_type
		with | Not_found -> raise (Failure ("undeclared function " ^ id))
	in	

	let var_add (env : env) (decl : vardecl) =
		let etype = check_expr env decl.declInit in 
		if etype = decl.declTyp || decl.declInit = Noexpr then (* declInit must be same type as declTyp. *)
			(try
				let _ = 
					(* Error out if local variable with same name already exists. *)
					List.find 
						(fun vdecl -> vdecl.declID = decl.declID) env.symtab.variables
				in raise (Failure ("Duplicate variable " ^ decl.declID))
			with
			| Not_found -> 
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
			else raise (Failure("variable declaration type mismatch")) 
	in 

	(* Add ArrayListInit as declInit of vardecl to env. *)
	let adding_arr (env : env) (decl : vardecl) (p : primitive) =
		(try
			let _ = 
				(* Error out if local variable with same name already exists. *)
				List.find 
					(fun vdecl -> vdecl.declID = decl.declID) env.symtab.variables
			in raise (Failure ("Duplicate variable " ^ decl.declID))
		with
		| Not_found -> 
			let new_symbol_table = 
				{
					(env.symtab)
					with 
					variables = decl :: env.symtab.variables;
				} in
			let new_env = { (env) with symtab = new_symbol_table; }
			and vdecl = 
				{
					declTyp = decl.declTyp; (* UnsizedArray(p, int) *)
					declID = decl.declID;
					declInit = decl.declInit; (* ArrayListInit(elist) *)
				}
			in (new_env, vdecl))
    in

    (* Add array when it is initialized by a function that returns an array. *)
    let adding_arr_func_call (env : env) (decl : vardecl) (p : primitive) =
		(try
			let _ = 
				(* Error out if local variable with same name already exists. *)
				List.find 
					(fun vdecl -> vdecl.declID = decl.declID) env.symtab.variables
			in raise (Failure ("Duplicate variable " ^ decl.declID))
		with
		| Not_found -> 
			let new_symbol_table = 
				{
					(env.symtab)
					with 
					variables = decl :: env.symtab.variables;
				} in
			let new_env = { (env) with symtab = new_symbol_table; }
			and vdecl = 
				{
					declTyp = decl.declTyp; (* UnsizedArray(p, int) *)
					declID = decl.declID;
					declInit = decl.declInit; (* ArrayListInit(elist) *)
				}
			in (new_env, vdecl))
    in

	(* When an unsized array is declared, the RHS must be 
	an ArrayListInit, or a function that returns ArrayListInit. *)
	let var_add_arr (env : env) (decl : vardecl) (p : primitive) =
		match decl.declInit with
		| ArrayListInit(elist) -> adding_arr env decl p
		| FuncCall(s, elist) -> adding_arr_func_call env decl p
		| _ -> raise (Failure("illegal array initialization"))
	in 		

	let check_variable_declaration (env : env) (decl: vardecl) = 

		(* A variable cannot have type void. *)
		let check_not_void_var (decl : vardecl) = 
			let var_typ = (fun v -> decl.declTyp) decl in
					if var_typ = Datatype(Void) then raise (Failure ("illegal void variable " ^ decl.declID))
					else ()
		in 
		ignore(check_not_void_var (decl));

		match decl.declTyp with
		| UnsizedArray(p,d) -> 
			if decl.declInit = Noexpr then raise(Failure("unsized array must be initialized"))
			else var_add_arr env decl p
		| SizedArray(p, intlist) -> if decl.declInit = Noexpr then adding_arr env decl p else raise (Failure("illegal array initialization"))
		| _ -> var_add env decl

	in	

	(* Return env and stmt tuple. *)
	let rec check_stmt (env : env) (stmt : stmt) :(env * stmt) = 
		match stmt with 
			Expr e -> ignore(check_expr env e); (env, stmt) (* Expression cannot mutate the environment. *)
		(* Return current env since Blocks have their own scope. *)
		| Block stmt_list ->
			let new_symbol_table = { parent = Some env.symtab; variables = []; args = []; } in
			let (_, checked_stmts) = check_stmt_list { (env) with symtab = new_symbol_table; } stmt_list in
			(env, stmt)
		| Decl vdecl -> (* Return new env*)
			let (new_env, vdecl) = check_variable_declaration env vdecl
			in (new_env, stmt)
		| If (e, s1, s2) ->
			let checked_expr = check_expr env e
			and (_, checked_s1) = check_stmt env s1
			and (_, checked_s2) = check_stmt env s2 in
			if is_bool checked_expr then (env, stmt)
			else raise(Failure("illogical if"))
		| For (e1, e2, e3, s) -> 
			let checked_e1 = check_expr env e1
			and checked_e2 = check_expr env e2
			and checked_e3 = check_expr env e3 in
			if is_bool checked_e2 then (env, stmt)
			else raise(Failure("illogical for"))
		| While (e, s) -> 
			let checked_expr = check_expr env e 
			and (_, checked_stmt) = check_stmt env s in
			if is_bool checked_expr then (env, stmt)
			else raise(Failure("illogical while")) 
		| Return e -> let e_type = check_expr env e in
			match env.return_type with
				| return_type ->
					if e_type = return_type then (env, stmt)
					else raise (Failure ("incorrect return type"))
	(* Each statement takes the environment updated from the previous statement. *)
	and check_stmt_list (env : env) ( slist : stmt list ) :(env * stmt list) = 
		let(new_env, stmts) = 
			List.fold_left (fun acc stmt ->
				let (nenv, s) = check_stmt (fst acc) stmt
			  in (nenv, (s :: (snd acc)))) (env, []) slist
		in (new_env, List.rev stmts) 
	in

	(* Check arguments *)
	let check_argdecl (env : env) (adecl : argdecl) = 

		(* An argument cannot have type void. *)
		let check_not_void_arg (adecl : argdecl) = 
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
		if fdecl.fname="main" && (List.length fdecl.args) > 0 
			then raise (Failure("main() may not take args")) else
		if (List.mem fdecl.fname (List.map (fun f -> f.name) built_in_functions)) then
		raise (Failure ("Cannot overwrite built-in function!!")) else
		if (List.mem fdecl.fname (List.map (fun f -> f.name) env.funcs)) then
		raise (Failure ("Duplicate function.")) else
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
			return_type = fdecl.typ;
		} in
		(* Add the args to the function scope. *)
		let (env_with_args, argdecl_list) = 
			List.fold_left (fun acc argdecl ->
				let (nenv, arg) = check_argdecl (fst acc) argdecl 
				in (nenv, (arg :: (snd acc)))) (new_env, []) fdecl.args in
		let (_, func_body) = 
			check_stmt_list env_with_args fdecl.body in
		let func_body = func_body in
		let f = 
		{
			typ = fdecl.typ;
			fname = fdecl.fname;
			args = List.rev argdecl_list;
			body = func_body;
		} in
		(* Return the environment with this added function. *)
		({ (env_with_args) with funcs = new_funcs; }, f) 
 	in

	(* Establish initial environment *)
	let env = 
		{
			symtab = { parent = None; variables = []; args = []; };
			funcs = built_in_functions; 
			return_type = Datatype(Int);
		} in

	(* Add global variables to the environment. *)
	let check_global_var (env : env) (vdecl : vardecl) = 
		(try
			let _ =	
				(* Error out if global variable with same name already exists. *)
				List.find 
					(fun v -> v.declID = vdecl.declID) env.symtab.variables
			in raise (Failure ("Duplicate variable " ^ vdecl.declID))
		with
			| Not_found -> 
				let new_symbol_table = 
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
		List.fold_left (fun acc v ->
			let (nenv, v) = check_global_var (fst acc) v
			in (nenv, (v :: (snd acc)))) (env, []) globals 
	in 

	(* Adding func decl to env, which also adds args to env.*)
	let (new_env, funcs) = 
		List.fold_left (fun acc f -> 
			let(nenv, f) = add_function_declaration (fst acc) f
			in (nenv, (f :: (snd acc)))) (new_env, []) (List.rev functions) 
	in

	let env_func_names = List.map (fun f -> f.name) env.funcs in

	let (new_env, funcs) = 
		try 
		let _ = List.find (fun func -> func.name = "main") new_env.funcs in
		(new_env, funcs)
		with | Not_found -> raise (Failure("no main"));
	in
	 
	let check_function functions =  

		(* Return list of functions after checking functions. *)
		functions in

		(* After semantically checking, we return the program -
		a tuple of a list of globals and a list of functions. *)
		(globals, functions);