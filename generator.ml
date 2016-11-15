(* code generation: translate takes semantically checked AST and produces LLVM IR *)

module L = Llvm
module A = Ast
(*module S = Sast*)


module StringMap = Map.Make(String)



let translate (globals, functions) =
    let context = L.global_context() in
    let the_module = L.create_module context "Blur" in
    
    let i32_t = L.i32_type context
    and iFl_t = L.double_type context
    and i8_t = L.i8_type context
    and i1_t = L.i1_type context
    and void_t = L.void_type context
    in

    let ltype_of_typ=  function
        A.Int -> i32_t
      | A.Double -> iFl_t
      | A.Char -> i8_t
      | A.Bool -> i1_t
      | A.Void -> void_t
    in
    
    let global_vars =
        (* FUNCTION global_var *)
        let global_var map (typ, name) =
            let init = L.const_int (ltype_of_typ typ) 0
            in StringMap.add name (L.define_global name init the_module) map in
        List.fold_left global_var StringMap.empty globals in

    (* declare built ins *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    (* define each function w/ args and return type so we can call it *)
    let function_decls =
        (* FUNCTION function_decl *)
        let function_decl map fdecl =
            let name = fdecl.A.fname
            and formal_types = Array.of_list (List.map (fun (typ,_) -> ltype_of_typ typ) fdecl.A.args) in
            (* use sast here prob *)
            let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) map in

        List.fold_left function_decl StringMap.empty functions in

    let codegen_func func_decl =
        let (f, _) = StringMap.find func_decl.A.fname function_decls in

        (* INIT INSTRUCTION BUILDER *)
        let llbuilder = L.builder_at_end context (L.entry_block f) in

        (* construct locals, its args and locally declared vars *)
        let local_vars =
            let add_formal map (typ, name) fmls = L.set_value_name name fmls;
            let local = L.build_alloca (ltype_of_typ typ) name llbuilder in
            ignore (L.build_store fmls local llbuilder);
            StringMap.add name local map in

            let add_local map (vdecl: A.vardecl) =
                let local_var = L.build_alloca (ltype_of_typ vdecl.declTyp) vdecl.declID llbuilder in
               StringMap.add vdecl.declID local_var map in

            let formals = List.fold_left2 add_formal StringMap.empty func_decl.A.args (Array.to_list (L.params f)) in
            List.fold_left add_local formals func_decl.A.locals
        in

        (* semantic checking ensures this will always be found *)
        let lookup name = try StringMap.find name local_vars with Not_found -> StringMap.find name global_vars in

        let func_lookup fname =
            match (L.lookup_function fname the_module) with
            (*None        -> raise (exception LLVMFunctionNotFound fname)*)
            Some f      -> f
        in

(*
        let codegen_print el llbuilder =
            let printf = func_lookup "printf" in
            let params = List.map (

        let handle_binop e1 opr e2 llbuilder ..... in *)
        (* let codegen_func_call f e llbuilder ..... in *)

        (* blur built-ins 
        let codegen_call f el llbuilder = function
            "print"     -> codegen_print el llbuilder
        in *)

        let rec codegen_expr llbuilder = function
            A.IntLit i        -> L.const_int i32_t i
          | A.DoubleLit i     -> L.const_float iFl_t i
          | A.StrLit s        -> L.build_global_stringptr s "tmp" llbuilder
          | A.CharLit c       -> L.const_int i8_t (Char.code c)
          | A.BoolLit b       -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
          | A.Id id           -> L.build_load (lookup id) id llbuilder (* todo: error-checking in lookup *)
          (*| S.Binop_t (e1, op, e2) -> handle_binop e1 op e2 llbuilder *)
          (*| S.FuncCall_t f el    -> codegen_call f el llbuilder *)
        in

        (* handle return statements *)
        let codegen_return e llbuilder =
            match func_decl.A.typ with
            A.Void  -> L.build_ret_void llbuilder
          | _       -> L.build_ret (codegen_expr llbuilder e) llbuilder
        in
        
        (* used to add a branch instruction to a basic block only if one doesn't already exist *)
        let add_terminal llbuilder f =
            match L.block_terminator (L.insertion_block llbuilder) with
                Some _  -> ()
              | None    -> ignore (f llbuilder)
        in  

        (* build instructions in the given builder for the statement,
         * return the builder for where the next instruction should be placed *)
        let rec codegen_stmt llbuilder = function
            A.Block sl        -> List.fold_left codegen_stmt llbuilder sl
          | A.Expr e          -> ignore (codegen_expr llbuilder e); llbuilder
          | A.Return e        -> ignore (codegen_return e); llbuilder
        in

        (* build the code for each statement in the function *)
        let builder = codegen_stmt llbuilder (A.Block func_decl.A.body)
        in add_terminal builder (match func_decl.A.typ with
                A.Void -> L.build_ret_void
              | typ -> L.build_ret (L.const_int (ltype_of_typ typ) 0))
    in

    List.iter codegen_func functions;
    the_module
