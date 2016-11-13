(* code generation: translate takes semantically checked AST and produces LLVM IR *)

module L = Llvm
module A = Ast
module S = Sast

module StringMap = Map.Make(String)


let i32_t = L.i32_type context;;
let iFl_t = L.double_type context;;
let i8_t = L.i8_type context;;
let i1_t = L.i1_type context;;
let void_t = L.void_type context;;


let translate (globals, functions) =
    let context = L.global_context() in
    let the_module = L.create_module context "Blur" in

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
            let init = L.const_int (ltype_of_int) 0
            in StringMap.add name (L.define_global name init the_module) map in
        List.fold_left global_var StringMap.empty globals in

    (* declare built ins *)

    (* define each function w/ args and return type so we can call it *)
    let function_decls =
        (* FUNCTION function_decl *)
        let function_decl map fdecl =
            let name = fdecl.A.name
            and formal_types = Array.of_list (List.map (fun (typ,_) -> ltype_of_typ typ) fdecl.A.args) in
            (* use sast here prob *)
            let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) map in

        List.fold_left function_decl StringMap.empty functions in

    let codegen_func func_decl =
        let (f, _) = StringMap.find func_decl.A.fname function_decls in
        (* BUILDER here *)
        let llbuilder = L.builder_at_end context (L.entry_block f) in

        (* construct locals, its args and locally declared vars *)
        let local_vars =
            let add_formal map (typ, name) fmls = L.set_value_name name fmls;
            let local = L.build_alloca (ltype_of_typ typ) name llbuilder in
            ignore (L.build_store fmls local llbuilder);
            StringMap.add name local map in

            let add_local map (typ, name) =
                let local_var = L.build_alloca (ltype_of_typ typ) name llbuilder in
               StringMap.add name local_var map in

            let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.args (Array.to_list (L.params f)) in
            List.fold_left add_local formals fdecl.A.locals
        in

        let lookup name = try StringMap.find name local_vars with Not_found -> StringMap.find name global_vars in

        let rec codegen_expr llbuilder = function
            S.IntLit_t i        -> L.const_int i32_t i
          | S.DoubleLit_t i     -> L.const_float f_t i
          | S.StrLit_t s        -> L.build_global_stringptr s "tmp" llbuilder
          | S.CharLit_t c       -> L.const_int i8_t (Char.code c)
          | S.BoolLit_t b       -> if b then const_int i1_t 1 else const_int i1_t 0
          | S.Id_t id           -> L.build_load (lookup id) id llbuilder (* todo: error-checking in lookup *)
          (*| S.Binop_t (e1, op, e2) -> handle_binop e1 op e2 llbuilder *)
        in

        let codegen_return e llbuilder =
            match func_decl.A.typ with
                A.void  -> L.build_ret_void llbuilder
              | _       -> L.build_ret (codegen_expr e) llbuilder
        in
        
        let add_terminal llbuilder f =
            match L.block_terminator (L.insertion_block llbuilder) with
                Some _  -> ()
              | None    -> ignore (f llbuilder)
        in  

        let rec codegen_stmt llbuilder = function
            S.Block_s sl        -> List.fold_left codegen_stmt llbuilder sl
          | S.Expr_s e          -> codegen_expr llbuilder e
          | S.Return_s e        -> codegen_ret e llbuilder
        in
        let builder = codegen_stmt llbuilder (A.block fdecl.A.body)
        in add_terminal builder (match func_decl.A.typ with
                A.void -> L.build_ret_void
              | typ -> L.build_ret (L.const_int (ltype_of_typ typ) 0))
    in

    List.iter codegen_func functions;
    the_module
