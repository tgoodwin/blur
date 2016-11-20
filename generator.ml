(* code generation: translate takes semantically checked AST and produces LLVM IR *)

open Ast
open Llvm
open Exceptions
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

    let string_t = L.pointer_type i8_t
    and i8ptr_t = L.pointer_type i8_t
    and i32ptr_t = L.pointer_type i32_t
    in

    (* TODO: Array, Canvas *)
    (* and a get_ptr_type function for arrays *)
    let ltype_of_typ = function
        A.Int -> i32_t
      | A.Double -> iFl_t
      | A.Char -> i8_t
      | A.String -> string_t
      | A.Bool -> i1_t
      | A.Void -> void_t
      (* | A.Array t -> (ltype_of_typ t) *)
    in
    
    let global_vars =
        (* FUNCTION global_var *)
        let global_var map (vdecl : A.vardecl) =
            let typ = vdecl.declTyp in
            let name = vdecl.declID in

            let init = L.const_int (ltype_of_typ typ) 0
            in StringMap.add name (L.define_global name init the_module) map in
        List.fold_left global_var StringMap.empty globals in

    (* declare built ins *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    let memset_t = L.function_type void_t [| i8ptr_t; i32_t; i32_t |] in
    let memset = L.declare_function "memset" memset_t the_module in

    (* instructions for 2D matrix configuration *)
    let sizeof_offset   = L.const_int i32_t (-1)
    and length_offset   = L.const_int i32_t (-2)
    and cols_offset     = L.const_int i32_t (-3)
    and rows_offset     = L.const_int i32_t (-4)
    (* byte sizes for blur primitives *)
    and int_size        = L.const_int i32_t 4
    and double_size     = L.const_int i32_t 8
    and one_32t         = L.const_int i32_t 1
    and zero_32t        = L.const_int i32_t 0
    in

    let get_size_of_typ = function
        A.Int           -> int_size
      | A.Double        -> double_size
      | A.Char          -> one_32t
      | A.Bool          -> one_32t (* bools are stored as 1 byte blocks, cus padding *)
    in

    let get_dim from_ptr item llbuilder =
        let loc = L.build_bitcast from_ptr i32ptr_t "dim" llbuilder in
        let loc = L.build_gep loc [| item |] "dim" llbuilder in
        L.build_load loc "dim" llbuilder in

    let put_dim from_ptr item the_val llbuilder =
        let loc = L.build_bitcast from_ptr i32ptr_t "dim" llbuilder in
        let loc = L.build_gep loc [| item |] "dim" llbuilder in
        L.build_store the_val loc llbuilder
    in

    (* STORAGE ALLOCATION INSTRUCTIONS for all ARRAY data structures *)

    let get_header loc llbuilder =
        let char_ptr = L.build_bitcast loc i8ptr_t "new" llbuilder in
        L.build_gep char_ptr [| (L.const_int i8_t (-16)) |] "new" llbuilder
    in

    let get_body loc llbuilder =
        let char_ptr = L.build_bitcast loc i8ptr_t "new" llbuilder in
        L.build_gep char_ptr [| (L.const_int i8_t (16)) |] "new" llbuilder
    in

    let build_block size llbuilder = 
        let ch_ptr = L.build_array_alloca i8_t size "new" llbuilder in
        ignore (L.build_call memset [| ch_ptr; zero_32t; size |] "" llbuilder); ch_ptr
    in

    let build_arr1D len size llbuilder =
        let size = L.build_mul len size "new" llbuilder in (* total sequential size *)
        let alloc_size = L.build_add size (L.const_int i32_t 16) "new" llbuilder in (* add 16 byte header *)
        let char_ptr = build_block alloc_size llbuilder in
        let arr_ptr = get_body char_ptr llbuilder in
        ignore (put_dim arr_ptr sizeof_offset alloc_size llbuilder);
        ignore (put_dim arr_ptr length_offset len llbuilder);
        ignore (put_dim arr_ptr rows_offset zero_32t llbuilder); (* ROWS, COLUMNS starting at 0 for now *)
        ignore (put_dim arr_ptr cols_offset zero_32t llbuilder);
        L.build_bitcast arr_ptr i32ptr_t "new" llbuilder
    in
    
    (* takes in an Ast type and row, col values *)
    let build_arr2D primtyp row col llbuilder =
        let typ_size = (get_size_of_typ primtyp) in
        let len = L.build_mul row col "new" llbuilder in
        let arr_ptr = build_arr1D len typ_size llbuilder in
        ignore (put_dim arr_ptr rows_offset row llbuilder);
        ignore (put_dim arr_ptr cols_offset col llbuilder);
        L.build_bitcast arr_ptr (L.pointer_type (ltype_of_typ primtyp))  "new" llbuilder
    in

    (* --- ARRAY ACCESS --- *)

    let build_put from_ptr offset value llbuilder =
        let loc = L.build_gep from_ptr [| offset |] "putarr" llbuilder in
        L.build_store value loc llbuilder in

    let build_get from_ptr offset llbuilder =
        let loc = L.build_gep from_ptr [| offset |] "getarr" llbuilder in
        L.build_load loc "getarr" llbuilder in

    let build_putrc from_ptr row col value llbuilder =
        let offset = get_dim from_ptr cols_offset llbuilder in
        let offset = L.build_mul row offset "getrc" llbuilder in
        let offset = L.build_add col offset "getrc" llbuilder in
        build_put from_ptr offset value llbuilder in

    let build_getrc from_ptr row col llbuilder =
        let offset = get_dim from_ptr cols_offset llbuilder in
        let offset = L.build_mul row offset "get" llbuilder in
        let offset = L.build_add col offset "get" llbuilder in
        build_get from_ptr offset llbuilder in

    (* --- FUNCTION DECLARATIONS --- *)
    let function_decls =
        (* FUNCTION function_decl *)
        let function_decl map fdecl =
            let name = fdecl.A.fname
            and formal_types = Array.of_list (List.map (fun (typ,_) -> ltype_of_typ typ) fdecl.A.args) in
            (* use sast here prob *)
            let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) map in

        List.fold_left function_decl StringMap.empty functions in

    (* --- CODEGEN FUNCTION BODY --- *)
    let codegen_func func_decl =
        let (f, _) = StringMap.find func_decl.A.fname function_decls in

        let llbuilder = L.builder_at_end context (L.entry_block f) in
        let local_vars = StringMap.empty in

        let add_formal map (typ, name) fml =
            L.set_value_name name fml;
            let local = L.build_alloca (ltype_of_typ typ) name llbuilder in
            ignore (L.build_store fml local llbuilder);
            StringMap.add name local map
        in
        let add_local (vdecl: A.vardecl) local_vars =
            let typ = vdecl.declTyp in
            let name = vdecl.declID in
            let local_var = L.build_alloca (ltype_of_typ typ) name llbuilder in
            StringMap.add name local_var local_vars
        in

        (* Only add each function's args for now, will add to map when we encounter a varDecl in the functions body,
         * which is a statement list *)

        let local_vars = List.fold_left2 add_formal local_vars func_decl.A.args (Array.to_list (L.params f)) in

        (* see if a variable has been declared already *)
        let rec lookup name locals =
            try StringMap.find name locals
            with Not_found -> try StringMap.find name global_vars
            with Not_found -> raise (Exceptions.UnknownVariable name)
        in

        (*and func_lookup fname =
            match (L.lookup_function fname the_module) with
            (*None        -> raise (exception LLVMFunctionNotFound fname)*)
            Some f      -> f *)

         let rec codegen_binop e1 op e2 locals llbuilder =
            let int_ops lh op rh  =
                match op with
                A.Add   -> L.build_add lh rh "tmp" llbuilder
              | A.Sub   -> L.build_sub lh rh "tmp" llbuilder
              | A.Mult  -> L.build_mul lh rh "tmp" llbuilder
              | A.Div   -> L.build_sdiv lh rh "tmp" llbuilder
              | A.And   -> L.build_and lh rh "tmp" llbuilder
              | A.Or    -> L.build_or lh rh "tmp" llbuilder
              | A.Eq    -> L.build_icmp Icmp.Eq lh rh "tmp" llbuilder
              | A.Neq   -> L.build_icmp Icmp.Ne lh rh "tmp" llbuilder
              | A.Lt    -> L.build_icmp Icmp.Slt lh rh "tmp" llbuilder
              | A.Leq   -> L.build_icmp Icmp.Sle lh rh "tmp" llbuilder
              | A.Gt    -> L.build_icmp Icmp.Sgt lh rh "tmp" llbuilder
              | A.Geq   -> L.build_icmp Icmp.Sge lh rh "tmp" llbuilder

            in
            let arith_binop e1 op e2 =
                let lh = codegen_expr (locals, llbuilder) e1
                and rh = codegen_expr (locals, llbuilder) e2
                in int_ops lh op rh
            in

            let handle_binop e1 op e2 =
                match op with
                A.Asn         -> codegen_asn (id_to_str e1) e2 locals llbuilder
              | _             -> arith_binop e1 op e2

            in
            handle_binop e1 op e2
                
        (* TODO: type handling for float, etc *)
        and codegen_unop op e locals llbuilder =
            let exp = (codegen_expr (locals, llbuilder)) e in
            match op with
            A.Neg       -> L.build_neg exp "int_unoptmp" llbuilder
          | A.Not       -> L.build_not exp "bool_unoptmp" llbuilder

        (* helper to get the raw string from an ID expression type. MOVE TO A UTILS FILE *)
        and id_to_str = function
            A.Id s      -> s

        (* ASSIGN an expression (value) to a declared variable *)
        and codegen_asn n e locals llbuilder =
            let gen_e = codegen_expr (locals, llbuilder) e in
            ignore(L.build_store gen_e (lookup n locals) llbuilder); gen_e

        and codegen_print e typ locals llbuilder =
            let param = (codegen_expr (locals, llbuilder) e) in
            let format_str = (codegen_expr (locals, llbuilder) typ) in (* should return string literal*)
            L.build_call printf_func [| format_str; param |] "printf" llbuilder

        (* let codegen_func_call f e llbuilder ..... in *)
        
        (* blur built-ins  *)
        and codegen_call f el (locals, llbuilder) =
            let (fdef, fdecl) = StringMap.find f function_decls in
            let args = List.rev (List.map (codegen_expr (locals, llbuilder)) (List.rev el)) in
            let result = (match func_decl.A.typ with
                A.Void  -> ""
              | _       -> f ^ "_result" )
            in L.build_call fdef (Array.of_list args) result llbuilder


       (* -------------- EXPRESSIONS --------------------*)
       (* TODO: ArrayListInit, CanvasInit, Noexpr *) 
        and codegen_expr tup e =
            let locals = fst tup and llbuilder = snd tup in
            match e with
            A.IntLit i        -> L.const_int i32_t i
          | A.DoubleLit i     -> L.const_float iFl_t i
          | A.StrLit s        -> L.build_global_stringptr s "tmp" llbuilder
          | A.CharLit c       -> L.const_int i8_t (Char.code c)
          | A.BoolLit b       -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
          | A.Id id           -> L.build_load (lookup id locals) id llbuilder (* todo: error-checking in lookup *)
          | A.Binop(e1, op, e2) -> codegen_binop e1 op e2 locals llbuilder
          | A.Unop(op, e)       -> codegen_unop op e locals llbuilder
          | A.FuncCall ("print", [e; typ])    -> codegen_print e typ locals llbuilder
          | A.FuncCall (n, el)          -> codegen_call n el (locals, llbuilder)
          (* | A.Noexpr            -> () ??? *)
        

        (* codegen_return: handle return statements *)
        and codegen_return e (locals, llbuilder) =
            match func_decl.A.typ with
            A.Void  -> L.build_ret_void llbuilder
          | _       -> L.build_ret (codegen_expr (locals, llbuilder) e) llbuilder
        
        (* codegen_vdecl: handle variable declarations *)
        and codegen_vdecl (vdecl: A.vardecl) (local_vars, llbuilder) =
            let name = vdecl.declID in

            (* add to local_vars map no matter what. if already exists, policy is to overwrite *)
            let local_vars = add_local vdecl local_vars in

            (* if vdecl contains an expression, codegen that expr and assign the variable to it.
             * note that codegen_asn is called after add_local local_vars vdecl. *)
            let init_expr = vdecl.declInit in
            match init_expr with 
                A.Noexpr        -> local_vars, llbuilder
              | e               -> (codegen_asn name e local_vars llbuilder); local_vars, llbuilder



        (* used to add a branch instruction to a basic block only if one doesn't already exist *)
        and codegen_conditional pred then_stmt else_stmt (locals, llbuilder) =
            let bool_val = (codegen_expr (locals, llbuilder) pred) in

            let merge_bb = L.append_block context "merge" f in
            let then_bb = L.append_block context "then" f in
            let then_builder = (L.builder_at_end context then_bb) in
            let then_tup = (codegen_stmt (locals, then_builder) then_stmt) in
            add_terminal (snd then_tup) (L.build_br merge_bb);

            let else_bb = L.append_block context "else" f in
            let else_builder = (L.builder_at_end context else_bb) in
            let else_tup = (codegen_stmt (locals, else_builder) else_stmt) in
            add_terminal (snd else_tup) (L.build_br merge_bb);
            ignore (L.build_cond_br bool_val then_bb else_bb llbuilder);
            L.builder_at_end context merge_bb

        (* WHILE LOOP: todo - figure out if scoping behaves right *)
        and codegen_while pred body (locals, llbuilder) =
            let pred_bb = L.append_block context "while" f in
            ignore (L.build_br pred_bb llbuilder);
            let body_bb = L.append_block context "while_body" f in
            let while_builder = (L.builder_at_end context body_bb) in
            add_terminal (snd (codegen_stmt (locals, while_builder) body)) (L.build_br pred_bb);

            let pred_builder = L.builder_at_end context pred_bb in
            let bool_val = (codegen_expr (locals, pred_builder) pred) in
            
            let merge_bb = L.append_block context "merge" f in
            ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
            L.builder_at_end context merge_bb

        (* FOR LOOP: todo - figure out if scoping behaves right *)
        and codegen_for e1 e2 e3 body (locals, llbuilder) =
            codegen_stmt (locals, llbuilder) (A.Block [A.Expr e1; A.While (e2, A.Block [body; A.Expr e3])])

        and add_terminal llbuilder f =
            match L.block_terminator (L.insertion_block llbuilder) with
                Some _  -> ()
              | None    -> ignore (f llbuilder)

        (* build instructions in the given builder for the statement,
         * return the builder for where the next instruction should be placed *)
        (* TODO: Continue, Break *)
        and codegen_stmt (locals, llbuilder) = function
            A.Block sl              -> List.fold_left codegen_stmt (locals, llbuilder) sl
          | A.Decl e                -> codegen_vdecl e (locals, llbuilder)
          | A.Expr e                -> ignore (codegen_expr (locals, llbuilder) e); locals, llbuilder
          | A.Return e              -> ignore (codegen_return e (locals, llbuilder)); locals, llbuilder
          | A.If(p, s1, s2)         -> ignore (codegen_conditional p s1 s2 (locals, llbuilder)); locals, llbuilder
          | A.While(p, body)        -> ignore (codegen_while p body (locals, llbuilder)); locals, llbuilder
          | A.For(e1, e2, e3, body) -> codegen_for e1 e2 e3 body (locals, llbuilder)

        (* build the code for each statement in the function *)
        in
        let tuple = codegen_stmt (local_vars, llbuilder) (A.Block func_decl.A.body) in
        let llbuilder = (snd tuple) in
        add_terminal llbuilder (match func_decl.A.typ with
                A.Void -> L.build_ret_void
              | typ -> L.build_ret (L.const_int (ltype_of_typ typ) 0))
    in

    List.iter codegen_func functions;
    the_module
