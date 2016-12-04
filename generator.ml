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
    and void_t = L.void_type context in

    let string_t = L.pointer_type i8_t in
    let array_t = L.array_type in
    let zero_t = L.const_int i32_t 0 in


    (* THIS DOESNT WORK TODO TODO *)
    let rec ltype_of_unsized_array t d =
        match d with
          3 -> array_t (array_t (array_t (ltype_of_typ (Datatype(t))) 0) 0) 0
        | 2 -> array_t (array_t (ltype_of_typ (Datatype(t))) 0) 0
        | 1 -> array_t (ltype_of_typ (Datatype(t))) 0 

    and ltype_of_sized_array t el = (*get_array_pointer (Datatype(t)) (List.length el) *)
        match (List.length el) with
            3 -> array_t (array_t (array_t (ltype_of_typ (Datatype(t))) (List.nth el 2)) (List.nth el 1)) (List.nth el 0)
          | 2 -> array_t (array_t (ltype_of_typ (Datatype(t))) (List.nth el 1)) (List.nth el 0)
          | 1 -> array_t (ltype_of_typ (Datatype(t))) (List.hd el)
    
    and get_array_pointer typ dims =
        let lltype = ltype_of_typ typ in
        match dims with
          1 -> L.pointer_type lltype
        | 2 -> L.pointer_type (L.pointer_type lltype)
        | 3 -> L.pointer_type (L.pointer_type (L.pointer_type lltype))

    and ltype_of_typ (d: A.datatype) = match d with
        Datatype(A.Int) -> i32_t
      | Datatype(A.Double) -> iFl_t
      | Datatype(A.Char) -> i8_t
      | Datatype(A.String) -> string_t
      | Datatype(A.Bool) -> i1_t
      | Datatype(A.Void) -> void_t
      | UnsizedArray(t, d) -> ltype_of_unsized_array t d
      | SizedArray(t, el)  -> ltype_of_sized_array t el
      | _            -> raise (Exceptions.NotADatatype)

    and ltype_of_primitive (p: A.primitive) = match p with
      A.Int     -> i32_t
    | A.Double  -> iFl_t
    | A.Char    -> i8_t
    | A.String  -> string_t
    | A.Bool    -> i1_t
    | A.Void    -> void_t

    (* literals *)
    and ltype_of_literal e = match e with
      A.IntLit i  -> i32_t
    | A.DoubleLit d -> iFl_t
    | A.CharLit c   -> i8_t
    | A.StrLit s    -> string_t
    | A.BoolLit b   -> i1_t
    | _             -> raise (Exceptions.NotALiteral)
    in

    let global_vars =
        (* FUNCTION global_var *)
        let global_var map (vdecl : A.vardecl) =
            let typ = vdecl.declTyp in
            let name = vdecl.declID in

            let init = L.const_int (ltype_of_typ typ) 0
            in StringMap.add name (L.define_global name init the_module) map in
        List.fold_left global_var StringMap.empty globals in

    (* DECLARE EXTERNAL LIBRARY FUNCTIONS *)
    let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
    let printf_func = L.declare_function "printf" printf_t the_module in

    let foo_t = L.var_arg_function_type i32_t [| i32_t |] in
    let foo_func = L.declare_function "foo" foo_t the_module in

    (* ---- end externals ------ *)

    (* define each function w/ args and return type so we can call it *)
    let function_decls =
        (* FUNCTION function_decl *)
        let function_decl map fdecl =
            let name = fdecl.A.fname
            and formal_types = Array.of_list (List.map (fun (typ) -> ltype_of_typ typ.argdeclType) fdecl.A.args) in
            (* use sast here prob *)
            let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) map in

        List.fold_left function_decl StringMap.empty functions in

    let codegen_func func_decl =
        let (f, _) = StringMap.find func_decl.A.fname function_decls in

        let llbuilder = L.builder_at_end context (L.entry_block f) in

        (* format strings *)
        let int_format_str = L.build_global_stringptr "%d\n" "int_fmt" llbuilder
        and str_format_str = L.build_global_stringptr "%s\n" "str_fmt" llbuilder
        and chr_format_str = L.build_global_stringptr "%c\n" "chr_fmt" llbuilder
        and flt_format_str = L.build_global_stringptr "%f\n" "flt_fmt" llbuilder
        in


        (* MAPS MAPS MAPS FAKE GLOBAL MAPS *)
        let local_vars = StringMap.empty in
        let arr_dims = StringMap.empty in

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

        (* --- ARRAY HELPER FUNCTIONS --- *)
        (* return a list of integers as dimensions *)
        let get_arrliteral_dims inputlist =
            let rec insert_at_end l i =
                match l with
                 [] -> [i]
               | h::t -> h:: (insert_at_end t i)
            in

            let rec get_length l dim_list =
                let dim = List.length l in
                let dim = L.const_int i32_t dim in
                insert_at_end dim_list dim

            in
            get_length inputlist []
        in

        let add_arrdim id dimension_list arr_dims =
            StringMap.add id dimension_list arr_dims
        in

        (* --- END OF ARRAY HELPER FUNCTIONS --- *)

        (* Only add each function's args for now, will add to map when we encounter a varDecl in the functions body,
         * which is a statement list *)

        let local_vars = List.fold_left2 add_formal local_vars (List.map (fun (t) -> (t.argdeclType, t.argdeclID)) func_decl.A.args) (Array.to_list (L.params f)) in

        let maps = (local_vars, arr_dims) in

        (* see if a variable has been declared already *)
        let rec lookup name locals =
            try StringMap.find name locals
            with Not_found -> try StringMap.find name global_vars
            with Not_found -> raise (Exceptions.UnknownVariable name)
        in

        let lookup_arr name arr_map =
            try StringMap.find name arr_map
            with Not_found -> raise (Exceptions.UninitializedArray name)
        in

        (*and func_lookup fname =
            match (L.lookup_function fname the_module) with
            (*None        -> raise (exception LLVMFunctionNotFound fname)*)
            Some f      -> f *)

         let rec codegen_binop e1 op e2 maps llbuilder =
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
            let float_ops lh op rh =
                match op with
                A.Add   -> L.build_fadd lh rh "flt_addtmp" llbuilder
              | A.Sub   -> L.build_fsub lh rh "flt_subtmp" llbuilder
              | A.Mult  -> L.build_fmul lh rh "flt_multmp" llbuilder
              | A.Div   -> L.build_fdiv lh rh "flt_divtmp" llbuilder
              | A.Eq    -> L.build_fcmp Fcmp.Oeq lh rh "flt_eqtmp" llbuilder
              | A.Neq   -> L.build_fcmp Fcmp.One lh rh "flt_neqtmp" llbuilder
              | A.Lt    -> L.build_fcmp Fcmp.Ult lh rh "flt_lesstmp" llbuilder
              | A.Leq   -> L.build_fcmp Fcmp.Ole lh rh "flt_leqtmp" llbuilder
              | A.Gt    -> L.build_fcmp Fcmp.Ogt lh rh "flt_sgttmp" llbuilder
              | A.Geq   -> L.build_fcmp Fcmp.Oge lh rh  "flt_sgetmp" llbuilder
              | _       -> raise (Exceptions.FloatOpNotSupported)
            in

            let arith_binop e1 op e2 =
                let lh = codegen_expr (maps, llbuilder) e1
                and rh = codegen_expr (maps, llbuilder) e2
                in
                (* peanut brittle! *)
                let op_typ = L.string_of_lltype (L.type_of lh) in match op_typ with
                  "i32" -> int_ops lh op rh
                | "double" -> float_ops lh op rh
            in
            let assign_binop e1 e2 =
                let arr_dims = (snd maps) in
                let id_str = (id_to_str e1) in
                (* if the assignment involves array dimensions, update the arr_dims map *)
                (* i.e. int[][] a = int[5][10] *)
                let arr_dims = (snd maps) (*= update_array_map id_str e2 arr_dims (maps, llbuilder) *) in
                let maps = ((fst maps), arr_dims) in
                match e1 with
                  A.Id s        -> codegen_asn s (codegen_expr (maps, llbuilder) e2) maps llbuilder
                | A.ArrayAccess(n, dl) -> codegen_asn_arr e1 e2 maps llbuilder
                | _                    -> raise (Exceptions.NotAssignable)
            in

            let handle_binop e1 op e2 =
                match op with
                A.Asn         -> assign_binop e1 e2
              | _             -> arith_binop e1 op e2

            in
            handle_binop e1 op e2
                
        (* TODO: type handling for float, etc *)
        and codegen_unop op e maps llbuilder =
            let exp = (codegen_expr (maps, llbuilder)) e in
            match op with
            A.Neg       -> L.build_neg exp "int_unoptmp" llbuilder
          | A.Not       -> L.build_not exp "bool_unoptmp" llbuilder

        (* helper to get the raw string from an ID expression type. MOVE TO A UTILS FILE *)
        and id_to_str id = match id with
            A.Id s      -> s
          | A.ArrayAccess(n, dl) -> n
          | _           -> raise Exceptions.NotAnId

        (* ASSIGN an expression (value) to a declared variable *)
        (*and update_array_map name e arr_map (maps, llbuilder) =
            match e with
              A.ArrayListInit(el)    -> let arr_dim = get_arrliteral_dims el in add_arrdim name arr_dim arr_map
            | A.ArraySizeInit(t, dl) -> add_arrdim name (List.map (codegen_expr (maps, llbuilder)) dl) arr_map
            | _                      -> arr_map *)
        

        and codegen_asn_arr e1 e2 maps llbuilder =
            let gen_e1 = (match e1 with
              A.ArrayAccess(n, dl)    -> build_array_access n dl maps llbuilder true
              | _                       -> raise Exceptions.IllegalAssignment)
            in 
            let gen_e2 = codegen_expr (maps, llbuilder) e2 in
            ignore(L.build_store gen_e2 gen_e1 llbuilder); gen_e2

        and codegen_asn n gen_e maps llbuilder =
            let locals = fst maps in
            ignore(L.build_store gen_e (lookup n locals) llbuilder); gen_e

        and codegen_print e maps llbuilder =
            let param = (codegen_expr (maps, llbuilder) e) in
            let theType = L.string_of_lltype (L.type_of param) in
            (* ignore(print_endline("; " ^ theType)); *)
            (* ignore(print_endline("; " ^ L.string_of_llvalue param)); *)
            let fmt_str = match theType with
              "i32"     -> int_format_str
            | "double"  -> flt_format_str
            | "i8"      -> chr_format_str
            | "i8*"     -> str_format_str
            | "i1"      -> int_format_str
            in
            L.build_call printf_func [| fmt_str; param |] "printf" llbuilder

        (* blur built-ins  *)
        and codegen_call f el (maps, llbuilder) =
            let (fdef, fdecl) = StringMap.find f function_decls in
            let args = List.rev (List.map (codegen_expr (maps, llbuilder)) (List.rev el)) in
            let result = (match func_decl.A.typ with
                A.Datatype(A.Void)  -> ""
              | _       -> f ^ "_result" )
            in L.build_call fdef (Array.of_list args) result llbuilder

        and codegen_expr (maps, llbuilder) e =
            match e with 
            A.IntLit i        -> L.const_int i32_t i
          | A.DoubleLit i     -> L.const_float iFl_t i
          | A.StrLit s        -> L.build_global_stringptr s "tmp" llbuilder
          | A.CharLit c       -> L.const_int i8_t (Char.code c)
          | A.BoolLit b       -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
          | A.Id id           -> L.build_load (lookup id (fst maps)) id llbuilder (* todo: error-checking in lookup *)
          | A.Binop(e1, op, e2) -> codegen_binop e1 op e2 maps llbuilder
          | A.Unop(op, e)       -> codegen_unop op e maps llbuilder
          (* --- built in functions --- *)
          | A.FuncCall ("print", [e])    -> codegen_print e maps llbuilder
          | A.FuncCall ("len", [arr])     -> L.const_int i32_t (L.array_length (L.type_of(codegen_expr (maps, llbuilder) arr)))
          | A.FuncCall ("foo", [e])     -> L.build_call foo_func [| (codegen_expr (maps, llbuilder) e) |] "foo" llbuilder
          (* --- end built-ins --- *)
          | A.FuncCall (n, el)          -> codegen_call n el (maps, llbuilder)
          | A.ArrayListInit el          -> build_array_of_list el (maps, llbuilder)
          | A.ArrayAccess(n, dl)        -> build_array_access n dl maps llbuilder false
          | A.Noexpr            -> L.const_int i32_t 0
        

        (* codegen_vdecl: handle variable declarations *)
        and codegen_vdecl (vdecl: A.vardecl) (maps, llbuilder) =

            let get_expr_type_then_add (prim, dims) (vdecl: A.vardecl) locals =
                let gen_e = codegen_expr(maps, llbuilder) vdecl.declInit in
                let ptr_typ = get_array_pointer (Datatype(prim)) dims in
                let exp_typ = (L.type_of gen_e) in
                let local_var = L.build_alloca ptr_typ vdecl.declID llbuilder in
                (StringMap.add vdecl.declID local_var locals), gen_e
            in

            (*let rec dynamic_sized_array t el locals =
                let array_type =
                if (List.length el) = 2 then
                    let dim1 = codegen_expr (maps, llbuilder) (List.nth el 0)
                    and dim2 = codegen_expr (maps, llbuilder) (List.nth el 1) in
                    ignore(print_endline("; ok here" ^ L.string_of_llvalue dim1));
                    (*let dim1 = int_of_string (L.string_of_llvalue dim1)
                    and dim2 = int_of_string (L.string_of_llvalue dim2) in *)
                    array_t (array_t (ltype_of_typ (Datatype(t))) 3) 3
                else
                    let dim1 = codegen_expr (maps, llbuilder) (List.hd el) in
                    (*let dim1 = int_of_string (L.string_of_llvalue dim1) in *)
                    ignore(print_endline("; ok here" ^ L.string_of_llvalue dim1));
                    array_t (ltype_of_typ (Datatype(t))) 5
                in let local_var = L.build_alloca array_type vdecl.declID llbuilder in
                StringMap.add vdecl.declID local_var locals
            in *)


            let local_vars = (fst maps) in
            match vdecl.declTyp with
              A.UnsizedArray(p, d) ->
                let local_vars, exp = get_expr_type_then_add (p, d) vdecl local_vars in
                let maps = (local_vars, (snd maps)) in
                ignore(codegen_asn vdecl.declID exp maps llbuilder); maps, llbuilder
            (*| A.SizedArray(t, el) ->
                ignore(print_endline("fuck"));
                let local_vars = dynamic_sized_array t el local_vars in
                let maps = (local_vars, (snd maps)) in
                (match vdecl.declInit with
                  A.Noexpr -> maps, llbuilder
                | e        -> let exp = (codegen_expr (maps, llbuilder) e) in ignore(codegen_asn vdecl.declID exp maps llbuilder); maps, llbuilder) *)
            | _         ->
                let local_vars = add_local vdecl local_vars in
                let maps = (local_vars, (snd maps)) in
                match vdecl.declInit with
                  A.Noexpr      -> maps, llbuilder
                | e             -> let exp = (codegen_expr (maps, llbuilder) e) in ignore(codegen_asn vdecl.declID exp maps llbuilder); maps, llbuilder


        (* BUILD n-dimensional array from Literal *)
        and build_array_of_list el (maps, llbuilder) =
            let llvalues = List.map (codegen_expr (maps, llbuilder)) el in
            let len = List.length llvalues in
            let typ = (L.type_of (List.hd llvalues)) in
            let cool_array = Array.of_list llvalues in
            let res = L.const_array (array_t i32_t len) cool_array in
            ignore(print_endline("; " ^ L.string_of_lltype (L.type_of res))); res

       (* ----------------------------- *)
            
        and build_array_access name idx_list maps llbuilder isAssign =
            let idx_list = List.map (codegen_expr (maps, llbuilder)) idx_list in
            let idx_list = (L.const_int i32_t 0)::[]@idx_list in

            let idx_arr = Array.of_list idx_list in
            if isAssign then
                L.build_gep (lookup name (fst maps)) idx_arr name llbuilder
            else
                L.build_load (L.build_gep (lookup name (fst maps)) idx_arr name llbuilder) name llbuilder

        (* s is expected to be the ID expression of an already declared array *)
        and build_array_reference s dims (maps, llbuilder) =
            let id = id_to_str s in
            let s_val = (lookup id (fst maps)) in
            match dims with
              1 -> L.build_in_bounds_gep s_val [| zero_t; zero_t |] id llbuilder
            | 2 -> L.build_in_bounds_gep s_val [| zero_t; zero_t; zero_t |] id llbuilder
            | 3 -> L.build_in_bounds_gep s_val [| zero_t; zero_t; zero_t; zero_t |] id llbuilder

        (* used to add a branch instruction to a basic block only if one doesn't already exist *)
        and codegen_conditional pred then_stmt else_stmt (maps, llbuilder) =
            let bool_val = (codegen_expr (maps, llbuilder) pred) in

            let merge_bb = L.append_block context "merge" f in
            let then_bb = L.append_block context "then" f in
            let then_builder = (L.builder_at_end context then_bb) in
            let then_tup = (codegen_stmt (maps, then_builder) then_stmt) in
            add_terminal (snd then_tup) (L.build_br merge_bb);

            let else_bb = L.append_block context "else" f in
            let else_builder = (L.builder_at_end context else_bb) in
            let else_tup = (codegen_stmt (maps, else_builder) else_stmt) in
            add_terminal (snd else_tup) (L.build_br merge_bb);
            ignore (L.build_cond_br bool_val then_bb else_bb llbuilder);
            L.builder_at_end context merge_bb

        (* WHILE LOOP *)
        and codegen_while pred body (maps, llbuilder) =
            let pred_bb = L.append_block context "while" f in
            ignore (L.build_br pred_bb llbuilder);

            let body_bb = L.append_block context "while_body" f in
            add_terminal (snd (codegen_stmt (maps, (L.builder_at_end context body_bb)) body)) (L.build_br pred_bb);

            let pred_builder = L.builder_at_end context pred_bb in
            let bool_val = (codegen_expr (maps, pred_builder) pred) in
            
            let merge_bb = L.append_block context "merge" f in
            ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
            L.builder_at_end context merge_bb

        (* FOR LOOP: todo - figure out if scoping behaves right *)
        and codegen_for e1 e2 e3 body (maps, llbuilder) =
            codegen_stmt (maps, llbuilder) (A.Block [A.Expr e1; A.While (e2, A.Block [body; A.Expr e3])])

        and add_terminal llbuilder f =
            match L.block_terminator (L.insertion_block llbuilder) with
              Some _ -> ()
            | None      -> ignore (f llbuilder)

        and codegen_return ret_e (maps, llbuilder) =
            match func_decl.A.typ with
              A.Datatype(A.Void)        -> L.build_ret_void llbuilder
            | A.UnsizedArray(p, d)      -> L.build_ret (build_array_reference ret_e d (maps, llbuilder)) llbuilder
            | _                         -> L.build_ret (codegen_expr (maps, llbuilder) ret_e) llbuilder
           
        (* build instructions in the given builder for the statement,
         * return the builder for where the next instruction should be placed *)
        (* TODO: Continue, Break *)
        and codegen_stmt (maps, llbuilder) = function
            A.Block sl              -> List.fold_left codegen_stmt (maps, llbuilder) sl
          | A.Decl e                -> codegen_vdecl e (maps, llbuilder)
          | A.Expr e                -> ignore (codegen_expr (maps, llbuilder) e); maps, llbuilder
          | A.Return e              -> ignore (codegen_return e (maps, llbuilder)); maps, llbuilder
          | A.If(p, s1, s2)         -> let builder = (codegen_conditional p s1 s2 (maps, llbuilder)) in maps, builder
          | A.While(p, body)        -> let builder = (codegen_while p body (maps, llbuilder)) in maps, builder
          | A.For(e1, e2, e3, body) -> codegen_for e1 e2 e3 body (maps, llbuilder)

        (* build the code for each statement in the function *)
        in
        let tuple = codegen_stmt (maps, llbuilder) (A.Block func_decl.A.body) in
        let llbuilder = (snd tuple) in
        add_terminal llbuilder (match func_decl.A.typ with
                A.Datatype(A.Void) -> L.build_ret_void
              | typ -> L.build_ret (L.const_int (ltype_of_typ typ) 0))
    in
    List.iter codegen_func functions;
    the_module
