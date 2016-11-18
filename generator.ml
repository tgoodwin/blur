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
    let string_t = L.pointer_type i8_t in

    (* TODO: Array, Canvas *)
    let rec ltype_of_typ = function
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

        let local_vars = StringMap.empty in

        let llbuilder = L.builder_at_end context (L.entry_block f) in

        let int_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in
        
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
                
        (* helper to get the raw string from an ID expression type *)
        and id_to_str e =
            match e with
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

       (* TODO: Unop, Asn, ArrayListInit, CanvasInit, Noexpr *) 
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
          | A.FuncCall ("print", [e; typ])    -> codegen_print e typ locals llbuilder
          | A.FuncCall (n, el)          -> codegen_call n el (locals, llbuilder)
        

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
              | _               -> (codegen_asn name init_expr local_vars llbuilder); local_vars, llbuilder


            (*let alloca = build_lloca decl.declTyp decl.declID llbuilder*)

        (* used to add a branch instruction to a basic block only if one doesn't already exist *)
        and add_terminal llbuilder f =
            match L.block_terminator (L.insertion_block llbuilder) with
                Some _  -> ()
              | None    -> ignore (f llbuilder)

        (* build instructions in the given builder for the statement,
         * return the builder for where the next instruction should be placed *)
        (* TODO: If, For, While, Continue, Break *)
        and codegen_stmt (locals, llbuilder) = function
            A.Block sl        -> List.fold_left codegen_stmt (locals, llbuilder) sl
          | A.Decl e          -> codegen_vdecl e (local_vars, llbuilder)
          | A.Expr e          -> ignore (codegen_expr (locals, llbuilder) e); locals, llbuilder
          | A.Return e        -> ignore (codegen_return e (locals, llbuilder)); locals, llbuilder

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
