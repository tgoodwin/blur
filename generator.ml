(* code generation: translate takes semantically checked AST and produces LLVM IR *)

open Ast
open Llvm
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

    (* TODO: String, Array, Canvas *)
    let ltype_of_typ=  function
        A.Int -> i32_t
      | A.Double -> iFl_t
      | A.Char -> i8_t
      | A.Bool -> i1_t
      | A.Void -> void_t
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

        let llbuilder = L.builder_at_end context (L.entry_block f) in

        let int_format_str = L.build_global_stringptr "%d\n" "fmt" llbuilder in

        (* construct locals, its args and locally declared vars *)
        let local_vars =
            let add_formal map (typ, name) fmls = L.set_value_name name fmls;
            let local = L.build_alloca (ltype_of_typ typ) name llbuilder in
            ignore (L.build_store fmls local llbuilder);
            StringMap.add name local map in

            (* this is not yet handling any initialized values *)
            let add_local map (vdecl: A.vardecl) =
                let typ = vdecl.declTyp in
                let name = vdecl.declID in
                let local_var = L.build_alloca (ltype_of_typ typ) name llbuilder in
               StringMap.add name local_var map in

            let rec translate_stmt = function
                    Decl vdecl -> vdecl
                in

            let formals = List.fold_left2 add_formal StringMap.empty func_decl.A.args (Array.to_list (L.params f)) in
            List.fold_left add_local formals (List.map translate_stmt func_decl.body)
        in

        (* semantic checking ensures this will always be found *)
        let rec lookup name = try StringMap.find name local_vars with Not_found -> StringMap.find name global_vars in

        (*and func_lookup fname =
            match (L.lookup_function fname the_module) with
            (*None        -> raise (exception LLVMFunctionNotFound fname)*)
            Some f      -> f *)

(**)
        let rec codegen_binop e1 op e2 llbuilder =
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
                let lh = codegen_expr llbuilder e1
                and rh = codegen_expr llbuilder e2
                in int_ops lh op rh
            in

            let handle_binop opr =
                match opr with
                A.Asn         -> codegen_asn e1 e2 llbuilder
              | _             -> arith_binop e1 op e2

            in
            handle_binop op
                
        and codegen_asn n e llbuilder =
            let gen_e = (codegen_expr llbuilder e) in
            ignore(L.build_store gen_e (lookup n) llbuilder); gen_e

        and codegen_print e llbuilder =
            let param = (codegen_expr llbuilder e) in
            L.build_call printf_func [| int_format_str; param |] "printf" llbuilder

        (*let handle_binop e1 opr e2 llbuilder ..... in *)
        (* let codegen_func_call f e llbuilder ..... in *)
        
        (* blur built-ins  *)
        and codegen_call f el llbuilder =
            let (fdef, fdecl) = StringMap.find f function_decls in
            let args = List.rev (List.map (codegen_expr llbuilder) (List.rev el)) in
            let result = (match func_decl.A.typ with
                A.Void  -> ""
              | _       -> f ^ "_result" )
            in L.build_call fdef (Array.of_list args) result llbuilder
       (* TODO: Binio, Unop, Asn, ArrayListInit, CanvasInit, Noexpr *) 
        and codegen_expr llbuilder = function
            A.IntLit i        -> L.const_int i32_t i
          | A.DoubleLit i     -> L.const_float iFl_t i
          | A.StrLit s        -> L.build_global_stringptr s "tmp" llbuilder
          | A.CharLit c       -> L.const_int i8_t (Char.code c)
          | A.BoolLit b       -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
          | A.Id id           -> L.build_load (lookup id) id llbuilder (* todo: error-checking in lookup *)
          (*| A.Asn(n, e)       -> codegen_asn n e llbuilder *)
          | A.Binop(e1, op, e2) -> codegen_binop e1 op e2 llbuilder 
          | A.FuncCall ("print", [el])    -> codegen_print el llbuilder
          | A.FuncCall (n, el)          -> codegen_call n el llbuilder
        

        (* handle return statements *)
        and codegen_return e llbuilder =
            match func_decl.A.typ with
            A.Void  -> L.build_ret_void llbuilder
          | _       -> L.build_ret (codegen_expr llbuilder e) llbuilder
        
        (* handle variable declarations *)
        and codegen_decl decl llbuilder =
            match decl.declInit with 
                Noexpr -> ()
            (* TODO KG - do stuff here *)
            (*let alloca = build_alloca decl.declTyp decl.declID llbuilder*)

        (* used to add a branch instruction to a basic block only if one doesn't already exist *)
        and add_terminal llbuilder f =
            match L.block_terminator (L.insertion_block llbuilder) with
                Some _  -> ()
              | None    -> ignore (f llbuilder)

        (* build instructions in the given builder for the statement,
         * return the builder for where the next instruction should be placed *)
        (* TODO: If, For, While, Continue, Break *)
        and codegen_stmt llbuilder = function
            A.Block sl        -> List.fold_left codegen_stmt llbuilder sl
          | A.Decl e          -> ignore (codegen_decl e llbuilder); llbuilder
          | A.Expr e          -> ignore (codegen_expr llbuilder e); llbuilder
          | A.Return e        -> ignore (codegen_return e llbuilder); llbuilder

        (* build the code for each statement in the function *)
        in
        let llbuilder = codegen_stmt llbuilder (A.Block func_decl.A.body)
        in add_terminal llbuilder (match func_decl.A.typ with
                A.Void -> L.build_ret_void
              | typ -> L.build_ret (L.const_int (ltype_of_typ typ) 0))
    in

    List.iter codegen_func functions;
    the_module
