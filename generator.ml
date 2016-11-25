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
    and array_t = L.array_type in


    let rec ltype_of_array datatype = match datatype with
        Arraytype(t) -> ltype_of_primitive (Datatype(t))
    
    and ltype_of_primitive (d: A.datatype) = match d with
        Datatype(A.Int) -> i32_t
      | Datatype(A.Double) -> iFl_t
      | Datatype(A.Char) -> i8_t
      | Datatype(A.String) -> string_t
      | Datatype(A.Bool) -> i1_t
      | Datatype(A.Void) -> void_t
      | Arraytype(t) -> ltype_of_array (Arraytype(t))

    and ltype_of_p (p: A.primitive) = match p with
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
    in
    (*let get_size_of_typ = function
        A.Int           -> int_size
      | A.Double       -> double_size
      | A.Char          -> one_32t
      | A.Bool          -> one_32t
    in *)

    let global_vars =
        (* FUNCTION global_var *)
        let global_var map (vdecl : A.vardecl) =
            let typ = vdecl.declTyp in
            let name = vdecl.declID in

            let init = L.const_int (ltype_of_primitive typ) 0
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
            and formal_types = Array.of_list (List.map (fun (typ) -> ltype_of_primitive typ.argdeclType) fdecl.A.args) in
            (* use sast here prob *)
            let ftype = L.function_type (ltype_of_primitive fdecl.A.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) map in

        List.fold_left function_decl StringMap.empty functions in

    let codegen_func func_decl =
        let (f, _) = StringMap.find func_decl.A.fname function_decls in

        let llbuilder = L.builder_at_end context (L.entry_block f) in

        (* MAPS MAPS MAPS FAKE GLOBAL MAPS *)
        let local_vars = StringMap.empty in
        let arr_dims = StringMap.empty in

        let add_formal map (typ, name) fml =
            L.set_value_name name fml;
            let local = L.build_alloca (ltype_of_primitive typ) name llbuilder in
            ignore (L.build_store fml local llbuilder);
            StringMap.add name local map
        in
        let add_local (vdecl: A.vardecl) local_vars =
            let typ = vdecl.declTyp in
            let name = vdecl.declID in
            let local_var = L.build_alloca (ltype_of_primitive typ) name llbuilder in
            StringMap.add name local_var local_vars
        in

        (* return a list of integers *)
        let get_arrliteral_dims inputlist =
            let rec insert_at_end l i =
                match l with
                 [] -> [i]
               | h::t -> h:: (insert_at_end t i)
            in

            let rec get_length l dim_list =
                let dim = List.length l in insert_at_end dim_list dim

            in
            get_length inputlist []
        in

        let add_arrdim id dimension_list arr_dims =
            StringMap.add id dimension_list arr_dims
        in

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
            let arith_binop e1 op e2 =
                let lh = codegen_expr (maps, llbuilder) e1
                and rh = codegen_expr (maps, llbuilder) e2
                in int_ops lh op rh
            in

            let handle_binop e1 op e2 =
                match op with
                A.Asn         -> codegen_asn (id_to_str e1) e2 maps llbuilder
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

        (* ASSIGN an expression (value) to a declared variable *)

        and codegen_asn n e maps llbuilder =
            let locals = fst maps and arrdims = snd maps in
            let gen_e = codegen_expr (maps, llbuilder) e in
            ignore(L.build_store gen_e (lookup n locals) llbuilder); gen_e

        and codegen_print e typ maps llbuilder =
            let param = (codegen_expr (maps, llbuilder) e) in
            let format_str = (codegen_expr (maps, llbuilder) typ) in (* should return string literal*)
            L.build_call printf_func [| format_str; param |] "printf" llbuilder

        (* blur built-ins  *)
        and codegen_call f el (maps, llbuilder) =
            let (fdef, fdecl) = StringMap.find f function_decls in
            let args = List.rev (List.map (codegen_expr (maps, llbuilder)) (List.rev el)) in
            let result = (match func_decl.A.typ with
                A.Datatype(A.Void)  -> ""
              | _       -> f ^ "_result" )
            in L.build_call fdef (Array.of_list args) result llbuilder

       (* TODO: ArrayListInit, CanvasInit, Noexpr *) 
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
          | A.FuncCall ("print", [e; typ])    -> codegen_print e typ maps llbuilder
          | A.FuncCall (n, el)          -> codegen_call n el (maps, llbuilder)
          | A.ArrayListInit el          -> build_array_of_list el (maps, llbuilder)
          | A.ArraySizeInit (t, dl)     ->  build_array_blocks t dl maps llbuilder
          (*| A.ArrayAccess(n, dl)        -> (* build_array_access *) () *)
          | A.Noexpr            -> L.const_int i32_t 0
        

        (* codegen_vdecl: handle variable declarations *)
        and codegen_vdecl (vdecl: A.vardecl) (maps, llbuilder) =

            (* add to local_vars map no matter what. if already exists, policy is to overwrite *)
            let local_vars = add_local vdecl (fst maps) in
            let arr_dims = (snd maps) in

            (* if array, keep track of its dimensions in the dimensions map*)
            let arr_dims =
                match vdecl.declInit with
                  A.ArrayListInit(el)      -> let arr_dim = get_arrliteral_dims el in add_arrdim vdecl.declID arr_dim arr_dims
                | A.ArraySizeInit(t, dl)   -> add_arrdim vdecl.declID dl arr_dims
                | _                        -> arr_dims

            in let maps = (local_vars, arr_dims) in

            let init_expr = vdecl.declInit in
            match init_expr with
              A.Noexpr      -> maps, llbuilder
            | e             -> ignore(codegen_asn vdecl.declID e maps llbuilder); maps, llbuilder


        (* BUILD 1D literal list *)
        and build_array_of_list el (maps, llbuilder) =
            let len = List.length el in
            let hd = List.hd el in
            let typ = ltype_of_literal (List.hd el) in
            let gen_list = List.map (codegen_expr (maps, llbuilder)) el in
            let arr = Array.of_list gen_list in
            L.const_array (array_t typ len) arr
            
        (* returns a pointer to a sequential memory region *)
        and build_array_blocks t dl maps llbuilder =
            let total_cells = List.fold_left (fun prod e -> prod * e) 1 dl in
            let total_cells = L.const_int i32_t total_cells in
            let typ = ltype_of_p t in
            let type_size = L.build_intcast (L.size_of typ) i32_t "tmp" llbuilder in
            let total_size = L.build_mul type_size total_cells "tmp" llbuilder in

            let arr = L.build_array_alloca typ total_size "tmp" llbuilder in
            let arr_ptr = L.build_pointercast arr (pointer_type typ) "tmp" llbuilder in
            arr_ptr

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

        (* WHILE LOOP: todo - figure out if scoping behaves right *)
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
           
        (* build instructions in the given builder for the statement,
         * return the builder for where the next instruction should be placed *)
        (* TODO: Continue, Break *)
        and codegen_stmt (maps, llbuilder) = function
            A.Block sl              -> List.fold_left codegen_stmt (maps, llbuilder) sl
          | A.Decl e                -> codegen_vdecl e (maps, llbuilder)
          | A.Expr e                -> ignore (codegen_expr (maps, llbuilder) e); maps, llbuilder
          | A.Return e              -> ignore(match func_decl.A.typ with
                                        A.Datatype(A.Void)      -> L.build_ret_void llbuilder
                                      | _                       -> L.build_ret (codegen_expr (maps, llbuilder) e) llbuilder); maps, llbuilder

          | A.If(p, s1, s2)         -> let builder = (codegen_conditional p s1 s2 (maps, llbuilder)) in maps, builder
          | A.While(p, body)        -> let builder = (codegen_while p body (maps, llbuilder)) in maps, builder
          | A.For(e1, e2, e3, body) -> codegen_for e1 e2 e3 body (maps, llbuilder)

        (* build the code for each statement in the function *)
        in
        let tuple = codegen_stmt (maps, llbuilder) (A.Block func_decl.A.body) in
        let llbuilder = (snd tuple) in
        add_terminal llbuilder (match func_decl.A.typ with
                A.Datatype(A.Void) -> L.build_ret_void
              | typ -> L.build_ret (L.const_int (ltype_of_primitive typ) 0))
    in
    List.iter codegen_func functions;
    the_module
