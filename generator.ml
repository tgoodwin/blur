(* code generation: translate takes semantically checked AST and produces LLVM IR *)

module L = Llvm

open Ast
open Sast

module StringMap = Map.Make(String)

let context = L.global_context()
let the_module = L.create_module context "Blur"

let i32_t = L.i32_type context;;
let iFl_t = L.double_type context;;
let i8_t = L.i8_type context;;
let i1_t = L.i1_type context;;
let void_t = L.void_type context;;



let ltype_of_typ = function
    Ast.Int -> i32_t
  | Ast.Double -> iFl_t
  | Ast.Char -> i8_t
  | Ast.Bool -> i1_t
  | Ast.Void -> void_t

let global_var map (typ, name) =
    let init = L.const_int (ltype_of_int) 0
    in StringMap.add name (L.define_global name init the_module) map

let function_decl map fdecl =
    let name = fdecl.Ast.name
    and formal_types =
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.Ast.args) in
    let ftype = L.function_type (ltype_of_typ fdecl.Ast.typ) formal_types in
    StringMap.add name (L.define_function name ftype the_module, fdecl) map

let init_params func formals =
    let formals = Array.of_list (formals) in



let codegen_function fdecl =
    let (f, _) = StringMap.find fdecl.Ast.name function_decls in
    let builder = L.builder_at_end context (L.entry_block f) in

    let _ = init_params f fdecl.Ast.args

    let local_vars =
        let add_formal map (typ, name) param = L.set_value_name name param;

        let local = L.build_alloca (ltype_of_typ t) name builder in
        ignore (L.build_store param local buiklder);
        StringMap.add name local map in

        let add_local map (typ, name) =
            let local_var = L.build_alloca (ltype_of_typ t) name builder
            in StringMap.add name local_var map in

        let formals =
            List.fold_left2 add_formal StringMap.empty fdecl.Ast.args
                (Array.to_list (L.params f)) in
                List.fold_left add_local formals fdecl.Ast.locals

        (*TODO with a scope that uses local_vars and global_vars*)

let codegen_main (globals, functions) =
    let global_vars = List.fold_left global_var StringMap.empty globals in
    (* declare built in functions *)
    let function_decls = List.fold_left function_decl StringMap.empty functions in

    the_module
