(* code generation: translate takes semantically checked AST and produces LLVM IR *)

module L = Llvm

open Ast
open Sast

module StringMap = Map.Make(String)


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
  | Ast.Void -> void_t;;

let translate (globals, functions) =
    let context = L.global_context() in
    let the_module = L.create_module context "Blur" in

    let ltype_of_typ=  function
        Ast.Int -> i32_t
      | Ast.Double -> iFl_t
      | Ast.Char -> i8_t
      | Ast.Bool -> i1_t
      | Ast.Void -> void_t
    in
    
    let global_vars =
        (* FUNCTION global_var *)
        let global_var map (typ, name) =
            let init = L.const_int (ltype_of_int) 0
            in StringMap.add name (L.define_global name init the_module) map in
        List.fold_left global_var StringMap.empty globals in

    (* declare built ins *)
    (* etc etc *)

    (* define each function w/ args and return type so we can call it *)

    let function_decls =
        (* FUNCTION function_decl *)
        let function_decl map fdecl =
            let name = fdecl.Ast.name
            and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.Ast.args) in
            let ftype = L.function_type (ltype_of_typ fdecl.Ast.typ) formal_types in
            StringMap.add name (L.define_function name ftype the_module, fdecl) map in

        List.fold_left function_decl StringMap.empty functions;

    in

    the_module
