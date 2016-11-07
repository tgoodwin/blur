open Ast

(* open Sast *)

type symbol_table = {
    parent: symbol_table option;
    variables: var_decl list
}

(* see slide 73 of types lecture *)
type func_type = {
    name: string
    input_types: typ list;
    return_type: typ
}

type translation_env = {
    return_type: typ option;
    scope: symbol_table;
    functions: func_type list;
    return_type: typ
}
