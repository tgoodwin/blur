open Ast 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> '+'
  | Sub -> '-'
  | Mul -> '*'
  | Div -> '/'

let string_of_expr = function
	  Lit(l) -> string_of_int l

let string_of_typ = function
	  Int -> "int"
  | Bool -> "bool"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

(* calls the individual print functinos of the data members of a variable_Declaration type *)
let print_string_of_var_decl vd = function
    _ -> "dummy"

(* this will handle all the data members of the function type call their print functions individually *)
let print_string_of_func_decl fd = function
    _ -> "fun dummy"

let print_string_of_decl = function
    VarDecl(vd) -> print_string_of_var_decl vd
  | FuncDecl(fd) -> print_string_of_func_decl fd

let print_string_of_program prog =
    List.map print_string_of_decls prog 

let string_of_prog prog =
	(*print_endline(String.concat "" (List.map string_of_vdecl vars)) *)
	print_endline ("hello")
