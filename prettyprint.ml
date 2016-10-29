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

let string_of_prog =
	(*print_endline(String.concat "" (List.map string_of_vdecl vars)) *)
	print_endline ("hello")
