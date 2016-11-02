open Ast 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> '+'
  | Sub -> '-'
  | Mul -> '*'
  | Div -> '/'

let string_of_expr = function
	  Lit(l) -> string_of_int l

let rec string_of_expr = function
    Lit(l) -> string_of_int l
  | Id(s) -> s

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"   

let string_of_typ = function
	  Int -> "int"
  | Bool -> "bool"
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_prog (vars, func)  =
	String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl func)
