open Ast 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Asn -> "="

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Void -> "void"
  | Array(t) -> string_of_typ t ^ "[]"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | DoubleLit(l) -> string_of_float l
  | StrLit(l) -> l
  | CharLit(l) -> Char.escaped l
  | BoolLit(l) -> string_of_bool l
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | ArrayListInit(l) -> "{" ^ String.concat ", " (List.map string_of_expr l) ^ "}"
  | ArraySizeInit(t, s) -> string_of_typ t ^ "[" ^ string_of_int s ^ "]"

(*let string_of_vardecl vdecl = 
    string_of_typ vdecl.declTyp ^ " " ^
    vdecl.declID ^ ";\n" ^
    string_of_expr vdecl.declInit*)
let string_of_vardecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"  


let string_of_funcdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ "filler" ^ ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_prog (vars, funcs) = 
    String.concat "" (List.map string_of_vardecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_funcdecl funcs)
