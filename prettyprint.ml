open Ast 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Asn -> "="
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Leq -> "<="
  | Gt -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop = function
    Not -> "!"
  | Neg -> "-"

let rec string_of_typ = function
    Int -> "int"
  | Double -> "double"
  | Char -> "char"
  | String -> "string"
  | Bool -> "bool"
  | Void -> "void"
  | Array(t) -> string_of_typ t ^ "[]"
  | Canvas -> "Canvas"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | DoubleLit(l) -> string_of_float l
  | StrLit(l) -> "\"" ^ l ^ "\""
  | CharLit(l) -> Char.escaped l
  | BoolLit(l) -> string_of_bool l
  | Id(s) -> s
  | Asn(s, e) -> s ^ " = " ^ string_of_expr e
  | Binop(e1, o, e2) -> "\t" ^ string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_unop o ^ string_of_expr e
  | ArrayListInit(l) -> "[" ^ String.concat ", " (List.map string_of_expr l) ^ "]"
  | CanvasInit(x, y, c) -> "(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ", '" ^ Char.escaped c ^ "'}"
  | FuncCall(n, p) -> n ^ "(" ^ String.concat ", " (List.map string_of_expr p) ^ ")"
  | Noexpr -> ""

let string_of_argdecl (t, id) = string_of_typ t ^ " " ^ id

let string_of_vardecl vdecl = 
    string_of_typ vdecl.declTyp ^ " " ^
    vdecl.declID ^ 
    string_of_expr vdecl.declInit ^ ";"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"   
  | Return(expr) -> "return" ^ " " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s1 ^ "}\n else {\n" ^ string_of_stmt s2 ^ "}\n"
  | For(e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ string_of_expr e2 ^ string_of_expr e3 ^ ") {\n" ^ string_of_stmt s ^ "}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s ^ "}\n"
  | Continue -> "continue;"
  | Break -> "break;"

let string_of_funcdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^
  String.concat ", " (List.map string_of_argdecl fdecl.args) ^ ")\n{\n\t" ^
  String.concat "\n\t" (List.map string_of_vardecl fdecl.locals) ^ "\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_prog (vars, funcs) = 
    String.concat "" (List.map string_of_vardecl vars) ^ "\n" ^
    String.concat "\n" (List.map string_of_funcdecl funcs)


