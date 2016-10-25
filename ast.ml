(* Abstract Syntax Tree *)
type operator = Add | Sub | Mul | Div | Eqlog | Neqlog | Lt | Leq | Gt | Geq

type expr =
    Binop of expr * operator * expr
    | Lit of int
    | Var of int
    | Asn of int * expr
    | Seq of expr * expr

(* Pretty-printing functions *)
let string_of_typ = function 
    IntLiteral(l) -> "int"
  | BoolLiteral(l) -> "bool" 
  (* | Void -> "void" *)
