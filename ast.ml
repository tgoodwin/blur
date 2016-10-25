(* Abstract Syntax Tree *)
type operator = Add | Sub | Mul | Div 

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Id of string
  | Asn of string * expr
  | Seq of expr * expr

(* Pretty-printing functions *)
let string_of_typ = function 
    IntLiteral(l) -> "int"
  | BoolLiteral(l) -> "bool" 
  (* | Void -> "void" *)
