(* Abstract Syntax Tree *)
type operator = Add | Sub | Mult | Div 

type typ = Int | Bool | Void

type bind = typ * string

type expr =
    Binop of expr * operator * expr
  | Lit of int
  | Id of string
  | Asn of string * expr
  | Seq of expr * expr
  | ListInit of expr list

type stmt = 
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * stmt * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list


