(* Abstract Syntax Tree *)
type binopr =
    Add
  | Sub
  | Mult
  | Div
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Asn

type typ =
    Int
  | Double
  | Char
  | String
  | Bool
  | Void
  | Array of typ

type bind = typ * string

type expr =
    Binop of expr * binopr * expr
  | IntLit of int
  | DoubleLit of float
  | StrLit of string
  | CharLit of char
  | BoolLit of bool
  | Id of string
  | ArrayListInit of expr list
  | ArraySizeInit of typ * int
  | FuncCall of string * expr list
  | Noexpr

type vardecl = {
    declTyp : typ;
    declID : string;
    declInit : expr
  }

type stmt = 
    Block of stmt list
  | Expr of expr
  | Decl of vardecl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type funcdecl = {
    typ : typ;
    fname : string;
    args : vardecl list;
    body : stmt list;
  }

type decl = 
    Vardecl of vardecl
  | Funcdecl of funcdecl

type program = decl list


