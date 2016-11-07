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

type unopr = Not | Neg

(* BLUR TYPES *)
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
  | Unop of unopr * expr
  | IntLit of int
  | DoubleLit of float
  | StrLit of string
  | CharLit of char
  | BoolLit of bool
  | Id of string
  | Asn of string * expr
  (* | Seq of expr * expr *)
  | ArrayListInit of expr list
  | ArraySizeInit of typ * int
  | FuncCall of string * expr list
  | Noexpr

type vardecl = typ * string

type stmt = 
    Block of stmt list
  | Expr of expr
  | Vardecl of vardecl
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Continue
  | Break

type funcdecl = {
    typ : typ;
    fname : string;
    args : vardecl list;
    locals: vardecl list;
    body : stmt list;
  }

type program = vardecl list * funcdecl list


