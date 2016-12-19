(* Abstract Syntax Tree *)
type binopr =
    Add
  | Sub
  | Mult
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | And
  | Or
  | Asn

type unopr = Not | Neg | Mag

(* BLUR TYPES *)
type primitive =
    Int
  | Double
  | Char
  | String
  | Bool
  | Void

type expr =
    Binop of expr * binopr * expr
  | Unop of unopr * expr
  | IntLit of int
  | DoubleLit of float
  | StrLit of string
  | CharLit of char
  | BoolLit of bool
  | Id of string
  | ArrayListInit of expr list
  | ArrayAccess of string * expr list
  | FuncCall of string * expr list
  | Noexpr

type datatype =
    SizedArray of primitive * int list
  | UnsizedArray of primitive * int
  | Datatype of primitive

type argdecl = {
  argdeclType : datatype;
  argdeclID : string;
}

type vardecl = {
  declTyp : datatype;
  declID : string;
  declInit : expr;  
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
    typ : datatype;
    fname : string;
    args : argdecl list;
    body : stmt list;
  }

type program = vardecl list * funcdecl list


