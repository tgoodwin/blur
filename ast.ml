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
type primitive =
    Int
  | Double
  | Char
  | String
  | Bool
  | Void
  | Canvas

type datatype = 
    Arraytype of primitive
  | Datatype of primitive

type expr =
    Binop of expr * binopr * expr
  | Unop of unopr * expr
  | IntLit of int
  | DoubleLit of float
  | StrLit of string
  | CharLit of char
  | BoolLit of bool
  | Id of string
  (*| Asn of string * expr *)
  (* | Seq of expr * expr *)
  | ArrayListInit of expr list
  | ArraySizeInit of primitive * int
  | ArrayAccess of string * int
  | CanvasInit of int * int * char
  | FuncCall of string * expr list
  | Noexpr

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
  | Continue
  | Break

type funcdecl = {
    typ : datatype;
    fname : string;
    args : argdecl list;
    body : stmt list;
  }

type program = vardecl list * funcdecl list


