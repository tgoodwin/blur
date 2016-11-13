open Ast

type expr =
    Binop_t of expr_t * binopr * expr_t
  | Unop_t of unopr * expr_t
  | IntLit_t of int
  | DoubleLit_t of float
  | StrLit_t of string
  | CharLit_t of char
  | BoolLit_t of bool
  | Id_t of string
  | Asn_t of string * expr_t
  | ArrayListInit_t of expr_t list
  | FuncCall_t of string * expr_t list
  | Noexpr_t

  and expr_t = (expr * typ)

type stmt_s =
    Block_s of stmt_s list
  | Expr_s of expr_t
  | Return_s of expr_t
  | If_s of expr_t * stmt_s * stmt_s
  | For_s of expr_t * expr_t * expr_t * stmt_s
  | While_s of expr_t * stmt_s
  | Continue_s
  | Break_s

type argdecl_s = typ * string

type vardecl_s =
    {
        declTyp_s: typ;
        declID_s: string;
        declInit_s: expr_t
    }

type funcdecl_s =
    {
        s_typ: typ;
        s_fname: string;
        s_args: argdecl_s list;
        s_locals: vardecl_s list;
        s_body: stmt_s list;
    }

type program_s = vardecl_s list * funcdecl_s list
