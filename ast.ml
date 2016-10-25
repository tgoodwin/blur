(* Abstract Syntax Tree *)

(* Pretty-printing functions *)
let string_of_typ = function 
    IntLiteral(l) -> "int"
  | BoolLiteral(l) -> "bool" 
  (* | Void -> "void" *)
