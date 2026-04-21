(* File : parse_tree_type.ml *)

type expr =
  | Term_Expr of term
  | Plus_Expr of (term * expr)
  | Minus_Expr of (term * expr)

and term =
  | Factor_Term of factor
  | Mult_Term of (factor * term)
  | Div_Term of (factor * term)

and factor = Id_Factor of string | Paren_Expr_Factor of expr
