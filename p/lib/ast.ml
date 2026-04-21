type value =
  | VInt   of int
  | VFloat of float
  | VBool  of bool
  | VNone

type binop =
  | Add | Sub | Mul | Div | IDiv | Mod | Pow
  | Eq | NEq | Lt | Gt | LEq | GEq
  | And | Or

type unop =
  | Neg | Not

type expr =
  | Lit    of value
  | Var    of string
  | Binop  of binop * expr * expr
  | Unop   of unop * expr

type stmt =
  | Assign  of string * expr
  | Print   of expr
  | If      of expr * block * block
  | While   of expr * block
  | Expr    of expr

and block = stmt list
