open Ast
open Env

let to_float = function
  | VFloat f -> f
  | VInt i   -> float_of_int i
  | VBool b  -> if b then 1.0 else 0.0
  | VNone    -> failwith "Cannot convert None to float"

let to_int = function
  | VInt i  -> i
  | VBool b -> if b then 1 else 0
  | _       -> failwith "Expected int"

let is_float = function VFloat _ -> true | _ -> false

let truthy = function
  | VBool b  -> b
  | VInt i   -> i <> 0
  | VFloat f -> f <> 0.0
  | VNone    -> false

let eval_binop op l r =
  match op with
  | Add  -> if is_float l || is_float r then VFloat (to_float l +. to_float r) else VInt (to_int l + to_int r)
  | Sub  -> if is_float l || is_float r then VFloat (to_float l -. to_float r) else VInt (to_int l - to_int r)
  | Mul  -> if is_float l || is_float r then VFloat (to_float l *. to_float r) else VInt (to_int l * to_int r)
  | Div  -> VFloat (to_float l /. to_float r)
  | IDiv -> VInt (int_of_float (floor (to_float l /. to_float r)))
  | Mod  -> VInt (to_int l mod to_int r)
  | Pow  -> VFloat (to_float l ** to_float r)
  | Eq   -> VBool (l = r)
  | NEq  -> VBool (l <> r)
  | Lt   -> VBool (to_float l <  to_float r)
  | Gt   -> VBool (to_float l >  to_float r)
  | LEq  -> VBool (to_float l <= to_float r)
  | GEq  -> VBool (to_float l >= to_float r)
  | And  -> VBool (truthy l && truthy r)
  | Or   -> VBool (truthy l || truthy r)

let eval_unop op v =
  match op with
  | Neg -> (match v with VInt i -> VInt (-i) | VFloat f -> VFloat (-.f) | _ -> failwith "Cannot negate")
  | Not -> VBool (not (truthy v))

let rec eval_expr env expr =
  match expr with
  | Lit v          -> v
  | Var id         -> lookup env id
  | Binop (op,l,r) -> eval_binop op (eval_expr env l) (eval_expr env r)
  | Unop  (op,e)   -> eval_unop op (eval_expr env e)

let value_to_string = function
  | VInt i   -> string_of_int i
  | VFloat f -> if Float.is_integer f then string_of_int (int_of_float f) ^ ".0" else string_of_float f
  | VBool b  -> if b then "True" else "False"
  | VNone    -> "None"

let rec exec_stmt env stmt =
  match stmt with
  | Assign (id, e)     -> update env id (eval_expr env e)
  | Print e            -> print_endline (value_to_string (eval_expr env e)); env
  | If (cond, tb, eb)  -> if truthy (eval_expr env cond) then exec_block env tb else exec_block env eb
  | While (cond, body) ->
    let r = ref env in
    while truthy (eval_expr !r cond) do r := exec_block !r body done;
    !r
  | Expr e             -> ignore (eval_expr env e); env

and exec_block env stmts = List.fold_left exec_stmt env stmts

let run program = ignore (exec_block empty program)
