open Ast

type env = (string * value) list

let empty : env = []

let rec lookup env id =
  match env with
  | [] -> failwith ("Unbound variable: " ^ id)
  | (k, v) :: rest -> if k = id then v else lookup rest id

let update env id v = (id, v) :: env
