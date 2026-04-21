(* File : token_type.ml *)
(* Author : Brysen Landis *)

type token =
  | Id_tok of string
  | Lparen_tok
  | Rparen_tok
  | Mult_tok
  | Div_tok
  | Plus_tok
  | Minus_tok
