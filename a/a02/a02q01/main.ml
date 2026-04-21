(*
  Author: Brysen Landis
  File : main.ml
*)

exception IgnoreCase
exception NotImplemented

(* Q1 *)
let max x y z =
  if x >= y && x >= z then x else if y >= x && y >= z then y else z

(* Q2 *)
let add_func f g = fun x -> f x +. g x

(* Q3 *)
let sub_func f g = fun x -> f x -. g x

(* Q4 *)
let mult_func f g = fun x -> f x *. g x

(* Q5 *)
let div_func f g x = f x /. g x

(* Q6 *)
let comp_func f g = fun x -> f (g x)

(* Q7 *)
let max_func f g x =
  let fv = f x in
  let gv = g x in
  if fv >= gv then fv else gv

(* Q8 *)
let d f h x = (f (x +. h) -. f x) /. h
