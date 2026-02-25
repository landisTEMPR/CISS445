(* for loop that takes elements of an array and sums them up*)
let rec arr_loop f xs i n acc =
  if i = n then
    acc
  else
    arr_loop f xs (i + 1) n (f xs.(i) acc)
;;
let s = arr_loop (+) [| 1; 2; 3; |] 0 3 0;;
