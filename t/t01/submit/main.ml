(* Author : Brysen Landis *)
(* File : main.ml *)

(* ---------- Printing ---------- *)

let print_board board n =
  let rec print_row r =
    if r >= n then ()
    else
      let rec print_col c =
        if c >= n then ()
        else begin
          if c > 0 then print_char '|';
          print_char board.(r).(c);
          print_col (c + 1)
        end
      in
      print_col 0;
      print_newline ();
      if r < n - 1 then begin
        let rec print_sep c =
          if c >= n then ()
          else begin
            if c > 0 then print_char '+';
            print_char '-';
            print_sep (c + 1)
          end
        in
        print_sep 0;
        print_newline ()
      end;
      print_row (r + 1)
  in
  print_row 0

(* ---------- Win Checking ---------- *)

let check_win board n player =
  let rec check_row r =
    if r >= n then false
    else
      let rec row_ok c =
        if c >= n then true
        else if board.(r).(c) <> player then false
        else row_ok (c + 1)
      in
      if row_ok 0 then true
      else check_row (r + 1)
  in

  let rec check_col c =
    if c >= n then false
    else
      let rec col_ok r =
        if r >= n then true
        else if board.(r).(c) <> player then false
        else col_ok (r + 1)
      in
      if col_ok 0 then true
      else check_col (c + 1)
  in

  let rec diag1 i =
    if i >= n then true
    else if board.(i).(i) <> player then false
    else diag1 (i + 1)
  in

  let rec diag2 i =
    if i >= n then true
    else if board.(i).(n - 1 - i) <> player then false
    else diag2 (i + 1)
  in

  check_row 0 || check_col 0 || diag1 0 || diag2 0

(* ---------- Full Board ---------- *)

let is_full board n =
  let rec check r c =
    if r >= n then true
    else if c >= n then check (r + 1) 0
    else if board.(r).(c) = ' ' then false
    else check r (c + 1)
  in
  check 0 0

(* ---------- Find Winning Move (1-step lookahead) ---------- *)

let find_winning_move board n player =
  let rec search r c =
    if r >= n then (-1, -1)
    else if c >= n then search (r + 1) 0
    else if board.(r).(c) = ' ' then begin
      board.(r).(c) <- player;
      let win = check_win board n player in
      board.(r).(c) <- ' ';
      if win then (r, c)
      else search r (c + 1)
    end
    else search r (c + 1)
  in
  search 0 0

(* ---------- AI Move ---------- *)

let ai_move board n =
  let (wr, wc) = find_winning_move board n 'O' in
  if wr >= 0 then
    board.(wr).(wc) <- 'O'
  else
    let (br, bc) = find_winning_move board n 'X' in
    if br >= 0 then
      board.(br).(bc) <- 'O'
    else
      let rec place r c =
        if r >= n then ()
        else if c >= n then place (r + 1) 0
        else if board.(r).(c) = ' ' then
          board.(r).(c) <- 'O'
        else place r (c + 1)
      in
      place 0 0

(* ---------- Main ---------- *)

let main () =
  let n = int_of_string (String.trim (input_line stdin)) in
  let board = Array.make_matrix n n ' ' in

  (* read holes *)
  let rec read_holes () =
    let r = int_of_string (String.trim (input_line stdin)) in
    if r = -1 then ()
    else
      let c = int_of_string (String.trim (input_line stdin)) in
      if r >= 0 && r < n && c >= 0 && c < n then
        board.(r).(c) <- '@';
      read_holes ()
  in
  read_holes ();

  print_board board n;

  let rec game_loop () =
    (* human move *)
    let rec get_move () =
      print_string "row: ";
      flush stdout;
      try
        let r = int_of_string (String.trim (input_line stdin)) in
        print_string "col: ";
        flush stdout;
        let c = int_of_string (String.trim (input_line stdin)) in
        if r >= 0 && r < n && c >= 0 && c < n &&
           board.(r).(c) = ' '
        then (r, c)
        else get_move ()
      with _ -> get_move ()
    in

    let (r, c) = get_move () in
    board.(r).(c) <- 'X';

    print_board board n;

    if check_win board n 'X' then
      print_endline "you win"
    else if is_full board n then
      print_endline "it's a draw"
    else begin
      ai_move board n;
      print_board board n;

      if check_win board n 'O' then
        print_endline "OCAML wins"
      else if is_full board n then
        print_endline "it's a draw"
      else
        game_loop ()
    end
  in

  game_loop ()

let () = main ()
