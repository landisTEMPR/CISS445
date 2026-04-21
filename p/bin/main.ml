let () =
  if Array.length Sys.argv < 2 then (print_endline "Usage: main <file.py>"; exit 1);
  let filename = Sys.argv.(1) in
  let ic = open_in filename in
  let n  = in_channel_length ic in
  let src = Bytes.create n in
  really_input ic src 0 n;
  close_in ic;
  let program = Interp.Parser.parse_program (Bytes.to_string src) in
  Interp.Eval.run program
