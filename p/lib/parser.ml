open Ast
open Lexer

type pstate = { mutable tokens : token list }

let make_parser toks = { tokens = toks }
let peek ps = match ps.tokens with [] -> EOF | t :: _ -> t
let consume ps = match ps.tokens with [] -> EOF | t :: rest -> ps.tokens <- rest; t
let expect ps tok =
  let t = consume ps in
  if t <> tok then failwith "Parse error: unexpected token"

let tokenize src =
  let lexbuf = Lexing.from_string src in
  let rec loop acc =
    let t = token lexbuf in
    if t = EOF then List.rev (EOF :: acc)
    else loop (t :: acc)
  in loop []

let indent_level line =
  let i = ref 0 in
  while !i < String.length line && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
  !i

let is_blank_or_comment line =
  let trimmed = String.trim line in
  String.length trimmed = 0 || trimmed.[0] = '#'

let rec parse_expr ps = parse_or ps

and parse_or ps =
  let l = parse_and ps in
  if peek ps = OR then (ignore (consume ps); Binop (Or, l, parse_or ps))
  else l

and parse_and ps =
  let l = parse_not ps in
  if peek ps = AND then (ignore (consume ps); Binop (And, l, parse_and ps))
  else l

and parse_not ps =
  if peek ps = NOT then (ignore (consume ps); Unop (Not, parse_not ps))
  else parse_cmp ps

and parse_cmp ps =
  let l = parse_add ps in
  match peek ps with
  | EQ  -> ignore (consume ps); Binop (Eq,  l, parse_add ps)
  | NEQ -> ignore (consume ps); Binop (NEq, l, parse_add ps)
  | LT  -> ignore (consume ps); Binop (Lt,  l, parse_add ps)
  | GT  -> ignore (consume ps); Binop (Gt,  l, parse_add ps)
  | LEQ -> ignore (consume ps); Binop (LEq, l, parse_add ps)
  | GEQ -> ignore (consume ps); Binop (GEq, l, parse_add ps)
  | _   -> l

and parse_add ps =
  let l = ref (parse_mul ps) in
  let go = ref true in
  while !go do
    match peek ps with
    | PLUS  -> ignore (consume ps); l := Binop (Add, !l, parse_mul ps)
    | MINUS -> ignore (consume ps); l := Binop (Sub, !l, parse_mul ps)
    | _     -> go := false
  done; !l

and parse_mul ps =
  let l = ref (parse_unary ps) in
  let go = ref true in
  while !go do
    match peek ps with
    | STAR        -> ignore (consume ps); l := Binop (Mul, !l, parse_unary ps)
    | SLASH       -> ignore (consume ps); l := Binop (Div, !l, parse_unary ps)
    | DOUBLESLASH -> ignore (consume ps); l := Binop (IDiv, !l, parse_unary ps)
    | PERCENT     -> ignore (consume ps); l := Binop (Mod, !l, parse_unary ps)
    | _           -> go := false
  done; !l

and parse_unary ps =
  match peek ps with
  | MINUS -> ignore (consume ps); Unop (Neg, parse_unary ps)
  | _     -> parse_pow ps

and parse_pow ps =
  let b = parse_atom ps in
  if peek ps = STARSTAR then (ignore (consume ps); Binop (Pow, b, parse_unary ps))
  else b

and parse_atom ps =
  match consume ps with
  | INT i   -> Lit (VInt i)
  | FLOAT f -> Lit (VFloat f)
  | BOOL b  -> Lit (VBool b)
  | NONE    -> Lit VNone
  | IDENT s -> Var s
  | LPAREN  -> let e = parse_expr ps in expect ps RPAREN; e
  | _       -> failwith "parse_atom: unexpected token"

let rec parse_block lines indent =
  match lines with
  | [] -> ([], [])
  | line :: rest ->
    if is_blank_or_comment line then parse_block rest indent
    else if indent_level line < indent then ([], lines)
    else if indent_level line > indent then failwith "Unexpected indent"
    else
      let (s, remaining) = parse_line lines indent in
      let (more, final)  = parse_block remaining indent in
      (s :: more, final)

and parse_line lines indent =
  match lines with
  | [] -> failwith "Expected statement"
  | line :: rest ->
    if is_blank_or_comment line then parse_line rest indent
    else
      let toks = tokenize (String.trim line) in
      let ps   = make_parser toks in
      match peek ps with
      | IF ->
        ignore (consume ps);
        let cond = parse_expr ps in
        expect ps COLON;
        if peek ps <> EOF then
          let body = parse_stmt_tokens ps in
          (If (cond, [body], []), rest)
        else begin
          let ni = indent + 4 in
          let (tbody, after) = parse_block rest ni in
          match after with
          | el :: rest2 when String.trim el = "else:" ->
            let (ebody, after2) = parse_block rest2 ni in
            (If (cond, tbody, ebody), after2)
          | _ -> (If (cond, tbody, []), after)
        end
      | WHILE ->
        ignore (consume ps);
        let cond = parse_expr ps in
        expect ps COLON;
        let ni = indent + 4 in
        let (body, after) = parse_block rest ni in
        (While (cond, body), after)
      | _ ->
        let s = parse_stmt_tokens ps in
        (s, rest)

and parse_stmt_tokens ps =
  match peek ps with
  | PRINT ->
    ignore (consume ps);
    expect ps LPAREN;
    let e = parse_expr ps in
    expect ps RPAREN;
    Print e
  | IDENT s ->
    ignore (consume ps);
    if peek ps = ASSIGN then begin
      ignore (consume ps);
      Assign (s, parse_expr ps)
    end else begin
      let ps2 = make_parser (IDENT s :: ps.tokens) in
      Expr (parse_expr ps2)
    end
  | _ -> Expr (parse_expr ps)

let parse_program src =
  let lines = String.split_on_char '\n' src in
  let (stmts, _) = parse_block lines 0 in
  stmts
