{
  type token =
    | INT    of int
    | FLOAT  of float
    | BOOL   of bool
    | NONE
    | IDENT  of string
    | PLUS | MINUS | STAR | SLASH | DOUBLESLASH | PERCENT | STARSTAR
    | EQ | NEQ | LT | GT | LEQ | GEQ
    | ASSIGN
    | AND | OR | NOT
    | LPAREN | RPAREN
    | COLON | COMMA
    | PRINT
    | IF | ELSE | WHILE
    | NEWLINE | EOF

  exception LexError of string
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let int   = digit+
let flt   = digit+ '.' digit*
let ident = alpha alnum*
let blank = [' ' '\t']

rule token = parse
  | blank+        { token lexbuf }
  | '\n'          { NEWLINE }
  | flt  as f     { FLOAT (float_of_string f) }
  | int  as i     { INT   (int_of_string i) }
  | "True"        { BOOL true }
  | "False"       { BOOL false }
  | "None"        { NONE }
  | "and"         { AND }
  | "or"          { OR }
  | "not"         { NOT }
  | "if"          { IF }
  | "else"        { ELSE }
  | "while"       { WHILE }
  | "print"       { PRINT }
  | ident as s    { IDENT s }
  | "**"          { STARSTAR }
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { STAR }
  | "//"          { DOUBLESLASH }
  | "/"           { SLASH }
  | "%"           { PERCENT }
  | "=="          { EQ }
  | "!="          { NEQ }
  | "<="          { LEQ }
  | ">="          { GEQ }
  | "<"           { LT }
  | ">"           { GT }
  | "="           { ASSIGN }
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | ":"           { COLON }
  | ","           { COMMA }
  | "#" [^'\n']*  { token lexbuf }
  | eof           { EOF }
  | _ as c        { raise (LexError (Printf.sprintf "Unexpected char: %c" c)) }
