#use "token_type.ml"

#use "parse_tree_type.ml"

#use "parser.ml"

let _ = expr [ Id_tok "a"; Plus_tok; Id_tok "b"; Lparen_tok ]
