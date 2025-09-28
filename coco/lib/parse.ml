type exp = Const of int
type statement = Return of exp
type fun_decl = Fun of string * statement list
type prog = Prog of fun_decl list

let expect expected tokens =
  match tokens with
  | t :: rest when t = expected -> rest
  | _ -> failwith "Expected another token, but got a different one."

let parse_exp tokens =
  match tokens with
  | Lex.NUM i :: rest -> (Const i, rest)
  | _ -> failwith "Expected an integer expression."

let parse_statement tokens =
  let tokens_after_return = expect Lex.RETURN tokens in
  let expr_node, tokens_after_expr = parse_exp tokens_after_return in
  let tokens_after_semicolon = expect Lex.SEMICOLON tokens_after_expr in

  (Return expr_node, tokens_after_semicolon)
