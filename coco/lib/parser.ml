open Ast

let expect expected tokens =
  match tokens with
  | t :: rest when t = expected -> rest
  | _ -> failwith "Expected another token, but got a different one."

let parse_exp tokens =
  match tokens with
  | Ast.NUM i :: rest -> (Const i, rest)
  | _ -> failwith "Expected an integer expression."

let parse_statement tokens =
  let tokens_after_return = expect Ast.RETURN tokens in
  let expr_node, tokens_after_expr = parse_exp tokens_after_return in
  let tokens_after_semicolon = expect Ast.SEMICOLON tokens_after_expr in

  (Return expr_node, tokens_after_semicolon)

let parse_fun_decl tokens =
  let tokens = expect Ast.INT tokens in
  let name, tokens =
    match tokens with
    | Ast.ID s :: rest -> (s, rest)
    | _ -> failwith "Expected function name"
  in
  let tokens = expect Ast.LPAREN tokens in
  let tokens = expect Ast.RPAREN tokens in
  let tokens = expect Ast.LBRACE tokens in

  let rec parse_statements_until_brace acc tokens =
    match tokens with
    | Ast.RBRACE :: _ -> (List.rev acc, tokens)
    | _ ->
        let stmt, rest = parse_statement tokens in
        parse_statements_until_brace (stmt :: acc) rest
  in

  let statements, tokens = parse_statements_until_brace [] tokens in

  let tokens = expect Ast.RBRACE tokens in

  (Fun (name, statements), tokens)

let parse tokens =
  let tokens_no_whitespace = List.filter (fun t -> t <> WHITESPACE) tokens in

  let rec parse_all_functions acc tokens =
    match tokens with
    | [] -> List.rev acc
    | _ ->
        let func, rest = parse_fun_decl tokens in
        parse_all_functions (func :: acc) rest
  in
  let all_funcs = parse_all_functions [] tokens_no_whitespace in
  Prog all_funcs
