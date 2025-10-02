open Ast

let expect expected tokens =
  match tokens with
  | t :: rest when t = expected -> rest
  | _ -> failwith "Expected another token, but got a different one."

let rec parse_exp tokens =
  match tokens with
  | ID name :: ASSIGN :: rest ->
      let rhs_exp, remaining_tokens = parse_exp rest in
      (Assign (name, rhs_exp), remaining_tokens)
  | _ -> parse_logical_or_exp tokens

and parse_logical_or_exp tokens =
  (* precedence level: || *)
  let left_node, remaining_tokens = parse_logical_and_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | OR :: rest ->
        let right_node, remaining = parse_logical_and_exp rest in
        loop (BinOp (acc, Or, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_logical_and_exp tokens =
  (* precedence level: && *)
  let left_node, remaining_tokens = parse_equality_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | AND :: rest ->
        let right_node, remaining = parse_equality_exp rest in
        loop (BinOp (acc, And, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_bitwise_or_exp tokens =
  (* precedence level: | *)
  let left_node, remaining_tokens = parse_bitwise_xor_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | BW_OR :: rest ->
        let right_node, remaining = parse_bitwise_xor_exp rest in
        loop (BinOp (acc, BitwiseOr, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_bitwise_xor_exp tokens =
  (* precedence level: ^ *)
  let left_node, remaining_tokens = parse_bitwise_and_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | BW_XOR :: rest ->
        let right_node, remaining = parse_bitwise_and_exp rest in
        loop (BinOp (acc, BitwiseXor, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_bitwise_and_exp tokens =
  (* precedence level: & *)
  let left_node, remaining_tokens = parse_equality_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | BW_AND :: rest ->
        let right_node, remaining = parse_equality_exp rest in
        loop (BinOp (acc, BitwiseAnd, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_equality_exp tokens =
  (* precedence level: ==, != *)
  let left_node, remaining_tokens = parse_relational_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | EQUAL :: rest ->
        let right_node, remaining = parse_relational_exp rest in
        loop (BinOp (acc, Equal, right_node)) remaining
    | NOT_EQUAL :: rest ->
        let right_node, remaining = parse_relational_exp rest in
        loop (BinOp (acc, NotEqual, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_relational_exp tokens =
  (* precedence level: <, <=, >, >= *)
  let left_node, remaining_tokens = parse_shift_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | LESS :: rest ->
        let right_node, remaining = parse_shift_exp rest in
        loop (BinOp (acc, Less, right_node)) remaining
    | LESS_EQUAL :: rest ->
        let right_node, remaining = parse_shift_exp rest in
        loop (BinOp (acc, LessEqual, right_node)) remaining
    | GREATER :: rest ->
        let right_node, remaining = parse_shift_exp rest in
        loop (BinOp (acc, Greater, right_node)) remaining
    | GREATER_EQUAL :: rest ->
        let right_node, remaining = parse_shift_exp rest in
        loop (BinOp (acc, GreaterEqual, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_shift_exp tokens =
  (* precedence level: <<, >> *)
  let left_node, remaining_tokens = parse_additive_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | LSHIFT :: rest ->
        let right_node, remaining = parse_additive_exp rest in
        loop (BinOp (acc, LShift, right_node)) remaining
    | RSHIFT :: rest ->
        let right_node, remaining = parse_additive_exp rest in
        loop (BinOp (acc, RShift, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_additive_exp tokens =
  (* precedence level: +, - *)
  let left_node, remaining_tokens = parse_term tokens in
  let rec loop acc tokens =
    match tokens with
    | ADD :: rest ->
        let right_node, remaining = parse_term rest in
        loop (BinOp (acc, Add, right_node)) remaining
    | MINUS :: rest ->
        let right_node, remaining = parse_term rest in
        loop (BinOp (acc, Subtract, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_term tokens =
  (* precedence level: *, /, % *)
  let left_node, remaining_tokens = parse_factor tokens in
  let rec loop acc tokens =
    match tokens with
    | MULTIPLY :: rest ->
        let right_node, remaining = parse_factor rest in
        loop (BinOp (acc, Multiply, right_node)) remaining
    | DIVIDE :: rest ->
        let right_node, remaining = parse_factor rest in
        loop (BinOp (acc, Divide, right_node)) remaining
    | MODULO :: rest ->
        (* <-- Add Modulo case *)
        let right_node, remaining = parse_factor rest in
        loop (BinOp (acc, Modulo, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_factor tokens =
  (* highest precedence: numbers, parens, unary ops *)
  match tokens with
  | NUM i :: rest -> (Const i, rest)
  | ID s :: rest -> (Var s, rest)
  | LPAREN :: rest ->
      let exp_node, tokens_after_exp = parse_exp rest in
      let tokens_after_paren = expect RPAREN tokens_after_exp in
      (exp_node, tokens_after_paren)
  | MINUS :: rest ->
      let factor_node, remaining_tokens = parse_factor rest in
      (UnOp (NEGATION, factor_node), remaining_tokens)
  | BWCOMPLIMENT :: rest ->
      let factor_node, remaining_tokens = parse_factor rest in
      (UnOp (BWCOMPLIMENT, factor_node), remaining_tokens)
  | LGNEGATION :: rest ->
      let factor_node, remaining_tokens = parse_factor rest in
      (UnOp (LGNEGATION, factor_node), remaining_tokens)
  | _ -> failwith "Invalid factor"

and parse_statement tokens =
  match tokens with
  | RETURN :: rest ->
      let exp_node, tokens_after_exp = parse_exp rest in
      let tokens_after_semicolon = expect SEMICOLON tokens_after_exp in
      (Return exp_node, tokens_after_semicolon)
  | INT :: ID name :: rest -> (
      (* variable declaration *)
      match rest with
      | SEMICOLON :: rest_after_decl -> (Declare (name, None), rest_after_decl)
      | ASSIGN :: rest_after_assign ->
          let init_exp, tokens_after_exp = parse_exp rest_after_assign in
          let tokens_after_semicolon = expect SEMICOLON tokens_after_exp in
          (Declare (name, Some init_exp), tokens_after_semicolon)
      | _ -> failwith "Invalid declaration syntax")
  | _ ->
      let exp_node, tokens_after_exp = parse_exp tokens in
      let tokens_after_semicolon = expect SEMICOLON tokens_after_exp in
      (Exp exp_node, tokens_after_semicolon)

let parse_fun_decl tokens =
  let tokens = expect INT tokens in
  let name, tokens =
    match tokens with
    | ID s :: rest -> (s, rest)
    | _ -> failwith "Expected function name"
  in
  let tokens = expect LPAREN tokens in
  let tokens = expect RPAREN tokens in
  let tokens = expect RPAREN tokens in
  let tokens = expect LBRACE tokens in
  let rec parse_statements_until_brace acc tokens =
    match tokens with
    | RBRACE :: _ -> (List.rev acc, tokens)
    | _ ->
        let stmt, rest = parse_statement tokens in
        parse_statements_until_brace (stmt :: acc) rest
  in
  let statements, tokens = parse_statements_until_brace [] tokens in
  let tokens = expect RBRACE tokens in
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
