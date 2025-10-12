open Ast

let expect expected tokens =
  match tokens with
  | t :: rest when t = expected -> rest
  | _ -> failwith "Expected a different token"

let rec parse_exp tokens =
  let left_node, remaining_tokens = parse_assignment_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | COMMA :: rest ->
        let right_node, remaining = parse_assignment_exp rest in
        loop (BinOp (acc, Comma, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_assignment_exp tokens =
  match tokens with
  | ID name :: ASSIGN :: rest ->
      let rhs, remaining = parse_assignment_exp rest in
      (Assign (name, rhs), remaining)
  | ID name :: COMPOUND_ADD :: rest ->
      let rhs, remaining = parse_assignment_exp rest in
      (CompoundAssign (name, Add, rhs), remaining)
  | ID name :: COMPOUND_SUB :: rest ->
      let rhs, remaining = parse_assignment_exp rest in
      (CompoundAssign (name, Subtract, rhs), remaining)
  | ID name :: COMPOUND_MUL :: rest ->
      let rhs, remaining = parse_assignment_exp rest in
      (CompoundAssign (name, Multiply, rhs), remaining)
  | ID name :: COMPOUND_DIV :: rest ->
      let rhs, remaining = parse_assignment_exp rest in
      (CompoundAssign (name, Divide, rhs), remaining)
  | ID name :: COMPOUND_MOD :: rest ->
      let rhs, remaining = parse_assignment_exp rest in
      (CompoundAssign (name, Modulo, rhs), remaining)
  | _ -> parse_conditional_exp tokens

and parse_conditional_exp tokens =
  let cond_exp, after_cond = parse_logical_or_exp tokens in
  match after_cond with
  | QUESTION :: rest ->
      let then_exp, after_then = parse_exp rest in
      let after_colon = expect COLON after_then in
      let else_exp, after_else = parse_conditional_exp after_colon in
      (Conditional (cond_exp, then_exp, else_exp), after_else)
  | _ -> (cond_exp, after_cond)

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
  let left_node, remaining_tokens = parse_bitwise_or_exp tokens in
  let rec loop acc tokens =
    match tokens with
    | AND :: rest ->
        let right_node, remaining = parse_bitwise_or_exp rest in
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
        let right_node, remaining = parse_factor rest in
        loop (BinOp (acc, Modulo, right_node)) remaining
    | _ -> (acc, tokens)
  in
  loop left_node remaining_tokens

and parse_factor tokens =
  (* highest precedence: numbers, parens, unary ops *)
  match tokens with
  | MINUS :: rest ->
      let factor, remaining = parse_factor rest in
      (UnOp (NEGATION, factor), remaining)
  | BWCOMPLIMENT :: rest ->
      let factor, remaining = parse_factor rest in
      (UnOp (BWCOMPLIMENT, factor), remaining)
  | LGNEGATION :: rest ->
      let factor, remaining = parse_factor rest in
      (UnOp (LGNEGATION, factor), remaining)
  | INCREMENT :: ID name :: rest -> (PrefixInc name, rest)
  | DECREMENT :: ID name :: rest -> (PrefixDec name, rest)
  | _ -> parse_postfix_exp tokens

and parse_postfix_exp tokens =
  let primary_exp, after_primary = parse_primary_exp tokens in
  match after_primary with
  | INCREMENT :: rest ->
      let var_name =
        match primary_exp with
        | Var s -> s
        | _ -> failwith "Operand of postfix ++ must be a variable"
      in
      (PostfixInc var_name, rest)
  | DECREMENT :: rest ->
      let var_name =
        match primary_exp with
        | Var s -> s
        | _ -> failwith "Operand of postfix -- must be a variable"
      in
      (PostfixDec var_name, rest)
  | _ -> (primary_exp, after_primary)

and parse_primary_exp tokens =
  match tokens with
  | NUM i :: rest -> (Const i, rest)
  | ID s :: LPAREN :: rest ->
      (* function call: ID '(' [args] ')' *)
      let rec parse_args acc tokens =
        match tokens with
        | RPAREN :: rest_after -> (List.rev acc, rest_after)
        | _ -> (
            let arg, after_arg = parse_assignment_exp tokens in
            match after_arg with
            | COMMA :: rest_after -> parse_args (arg :: acc) rest_after
            | RPAREN :: rest_after -> (List.rev (arg :: acc), rest_after)
            | _ -> failwith "Expected , or ) in function call")
      in
      let args, remaining_after_args =
        match rest with
        | RPAREN :: r -> ([], r)
        | _ -> parse_args [] rest
      in
      (FunCall (s, args), remaining_after_args)
  | ID s :: rest -> (Var s, rest)
  | LPAREN :: rest ->
      let exp_node, tokens_after_exp = parse_exp rest in
      let tokens_after_paren = expect RPAREN tokens_after_exp in
      (exp_node, tokens_after_paren)
  | _ -> failwith "Invalid primary expression"

and parse_exp_option tokens =
  match tokens with
  | SEMICOLON :: _ -> (None, tokens)
  | RPAREN :: _ -> (None, tokens)
  | _ ->
      let exp, rest = parse_exp tokens in
      (Some exp, rest)

and parse_declaration tokens =
  match tokens with
  | INT :: ID name :: rest -> (
      match rest with
      | SEMICOLON :: rest_after_decl -> (Declare (name, None), rest_after_decl)
      | ASSIGN :: rest_after_assign ->
          let init_exp, tokens_after_exp = parse_exp rest_after_assign in
          let tokens_after_semicolon = expect SEMICOLON tokens_after_exp in
          (Declare (name, Some init_exp), tokens_after_semicolon)
      | _ -> failwith "Invalid declaration syntax")
  | _ -> failwith "Not a declaration"

and parse_statement tokens =
  match tokens with
  | RETURN :: rest ->
      let exp_node, tokens_after_exp = parse_exp rest in
      let tokens_after_semicolon = expect SEMICOLON tokens_after_exp in
      (Return exp_node, tokens_after_semicolon)
  | IF :: LPAREN :: rest -> (
      let cond_exp, after_cond = parse_exp rest in
      let after_paren = expect RPAREN after_cond in
      let then_stmt, after_then = parse_statement after_paren in
      match after_then with
      | ELSE :: after_else ->
          let else_stmt, after_else_stmt = parse_statement after_else in
          (If (cond_exp, then_stmt, Some else_stmt), after_else_stmt)
      | _ -> (If (cond_exp, then_stmt, None), after_then))
  | LBRACE :: rest ->
      let rec parse_block_items_until_brace acc tokens =
        match tokens with
        | RBRACE :: _ -> (List.rev acc, tokens)
        | _ ->
            let item, rest_after_item = parse_block_item tokens in
            parse_block_items_until_brace (item :: acc) rest_after_item
      in
      let items, after_items = parse_block_items_until_brace [] rest in
      let after_brace = expect RBRACE after_items in
      (Block items, after_brace)
  | FOR :: LPAREN :: rest ->
      let for_node, after_body =
        if List.hd rest = INT then
          let decl, after_decl = parse_declaration rest in
          let cond_exp_opt, after_cond = parse_exp_option after_decl in
          let cond_exp =
            match cond_exp_opt with
            | Some e -> e
            | None -> Const 1
          in
          let after_semi = expect SEMICOLON after_cond in
          let post_exp, after_post = parse_exp_option after_semi in
          let after_paren = expect RPAREN after_post in
          let body, after_body = parse_statement after_paren in
          (ForDecl (decl, cond_exp, post_exp, body), after_body)
        else
          let init_exp, after_init = parse_exp_option rest in
          let after_first_semi = expect SEMICOLON after_init in
          let cond_exp_opt, after_cond = parse_exp_option after_first_semi in
          let cond_exp =
            match cond_exp_opt with
            | Some e -> e
            | None -> Const 1
          in
          let after_second_semi = expect SEMICOLON after_cond in
          let post_exp, after_post = parse_exp_option after_second_semi in
          let after_paren = expect RPAREN after_post in
          let body, after_body = parse_statement after_paren in
          (For (init_exp, cond_exp, post_exp, body), after_body)
      in
      (for_node, after_body)
  | WHILE :: LPAREN :: rest ->
      let cond, after_cond = parse_exp rest in
      let after_paren = expect RPAREN after_cond in
      let body, after_body = parse_statement after_paren in
      (While (cond, body), after_body)
  | DO :: rest ->
      let body, after_body = parse_statement rest in
      let after_while = expect WHILE after_body in
      let after_lparen = expect LPAREN after_while in
      let cond, after_cond = parse_exp after_lparen in
      let after_rparen = expect RPAREN after_cond in
      let after_semi = expect SEMICOLON after_rparen in
      (Do (body, cond), after_semi)
  | BREAK :: rest -> (Break, expect SEMICOLON rest)
  | CONTINUE :: rest -> (Continue, expect SEMICOLON rest)
  | _ ->
      let exp_opt, after_exp = parse_exp_option tokens in
      let after_semi = expect SEMICOLON after_exp in
      (Exp exp_opt, after_semi)

and parse_block_item tokens =
  match tokens with
  | INT :: _ ->
      let decl, remaining = parse_declaration tokens in
      (Declaration decl, remaining)
  | _ ->
      let stmt, remaining = parse_statement tokens in
      (Statement stmt, remaining)

let parse_fun_decl tokens =
  let tokens = expect INT tokens in
  let name, tokens =
    match tokens with
    | ID s :: rest -> (s, rest)
    | _ -> failwith "Expected function name"
  in
  (* parse parameter list: '(' [ 'int' ID {',' 'int' ID} ] ')' *)
  let tokens = expect LPAREN tokens in
  let rec parse_params acc tokens =
    match tokens with
    | RPAREN :: rest -> (List.rev acc, rest)
    | INT :: ID pname :: rest -> (
        match rest with
        | COMMA :: more -> parse_params (pname :: acc) more
        | RPAREN :: more -> (List.rev (pname :: acc), more)
        | _ -> failwith "Invalid function parameter list")
    | _ -> failwith "Invalid function parameter list"
  in
  let params, tokens = parse_params [] tokens in
  (* now either a semicolon (declaration) or a function body '{' ... '}' *)
  match tokens with
  | SEMICOLON :: rest -> (Fun (name, params, None), rest)
  | LBRACE :: rest ->
      let rec parse_block_items_until_brace acc tokens =
        match tokens with
        | RBRACE :: _ -> (List.rev acc, tokens)
        | _ ->
            let item, rest = parse_block_item tokens in
            parse_block_items_until_brace (item :: acc) rest
      in
      let block_items, after_items = parse_block_items_until_brace [] rest in
      let after_brace = expect RBRACE after_items in
      (Fun (name, params, Some block_items), after_brace)
  | _ -> failwith "Expected function body or semicolon"

let parse tokens =
  let tokens_no_whitespace = List.filter (fun t -> t <> WHITESPACE) tokens in
  let rec parse_all_items acc tokens =
    match tokens with
    | [] -> List.rev acc
  | INT :: ID _ :: LPAREN :: _ ->
    let func, rest = parse_fun_decl tokens in
    parse_all_items (FunDecl func :: acc) rest
  | INT :: _ ->
    let decl, rest = parse_declaration tokens in
    parse_all_items (VarDecl decl :: acc) rest
    | _ -> failwith "Top-level items must be function or variable declarations"
  in
  let all_items = parse_all_items [] tokens_no_whitespace in
  Prog all_items
