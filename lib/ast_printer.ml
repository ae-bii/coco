open Ast

let string_of_unop (op : unop) : string =
  match op with
  | NEGATION -> "Neg"
  | BWCOMPLIMENT -> "BwCompl"
  | LGNEGATION -> "LgNeg"

let string_of_binop (op : binop) : string =
  match op with
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "=="
  | NotEqual -> "!="
  | Less -> "<"
  | LessEqual -> "<="
  | Greater -> ">"
  | GreaterEqual -> ">="
  | Modulo -> "%"
  | BitwiseAnd -> "&"
  | BitwiseOr -> "|"
  | BitwiseXor -> "^"
  | LShift -> "<<"
  | RShift -> ">>"
  | Comma -> ","

let indent_space level = String.make (level * 4) ' '

let rec string_of_prog (p : prog) : string =
  match p with
  | Prog funcs ->
      let funcs_str = List.map (string_of_fun_decl 0) funcs in
      String.concat "\n\n" funcs_str

and string_of_fun_decl level (f : fun_decl) : string =
  match f with
  | Fun (name, stmts) ->
      let i = indent_space level in
      let i_plus_1 = indent_space (level + 1) in
      let stmts_str = List.map (string_of_statement (level + 2)) stmts in
      let body_str = String.concat "\n" stmts_str in

      Printf.sprintf "%sFUN INT %s:\n%sparams: ()\n%sbody:\n%s" i name i_plus_1
        i_plus_1 body_str

and string_of_statement level (s : statement) : string =
  let i = indent_space level in
  match s with
  | Return exp ->
      let exp_str = string_of_exp (level + 1) exp in
      Printf.sprintf "%sRETURN\n%s" i exp_str
  | Declare (name, exp_opt) ->
      let init_str =
        match exp_opt with
        | Some exp -> Printf.sprintf "\n%s" (string_of_exp (level + 1) exp)
        | None -> ""
      in
      Printf.sprintf "%sDECLARE \"%s\"%s" i name init_str
  | Exp exp ->
      let exp_str = string_of_exp (level + 1) exp in
      Printf.sprintf "%sEXP\n%s" i exp_str

and string_of_exp level (e : exp) : string =
  let i = indent_space level in
  match e with
  | Const i_val -> Printf.sprintf "%sInt<%d>" i i_val
  | Var name -> Printf.sprintf "%sVar<\"%s\">" i name
  | Assign (name, exp) ->
      let exp_str = string_of_exp (level + 1) exp in
      Printf.sprintf "%sASSIGN to \"%s\"\n%s" i name exp_str
  | UnOp (op, inner_exp) ->
      let op_str = string_of_unop op in
      let exp_str = string_of_exp (level + 1) inner_exp in
      Printf.sprintf "%sUnOp<%s>(\n%s\n%s)" i op_str exp_str i
  | BinOp (left_exp, op, right_exp) ->
      let op_str = string_of_binop op in
      let left_str = string_of_exp (level + 1) left_exp in
      let right_str = string_of_exp (level + 1) right_exp in
      Printf.sprintf "%sBinOp<%s>(\n%s,\n%s\n%s)" i op_str left_str right_str i
  | CompoundAssign (name, op, exp) ->
      let op_str = string_of_binop op in
      let exp_str = string_of_exp (level + 1) exp in
      Printf.sprintf "%sCOMPOUND_ASSIGN to \"%s\" op <%s>\n%s" i name op_str
        exp_str
  | PostfixInc name -> Printf.sprintf "%sPOSTFIX_INC on \"%s\"" i name
  | PostfixDec name -> Printf.sprintf "%sPOSTFIX_DEC on \"%s\"" i name
  | PrefixInc name -> Printf.sprintf "%sPREFIX_INC on \"%s\"" i name
  | PrefixDec name -> Printf.sprintf "%sPREFIX_DEC on \"%s\"" i name
