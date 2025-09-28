open Ast

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

      (* recursively call string_of_statement for the body, increasing indentation *)
      let stmts_str = List.map (string_of_statement (level + 2)) stmts in
      let body_str = String.concat "\n" stmts_str in

      Printf.sprintf "%sFUN INT %s:\n%sparams: ()\n%sbody:\n%s" i name i_plus_1
        i_plus_1 body_str

and string_of_statement level (s : statement) : string =
  match s with
  | Return exp ->
      let i = indent_space level in
      let exp_str = string_of_exp exp in
      Printf.sprintf "%sRETURN %s" i exp_str

and string_of_exp (e : exp) : string =
  match e with
  | Const i -> Printf.sprintf "Int<%d>" i
