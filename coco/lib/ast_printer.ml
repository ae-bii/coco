open Ast

let rec string_of_prog (p : prog) : string =
  match p with
  | Prog funcs ->
      let funcs_str = List.map string_of_fun_decl funcs |> String.concat "\n" in
      Printf.sprintf "Prog([\n%s\n])" funcs_str

and string_of_fun_decl (f : fun_decl) : string =
  match f with
  | Fun (name, stmts) ->
      let stmts_str =
        List.map string_of_statement stmts |> String.concat "\n"
      in
      Printf.sprintf "  Fun(\"%s\", [\n%s\n  ])" name stmts_str

and string_of_statement (s : statement) : string =
  match s with
  | Return exp ->
      let exp_str = string_of_exp exp in
      Printf.sprintf "    Return(%s)" exp_str

and string_of_exp (e : exp) : string =
  match e with
  | Const i -> Printf.sprintf "Const(%d)" i
