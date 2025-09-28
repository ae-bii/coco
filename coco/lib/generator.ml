open Ast

let rec generate_prog (p : prog) : string =
  match p with
  | Prog funcs ->
      let funcs_asm = List.map generate_fun_decl funcs |> String.concat "" in
      "section .text\n" ^ funcs_asm

and generate_fun_decl (f : fun_decl) : string =
  match f with
  | Fun (name, stmts) ->
      let stmts_asm = List.map generate_statement stmts |> String.concat "" in
      (* No prologue/epilogue needed for such simple functions *)
      Printf.sprintf "global _%s\n_%s:\n%s" name name stmts_asm

and generate_statement (s : statement) : string =
  match s with
  | Return exp ->
      (* Generate the code for the expression, then a simple return instruction *)
      let exp_asm = generate_exp exp in
      exp_asm ^ "  ret\n"

and generate_exp (e : exp) : string =
  match e with
  | Const i -> Printf.sprintf "  mov rax, %d\n" i

let generate (ast : prog) : string = generate_prog ast
