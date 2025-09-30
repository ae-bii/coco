open Ast

let rec generate_prog (p : prog) : string =
  match p with
  | Prog funcs ->
      let funcs_asm = List.map generate_fun_decl funcs |> String.concat "" in
      "section .text\n" ^ funcs_asm

and generate_fun_decl (f : fun_decl) : string =
  match f with
  | Fun (name, stmts) ->
      let prologue =
        Printf.sprintf "global _%s\n_%s:\n  push rbp\n  mov rbp, rsp\n" name
          name
      in
      let stmts_asm = List.map generate_statement stmts |> String.concat "" in
      let epilogue = "  pop rbp\n  ret\n" in
      prologue ^ stmts_asm ^ epilogue

and generate_statement (s : statement) : string =
  match s with
  | Return exp -> generate_exp exp

and generate_exp (e : exp) : string =
  match e with
  | Const i -> Printf.sprintf "  mov rax, %d\n" i
  | UnOp (op, inner_exp) ->
      let inner_exp_asm = generate_exp inner_exp in
      let op_asm =
        match op with
        | NEGATION -> "  neg rax\n"
        | BWCOMPLIMENT -> "  not rax\n"
        | LGNEGATION -> "  cmp rax, 0\n" ^ "  mov rax, 0\n" ^ "  sete al\n"
      in
      inner_exp_asm ^ op_asm
  | BinOp (left_exp, op, right_exp) -> (
      match op with
      | Add | Multiply ->
          let left_asm = generate_exp left_exp in
          let right_asm = generate_exp right_exp in
          left_asm ^ "  push rax\n" ^ right_asm ^ "  pop rcx\n"
          ^
          if op = Add then
            "  add rax, rcx\n"
          else
            "  imul rax, rcx\n"
      | Subtract | Divide ->
          let right_asm = generate_exp right_exp in
          let left_asm = generate_exp left_exp in
          right_asm ^ "  push rax\n" ^ left_asm ^ "  pop rcx\n"
          ^
          if op = Subtract then
            "  sub rax, rcx\n"
          else
            "  cqo\n  idiv rcx\n")

let generate (ast : prog) : string = generate_prog ast
