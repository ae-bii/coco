open Ast

let label_counter = ref 0

let new_label () =
  label_counter := !label_counter + 1;
  Printf.sprintf "L%d" !label_counter

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
      let left_asm = generate_exp left_exp in
      let right_asm = generate_exp right_exp in
      match op with
      (* arithmetic operators *)
      | Add | Multiply ->
          left_asm ^ "  push rax\n" ^ right_asm ^ "  pop rcx\n"
          ^
          if op = Add then
            "  add rax, rcx\n"
          else
            "  imul rax, rcx\n"
      | Subtract | Divide ->
          right_asm ^ "  push rax\n" ^ left_asm ^ "  pop rcx\n"
          ^
          if op = Subtract then
            "  sub rax, rcx\n"
          else
            "  cqo\n  idiv rcx\n"
      (* relational operators *)
      | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual -> (
          left_asm ^ "  push rax\n" ^ right_asm ^ "  pop rcx\n"
          ^ "  cmp rcx, rax\n" (* compare e1 in rcx to e2 in rax *)
          ^ "  mov rax, 0\n" (* zero out rax *)
          ^
          match op with
          | Equal -> "  sete al\n"
          | NotEqual -> "  setne al\n"
          | Less -> "  setl al\n"
          | LessEqual -> "  setle al\n"
          | Greater -> "  setg al\n"
          | GreaterEqual -> "  setge al\n"
          | _ -> "" (* should not happen *))
      (* logical operators with short circuiting *)
      | And ->
          let clause2_label = new_label () in
          let end_label = new_label () in
          left_asm ^ "  cmp rax, 0\n"
          ^ Printf.sprintf "  jne %s\n"
              clause2_label (* if e1 is not 0 check e2 *)
          ^ Printf.sprintf "  jmp %s\n"
              end_label (* otherwise result is 0 so jump to end *)
          ^ Printf.sprintf "%s:\n" clause2_label
          ^ right_asm ^ "  cmp rax, 0\n" ^ "  mov rax, 0\n"
          ^ "  setne al\n" (* set rax to 1 if e2 is not 0 else 0 *)
          ^ Printf.sprintf "%s:\n" end_label
      | Or ->
          let clause2_label = new_label () in
          let end_label = new_label () in
          left_asm ^ "  cmp rax, 0\n"
          ^ Printf.sprintf "  je %s\n" clause2_label (* if e1 is 0 check e2 *)
          ^ "  mov rax, 1\n" (* otherwise result is 1 *)
          ^ Printf.sprintf "  jmp %s\n" end_label (* jump to end *)
          ^ Printf.sprintf "%s:\n" clause2_label
          ^ right_asm ^ "  cmp rax, 0\n" ^ "  mov rax, 0\n"
          ^ "  setne al\n" (* set rax to 1 if e2 is not 0 else 0 *)
          ^ Printf.sprintf "%s:\n" end_label)

let generate (ast : prog) : string = generate_prog ast
