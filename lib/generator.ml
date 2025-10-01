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
      match op with
      (* commutative operators: evaluate left, then right *)
      | Add | Multiply | BitwiseAnd | BitwiseOr | BitwiseXor -> (
          let left_asm = generate_exp left_exp in
          let right_asm = generate_exp right_exp in
          left_asm ^ "  push rax\n" ^ right_asm ^ "  pop rcx\n"
          ^
          match op with
          | Add -> "  add rax, rcx\n"
          | Multiply -> "  imul rax, rcx\n"
          | BitwiseAnd -> "  and rax, rcx\n"
          | BitwiseOr -> "  or rax, rcx\n"
          | BitwiseXor -> "  xor rax, rcx\n"
          | _ -> "" (* should not be reached *))
      (* non commutative operators: evaluate right, then left *)
      | Subtract | Divide | Modulo | LShift | RShift -> (
          let right_asm = generate_exp right_exp in
          let left_asm = generate_exp left_exp in
          right_asm ^ "  push rax\n" ^ left_asm ^ "  pop rcx\n"
          ^
          match op with
          | Subtract -> "  sub rax, rcx\n"
          | Divide -> "  cqo\n  idiv rcx\n"
          | Modulo -> "  cqo\n  idiv rcx\n  mov rax, rdx\n"
          | LShift -> "  shl rax, cl\n"
          | RShift -> "  sar rax, cl\n"
          | _ -> "" (* should not be reached *))
      (* relational operators *)
      | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual -> (
          let left_asm = generate_exp left_exp in
          let right_asm = generate_exp right_exp in
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
          | _ -> "" (* should not be reached *))
      (* logical operators with short circuiting *)
      | And ->
          let left_asm = generate_exp left_exp in
          let right_asm = generate_exp right_exp in
          let clause2_label = new_label () in
          let end_label = new_label () in
          left_asm ^ "  cmp rax, 0\n"
          ^ Printf.sprintf "  jne %s\n" clause2_label
          ^ Printf.sprintf "  jmp %s\n" end_label
          ^ Printf.sprintf "%s:\n" clause2_label
          ^ right_asm ^ "  cmp rax, 0\n" ^ "  mov rax, 0\n" ^ "  setne al\n"
          ^ Printf.sprintf "%s:\n" end_label
      | Or ->
          let left_asm = generate_exp left_exp in
          let right_asm = generate_exp right_exp in
          let clause2_label = new_label () in
          let end_label = new_label () in
          left_asm ^ "  cmp rax, 0\n"
          ^ Printf.sprintf "  je %s\n" clause2_label
          ^ "  mov rax, 1\n"
          ^ Printf.sprintf "  jmp %s\n" end_label
          ^ Printf.sprintf "%s:\n" clause2_label
          ^ right_asm ^ "  cmp rax, 0\n" ^ "  mov rax, 0\n" ^ "  setne al\n"
          ^ Printf.sprintf "%s:\n" end_label)

let generate (ast : prog) : string = generate_prog ast
