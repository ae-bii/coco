open Ast

type context = {
  stack_index : int;
  var_map : (string, int) Hashtbl.t;
}

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
      let initial_context = { stack_index = 0; var_map = Hashtbl.create 16 } in
      let body_asm, _ =
        List.fold_left
          (fun (asm, ctx) item ->
            let new_asm, new_ctx = generate_block_item item ctx in
            (asm ^ new_asm, new_ctx))
          ("", initial_context) stmts
      in

      let last_item = List.nth_opt (List.rev stmts) 0 in
      let epilogue =
        match last_item with
        | Some (Statement (Return _)) -> ""
        | _ -> "  mov rax, 0\n  mov rsp, rbp\n  pop rbp\n  ret\n"
      in
      prologue ^ body_asm ^ epilogue

and generate_block_item (i : block_item) (ctx : context) : string * context =
  match i with
  | Statement s -> generate_statement s ctx
  | Declaration d -> generate_declaration d ctx

and generate_declaration (d : declaration) (ctx : context) : string * context =
  match d with
  | Declare (name, exp_opt) ->
      if Hashtbl.mem ctx.var_map name then
        failwith ("Variable declared twice: " ^ name);
      let init_asm, ctx_after_init =
        match exp_opt with
        | Some e -> generate_exp e ctx
        | None -> ("  mov rax, 0\n", ctx)
      in
      let new_stack_index = ctx_after_init.stack_index - 8 in
      Hashtbl.add ctx_after_init.var_map name new_stack_index;
      let new_ctx =
        { stack_index = new_stack_index; var_map = ctx_after_init.var_map }
      in
      (init_asm ^ "  push rax\n", new_ctx)

and generate_statement (s : statement) (ctx : context) : string * context =
  match s with
  | Return exp ->
      let exp_asm, updated_ctx = generate_exp exp ctx in
      let epilogue = "  mov rsp, rbp\n  pop rbp\n  ret\n" in
      (exp_asm ^ epilogue, updated_ctx)
  | Exp exp -> generate_exp exp ctx
  | If (cond, then_stmt, else_opt) -> (
      let cond_asm, ctx1 = generate_exp cond ctx in
      let then_asm, ctx2 = generate_statement then_stmt ctx1 in
      let else_label = new_label () in
      let end_label = new_label () in

      match else_opt with
      | Some else_stmt ->
          let else_asm, ctx3 = generate_statement else_stmt ctx2 in
          let asm =
            cond_asm ^ "  cmp rax, 0\n"
            ^ Printf.sprintf "  je %s\n" else_label
            ^ then_asm
            ^ Printf.sprintf "  jmp %s\n" end_label
            ^ Printf.sprintf "%s:\n" else_label
            ^ else_asm
            ^ Printf.sprintf "%s:\n" end_label
          in
          (asm, ctx3)
      | None ->
          let asm =
            cond_asm ^ "  cmp rax, 0\n"
            ^ Printf.sprintf "  je %s\n" end_label
            ^ then_asm
            ^ Printf.sprintf "%s:\n" end_label
          in
          (asm, ctx2))

and generate_exp (e : exp) (ctx : context) : string * context =
  match e with
  | Const i -> (Printf.sprintf "  mov rax, %d\n" i, ctx)
  | Var name ->
      if not (Hashtbl.mem ctx.var_map name) then
        failwith ("Undeclared variable: " ^ name);
      let offset = Hashtbl.find ctx.var_map name in
      (Printf.sprintf "  mov rax, [rbp + %d]\n" offset, ctx)
  | Assign (name, exp) ->
      if not (Hashtbl.mem ctx.var_map name) then
        failwith ("Undeclared variable: " ^ name);
      let offset = Hashtbl.find ctx.var_map name in
      let exp_asm, updated_ctx = generate_exp exp ctx in
      let assign_asm = Printf.sprintf "  mov [rbp + %d], rax\n" offset in
      (exp_asm ^ assign_asm, updated_ctx)
  | UnOp (op, inner_exp) ->
      let inner_exp_asm, updated_ctx = generate_exp inner_exp ctx in
      let op_asm =
        match op with
        | NEGATION -> "  neg rax\n"
        | BWCOMPLIMENT -> "  not rax\n"
        | LGNEGATION -> "  cmp rax, 0\n  mov rax, 0\n  sete al\n"
      in
      (inner_exp_asm ^ op_asm, updated_ctx)
  | Conditional (cond, then_exp, else_exp) ->
      let cond_asm, ctx1 = generate_exp cond ctx in
      let then_asm, ctx2 = generate_exp then_exp ctx1 in
      let else_asm, ctx3 = generate_exp else_exp ctx2 in
      let else_label = new_label () in
      let end_label = new_label () in
      let asm =
        cond_asm ^ "  cmp rax, 0\n"
        ^ Printf.sprintf "  je %s\n" else_label
        ^ then_asm
        ^ Printf.sprintf "  jmp %s\n" end_label
        ^ Printf.sprintf "%s:\n" else_label
        ^ else_asm
        ^ Printf.sprintf "%s:\n" end_label
      in
      (asm, ctx3)
  | CompoundAssign (name, op, exp) ->
      if not (Hashtbl.mem ctx.var_map name) then
        failwith ("Undeclared variable: " ^ name);
      let offset = Hashtbl.find ctx.var_map name in
      let exp_asm, updated_ctx = generate_exp exp ctx in
      let op_str =
        match op with
        | Add -> "add"
        | Subtract -> "sub"
        | Multiply -> "imul"
        | BitwiseAnd -> "and"
        | BitwiseOr -> "or"
        | BitwiseXor -> "xor"
        | _ ->
            failwith
              "Compound assignment for this operator is not yet implemented"
      in
      ( exp_asm (* Result of right-hand side is in RAX *)
        ^ Printf.sprintf "  %s [rbp + %d], rax\n" op_str offset
        ^ Printf.sprintf "  mov rax, [rbp + %d]\n" offset,
        (* Result is the new value *)
        updated_ctx )
  | PrefixInc name ->
      let offset = Hashtbl.find ctx.var_map name in
      ( Printf.sprintf "  add qword [rbp + %d], 1\n" offset
        ^ Printf.sprintf "  mov rax, [rbp + %d]\n" offset,
        ctx )
  | PostfixInc name ->
      let offset = Hashtbl.find ctx.var_map name in
      ( Printf.sprintf "  mov rax, [rbp + %d]\n" offset
        ^ Printf.sprintf "  add qword [rbp + %d], 1\n" offset,
        ctx )
  | PrefixDec name ->
      let offset = Hashtbl.find ctx.var_map name in
      ( Printf.sprintf "  sub qword [rbp + %d], 1\n" offset
        ^ Printf.sprintf "  mov rax, [rbp + %d]\n" offset,
        ctx )
  | PostfixDec name ->
      let offset = Hashtbl.find ctx.var_map name in
      ( Printf.sprintf "  mov rax, [rbp + %d]\n" offset
        ^ Printf.sprintf "  sub qword [rbp + %d], 1\n" offset,
        ctx )
  | BinOp (left_exp, op, right_exp) -> (
      match op with
      | Comma ->
          let left_asm, ctx1 = generate_exp left_exp ctx in
          let right_asm, ctx2 = generate_exp right_exp ctx1 in
          (left_asm ^ right_asm, ctx2)
      (* short circuiting operators *)
      | And ->
          let left_asm, ctx1 = generate_exp left_exp ctx in
          let right_asm, ctx2 = generate_exp right_exp ctx1 in
          let clause2_label = new_label () and end_label = new_label () in
          let asm =
            left_asm ^ "  cmp rax, 0\n"
            ^ Printf.sprintf "  jne %s\n" clause2_label
            ^ Printf.sprintf "  jmp %s\n" end_label
            ^ Printf.sprintf "%s:\n" clause2_label
            ^ right_asm ^ "  cmp rax, 0\n  mov rax, 0\n  setne al\n"
            ^ Printf.sprintf "%s:\n" end_label
          in
          (asm, ctx2)
      | Or ->
          let left_asm, ctx1 = generate_exp left_exp ctx in
          let right_asm, ctx2 = generate_exp right_exp ctx1 in
          let clause2_label = new_label () and end_label = new_label () in
          let asm =
            left_asm ^ "  cmp rax, 0\n"
            ^ Printf.sprintf "  je %s\n" clause2_label
            ^ "  mov rax, 1\n"
            ^ Printf.sprintf "  jmp %s\n" end_label
            ^ Printf.sprintf "%s:\n" clause2_label
            ^ right_asm ^ "  cmp rax, 0\n  mov rax, 0\n  setne al\n"
            ^ Printf.sprintf "%s:\n" end_label
          in
          (asm, ctx2)
      (* relational operators *)
      | Equal | NotEqual | Less | LessEqual | Greater | GreaterEqual ->
          let left_asm, ctx1 = generate_exp left_exp ctx in
          let right_asm, ctx2 = generate_exp right_exp ctx1 in
          let op_asm =
            "  pop rcx\n" ^ "  cmp rcx, rax\n" ^ "  mov rax, 0\n"
            ^
            match op with
            | Equal -> "  sete al\n"
            | NotEqual -> "  setne al\n"
            | Less -> "  setl al\n"
            | LessEqual -> "  setle al\n"
            | Greater -> "  setg al\n"
            | GreaterEqual -> "  setge al\n"
            | _ -> failwith "unreachable"
          in
          (left_asm ^ "  push rax\n" ^ right_asm ^ op_asm, ctx2)
      (* arithmetic, bitwise, and shift operators *)
      | Add | Multiply | BitwiseAnd | BitwiseOr | BitwiseXor ->
          (* commutative *)
          let left_asm, ctx1 = generate_exp left_exp ctx in
          let right_asm, ctx2 = generate_exp right_exp ctx1 in
          let op_asm =
            "  pop rcx\n"
            ^
            match op with
            | Add -> "  add rax, rcx\n"
            | Multiply -> "  imul rax, rcx\n"
            | BitwiseAnd -> "  and rax, rcx\n"
            | BitwiseOr -> "  or rax, rcx\n"
            | BitwiseXor -> "  xor rax, rcx\n"
            | _ -> failwith "unreachable"
          in
          (left_asm ^ "  push rax\n" ^ right_asm ^ op_asm, ctx2)
      | Subtract | Divide | Modulo | LShift | RShift ->
          (* non commutative *)
          let right_asm, ctx1 = generate_exp right_exp ctx in
          let left_asm, ctx2 = generate_exp left_exp ctx1 in
          let op_asm =
            "  pop rcx\n"
            ^
            match op with
            | Subtract -> "  sub rax, rcx\n"
            | Divide -> "  cqo\n  idiv rcx\n"
            | Modulo -> "  cqo\n  idiv rcx\n  mov rax, rdx\n"
            | LShift -> "  shl rax, cl\n"
            | RShift -> "  sar rax, cl\n"
            | _ -> failwith "unreachable"
          in
          (right_asm ^ "  push rax\n" ^ left_asm ^ op_asm, ctx2))

let generate (ast : prog) : string = generate_prog ast
