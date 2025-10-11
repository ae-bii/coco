open Ast

type context = {
  stack_index : int;
  var_map : (string, int) Hashtbl.t;
  break_label : string option;
  continue_label : string option;
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
  | Fun (name, block_items) ->
      let prologue =
        Printf.sprintf "global _%s\n_%s:\n  push rbp\n  mov rbp, rsp\n" name
          name
      in
      let initial_context =
        { stack_index = 0;
          var_map = Hashtbl.create 16;
          break_label = None;
          continue_label = None }
      in
      let body_asm, _ = generate_block block_items initial_context in
      let last_item = List.nth_opt (List.rev block_items) 0 in
      let epilogue =
        match last_item with
        | Some (Statement (Return _)) -> ""
        | _ -> "  mov rax, 0\n  mov rsp, rbp\n  pop rbp\n  ret\n"
      in
      prologue ^ body_asm ^ epilogue

and generate_block (items : block_item list) (ctx : context) : string * context
    =
  let block_ctx =
    { stack_index = ctx.stack_index;
      var_map = Hashtbl.copy ctx.var_map;
      break_label = ctx.break_label;
      continue_label = ctx.continue_label }
  in
  let block_asm, _, vars_declared_in_block =
    List.fold_left
      (fun (asm, current_ctx, declared_vars) item ->
        match item with
        | Statement s ->
            let new_asm, _ = generate_statement s current_ctx in
            (asm ^ new_asm, current_ctx, declared_vars)
        | Declaration d ->
            let var_name, new_asm, new_ctx =
              generate_declaration d current_ctx declared_vars
            in
            (asm ^ new_asm, new_ctx, var_name :: declared_vars))
      ("", block_ctx, []) items
  in
  let dealloc_bytes = List.length vars_declared_in_block * 8 in
  let dealloc_asm =
    if dealloc_bytes > 0 then
      Printf.sprintf "  add rsp, %d\n" dealloc_bytes
    else
      ""
  in
  (block_asm ^ dealloc_asm, ctx)

and generate_block_item (i : block_item) (ctx : context) : string * context =
  match i with
  | Statement s -> generate_statement s ctx
  | Declaration d ->
      let _name, asm, new_ctx = generate_declaration d ctx [] in
      (asm, new_ctx)

and generate_declaration (d : declaration) (ctx : context)
    (scope_vars : string list) : string * string * context =
  match d with
  | Declare (name, exp_opt) ->
      if List.mem name scope_vars then
        failwith ("Variable declared twice in the same scope: " ^ name);

      let init_asm, ctx_after_init =
        match exp_opt with
        | Some e -> generate_exp e ctx
        | None -> ("  mov rax, 0\n", ctx)
      in

      let new_stack_index = ctx_after_init.stack_index - 8 in
      Hashtbl.add ctx_after_init.var_map name new_stack_index;
      let new_ctx =
        { stack_index = new_stack_index;
          var_map = ctx_after_init.var_map;
          break_label = ctx_after_init.break_label;
          continue_label = ctx_after_init.continue_label }
      in
      (name, init_asm ^ "  push rax\n", new_ctx)

and generate_statement (s : statement) (ctx : context) : string * context =
  match s with
  | Return exp ->
      let exp_asm, updated_ctx = generate_exp exp ctx in
      let epilogue = "  mov rsp, rbp\n  pop rbp\n  ret\n" in
      (exp_asm ^ epilogue, updated_ctx)
  | Exp exp_opt -> (
      match exp_opt with
      | Some e -> generate_exp e ctx
      | None -> ("", ctx))
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
  | Block block_items -> generate_block block_items ctx
  | While (cond, body_stmt) ->
      let start_label = new_label () in
      let end_label = new_label () in
      let cond_asm, ctx1 = generate_exp cond ctx in
      (* body runs with break -> end_label and continue -> start_label *)
      let body_ctx =
        { stack_index = ctx1.stack_index;
          var_map = Hashtbl.copy ctx1.var_map;
          break_label = Some end_label;
          continue_label = Some start_label }
      in
      let body_asm, _ = generate_statement body_stmt body_ctx in
      let asm =
        Printf.sprintf "%s:\n" start_label
        ^ cond_asm ^ "  cmp rax, 0\n"
        ^ Printf.sprintf "  je %s\n" end_label
        ^ body_asm
        ^ Printf.sprintf "  jmp %s\n" start_label
        ^ Printf.sprintf "%s:\n" end_label
      in
      (asm, ctx)
  | Do (body_stmt, cond) ->
      let start_label = new_label () in
      let end_label = new_label () in
      let body_ctx =
        { stack_index = ctx.stack_index;
          var_map = Hashtbl.copy ctx.var_map;
          break_label = Some end_label;
          continue_label = Some end_label }
      in
      let body_asm, _ = generate_statement body_stmt body_ctx in
      let cond_asm, _ = generate_exp cond ctx in
      let asm =
        Printf.sprintf "%s:\n" start_label
        ^ body_asm
        ^ cond_asm ^ "  cmp rax, 0\n"
        ^ Printf.sprintf "  jne %s\n" start_label
        ^ Printf.sprintf "%s:\n" end_label
      in
      (asm, ctx)
  | For (init_opt, cond, post_opt, body_stmt) ->
      (* The for-loop header and body form a block with its own scope. *)
      let header_ctx =
        { stack_index = ctx.stack_index; var_map = Hashtbl.copy ctx.var_map;
          break_label = ctx.break_label; continue_label = ctx.continue_label }
      in
      (* init *)
      let init_asm, header_ctx =
        match init_opt with
        | Some e -> generate_exp e header_ctx
        | None -> ("", header_ctx)
      in
      let start_label = new_label () in
      let post_label = new_label () in
      let end_label = new_label () in
      (* condition *)
      let cond_asm, _ = generate_exp cond header_ctx in
      (* body runs with break -> end_label and continue -> post_label *)
      let body_ctx =
        { stack_index = header_ctx.stack_index;
          var_map = Hashtbl.copy header_ctx.var_map;
          break_label = Some end_label;
          continue_label = Some post_label }
      in
      let body_asm, _ = generate_statement body_stmt body_ctx in
      (* post expression *)
      let post_asm, _ =
        match post_opt with
        | Some e -> generate_exp e header_ctx
        | None -> ("", header_ctx)
      in
      let dealloc_bytes = ctx.stack_index - header_ctx.stack_index in
      let dealloc_asm = if dealloc_bytes > 0 then Printf.sprintf "  add rsp, %d\n" dealloc_bytes else "" in
      let asm =
        init_asm
        ^ Printf.sprintf "%s:\n" start_label
        ^ cond_asm ^ "  cmp rax, 0\n"
        ^ Printf.sprintf "  je %s\n" end_label
        ^ body_asm
        ^ Printf.sprintf "%s:\n" post_label
        ^ post_asm
        ^ Printf.sprintf "  jmp %s\n" start_label
        ^ Printf.sprintf "%s:\n" end_label
        ^ dealloc_asm
      in
      (asm, ctx)
  | ForDecl (decl, cond, post_opt, body_stmt) ->
      (* Similar to For but first item is a declaration in the for header. *)
      let header_ctx =
        { stack_index = ctx.stack_index; var_map = Hashtbl.copy ctx.var_map;
          break_label = ctx.break_label; continue_label = ctx.continue_label }
      in
      let _name, init_asm, header_ctx = generate_declaration decl header_ctx [] in
      let start_label = new_label () in
      let post_label = new_label () in
      let end_label = new_label () in
      let cond_asm, _ = generate_exp cond header_ctx in
      let body_ctx =
        { stack_index = header_ctx.stack_index;
          var_map = Hashtbl.copy header_ctx.var_map;
          break_label = Some end_label;
          continue_label = Some post_label }
      in
      let body_asm, _ = generate_statement body_stmt body_ctx in
      let post_asm, _ =
        match post_opt with
        | Some e -> generate_exp e header_ctx
        | None -> ("", header_ctx)
      in
      let dealloc_bytes = ctx.stack_index - header_ctx.stack_index in
      let dealloc_asm = if dealloc_bytes > 0 then Printf.sprintf "  add rsp, %d\n" dealloc_bytes else "" in
      let asm =
        init_asm
        ^ Printf.sprintf "%s:\n" start_label
        ^ cond_asm ^ "  cmp rax, 0\n"
        ^ Printf.sprintf "  je %s\n" end_label
        ^ body_asm
        ^ Printf.sprintf "%s:\n" post_label
        ^ post_asm
        ^ Printf.sprintf "  jmp %s\n" start_label
        ^ Printf.sprintf "%s:\n" end_label
        ^ dealloc_asm
      in
      (asm, ctx)
  | Break -> (
      match ctx.break_label with
      | Some lbl -> (Printf.sprintf "  jmp %s\n" lbl, ctx)
      | None -> failwith "'break' used outside of a loop or switch")
  | Continue -> (
      match ctx.continue_label with
      | Some lbl -> (Printf.sprintf "  jmp %s\n" lbl, ctx)
      | None -> failwith "'continue' used outside of a loop")

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
