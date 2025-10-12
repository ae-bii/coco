open Ast

(* validation pass: enforce function declaration/definition rules and call arg counts *)
let validate_prog (p : prog) : unit =
  match p with
  | Prog items ->
      let func_table = Hashtbl.create 32 in
      let var_table = Hashtbl.create 32 in
      List.iter
        (fun it ->
          match it with
          | FunDecl (Fun (name, params, body_opt)) ->
              let param_count = List.length params in
              if Hashtbl.mem func_table name then
                let existing_count, defined = Hashtbl.find func_table name in
                if existing_count <> param_count then
                  failwith ("Declaration mismatch for function " ^ name)
                else if defined && body_opt <> None then
                  failwith ("Function redefined: " ^ name)
                else if body_opt <> None then
                  Hashtbl.replace func_table name (existing_count, true)
                else
                  ()
              else
                Hashtbl.add func_table name (param_count, body_opt <> None)
          | _ -> ())
        items;

      let rec check_exp local_vars e =
        match e with
        | FunCall (n, args) ->
            if not (Hashtbl.mem func_table n) then
              failwith ("Call to undeclared function: " ^ n);
            let pc, _ = Hashtbl.find func_table n in
            if pc <> List.length args then
              failwith ("Function call argument count mismatch for " ^ n);
            List.iter (check_exp local_vars) args
        | UnOp (_, inner) -> check_exp local_vars inner
        | Conditional (c, t, e2) ->
            check_exp local_vars c;
            check_exp local_vars t;
            check_exp local_vars e2
        | BinOp (l, _, r) ->
            check_exp local_vars l;
            check_exp local_vars r
        | Assign (name, rhs) ->
            (* must be declared either locally (before use) or globally *)
            if
              (not (List.mem name local_vars))
              && not (Hashtbl.mem var_table name)
            then
              failwith ("Undeclared variable: " ^ name);
            check_exp local_vars rhs
        | CompoundAssign (name, _, rhs) ->
            if
              (not (List.mem name local_vars))
              && not (Hashtbl.mem var_table name)
            then
              failwith ("Undeclared variable: " ^ name);
            check_exp local_vars rhs
        | PostfixInc name | PostfixDec name | PrefixInc name | PrefixDec name ->
            if
              (not (List.mem name local_vars))
              && not (Hashtbl.mem var_table name)
            then
              failwith ("Undeclared variable: " ^ name)
        | Const _ -> ()
        | Var name ->
            if
              (not (List.mem name local_vars))
              && not (Hashtbl.mem var_table name)
            then
              failwith ("Undeclared variable: " ^ name)
      in

      let rec check_stmt local_vars s =
        match s with
        | Return e -> check_exp local_vars e
        | Exp (Some e) -> check_exp local_vars e
        | Exp None -> ()
        | If (c, t, eo) -> (
            check_exp local_vars c;
            check_stmt local_vars t;
            match eo with
            | Some s -> check_stmt local_vars s
            | None -> ())
        | Block items ->
            let rec loop block_locals = function
              | [] -> ()
              | Statement st :: rest ->
                  let visible = local_vars @ block_locals in
                  check_stmt visible st;
                  loop block_locals rest
              | Declaration (Declare (name, init_opt)) :: rest ->
                  let visible_for_init = local_vars @ block_locals in
                  (match init_opt with
                  | Some e -> check_exp visible_for_init e
                  | None -> ());
                  if List.mem name block_locals then
                    failwith ("Variable declared twice in same scope: " ^ name);
                  loop (name :: block_locals) rest
            in
            loop [] items
        | For (init_opt, cond, post_opt, body) ->
            (match init_opt with
            | Some e -> check_exp local_vars e
            | None -> ());
            check_exp local_vars cond;
            (match post_opt with
            | Some e -> check_exp local_vars e
            | None -> ());
            check_stmt local_vars body
        | ForDecl (Declare (name, init_opt), cond, post_opt, body) ->
            (match init_opt with
            | Some e -> check_exp local_vars e
            | None -> ());
            let visible = name :: local_vars in
            check_exp visible cond;
            (match post_opt with
            | Some e -> check_exp visible e
            | None -> ());
            check_stmt visible body
        | While (c, b) ->
            check_exp local_vars c;
            check_stmt local_vars b
        | Do (b, c) ->
            check_stmt local_vars b;
            check_exp local_vars c
        | Break | Continue -> ()
      in

      List.iter
        (fun it ->
          match it with
          | FunDecl (Fun (_name, params, Some body)) ->
              let param_set = List.fold_left (fun s n -> n :: s) [] params in
              (* walk the block items, tracking declared locals and using current var_table for globals *)
              let rec loop locals = function
                | [] -> ()
                | Statement st :: rest ->
                    check_stmt locals st;
                    loop locals rest
                | Declaration (Declare (n, init_opt)) :: rest ->
                    if List.mem n locals then
                      failwith ("Variable declared twice in same scope: " ^ n);
                    if List.mem n param_set then
                      failwith ("Variable redeclared: " ^ n);
                    (match init_opt with
                    | Some e -> check_exp locals e
                    | None -> ());
                    loop (n :: locals) rest
              in
              loop param_set body
          | FunDecl (Fun (_, _, None)) -> ()
          | VarDecl (Declare (vname, init_opt)) ->
              (* adding a global now: check conflicts with function names *)
              if Hashtbl.mem func_table vname then
                failwith ("Symbol used as both function and variable: " ^ vname)
              else if Hashtbl.mem var_table vname then
                (* existing entry: allow multiple declarations, but error if both have initializers *)
                let existing = Hashtbl.find var_table vname in
                match (existing, init_opt) with
                | Some _, Some _ ->
                    failwith ("Global variable declared twice: " ^ vname)
                | None, Some _ -> Hashtbl.replace var_table vname init_opt
                | _ -> ()
              else
                Hashtbl.add var_table vname init_opt)
        items;

      Hashtbl.iter
        (fun v init_opt ->
          match init_opt with
          | Some (Const _) -> ()
          | Some _ -> failwith ("Global initializer must be a constant: " ^ v)
          | None -> ())
        var_table
