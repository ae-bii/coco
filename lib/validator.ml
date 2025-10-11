open Ast

(* validation pass: enforce function declaration/definition rules and call arg counts *)
let validate_prog (p : prog) : unit =
  match p with
  | Prog funcs ->
      let table = Hashtbl.create 32 in
      List.iter
        (fun f ->
          match f with
          | Fun (name, params, body_opt) ->
              let param_count = List.length params in
              if Hashtbl.mem table name then
                let existing_count, defined = Hashtbl.find table name in
                if existing_count <> param_count then
                  failwith ("Declaration mismatch for function " ^ name)
                else if defined && body_opt <> None then
                  failwith ("Function redefined: " ^ name)
                else if body_opt <> None then
                  Hashtbl.replace table name (existing_count, true)
                else
                  ()
              else
                Hashtbl.add table name (param_count, body_opt <> None))
        funcs;

      (* traverse bodies to find calls and check arg counts and existence *)
      let rec check_exp e =
        match e with
        | FunCall (n, args) ->
            if not (Hashtbl.mem table n) then
              failwith ("Call to undeclared function: " ^ n);
            let pc, _ = Hashtbl.find table n in
            if pc <> List.length args then
              failwith ("Function call argument count mismatch for " ^ n)
        | UnOp (_, inner) -> check_exp inner
        | Conditional (c, t, e2) ->
            check_exp c;
            check_exp t;
            check_exp e2
        | BinOp (l, _, r) ->
            check_exp l;
            check_exp r
        | Assign (_, rhs) -> check_exp rhs
        | CompoundAssign (_, _, rhs) -> check_exp rhs
        | PostfixInc _
        | PostfixDec _
        | PrefixInc _
        | PrefixDec _
        | Const _
        | Var _ -> ()
      in
      let rec check_stmt s =
        match s with
        | Return e -> check_exp e
        | Exp (Some e) -> check_exp e
        | Exp None -> ()
        | If (c, t, eo) -> (
            check_exp c;
            check_stmt t;
            match eo with
            | Some s -> check_stmt s
            | None -> ())
        | Block items ->
            List.iter
              (fun it ->
                match it with
                | Statement st -> check_stmt st
                | Declaration (Declare (_, Some e)) -> check_exp e
                | Declaration _ -> ())
              items
        | For (init_opt, cond, post_opt, body) ->
            (match init_opt with
            | Some e -> check_exp e
            | None -> ());
            check_exp cond;
            (match post_opt with
            | Some e -> check_exp e
            | None -> ());
            check_stmt body
        | ForDecl (Declare (_, init_opt), cond, post_opt, body) ->
            (match init_opt with
            | Some e -> check_exp e
            | None -> ());
            check_exp cond;
            (match post_opt with
            | Some e -> check_exp e
            | None -> ());
            check_stmt body
        | While (c, b) ->
            check_exp c;
            check_stmt b
        | Do (b, c) ->
            check_stmt b;
            check_exp c
        | Break | Continue -> ()
      in
      List.iter
        (fun f ->
          match f with
          | Fun (_, params, Some body) ->
              let param_set = List.fold_left (fun s n -> n :: s) [] params in
              List.iter
                (fun it ->
                  match it with
                  | Statement st -> check_stmt st
                  | Declaration (Declare (name, Some e)) ->
                      if List.mem name param_set then
                        failwith ("Variable redeclared: " ^ name);
                      check_exp e
                  | Declaration (Declare (name, None)) ->
                      if List.mem name param_set then
                        failwith ("Variable redeclared: " ^ name)
                      else
                        ())
                body
          | Fun _ -> ())
        funcs
