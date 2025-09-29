type token =
  (* data *)
  | ID of string
  | NUM of int
  (* keywords *)
  | INT
  | RETURN
  (* syntax *)
  | LPAREN
  | RPAREN
  | SEMICOLON
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | WHITESPACE
  (* operators *)
  | NEGATION
  | BWCOMPLIMENT
  | LGNEGATION

type unop =
  | NEGATION
  | BWCOMPLIMENT
  | LGNEGATION

type exp =
  | Const of int
  | UnOp of unop * exp

type statement = Return of exp
type fun_decl = Fun of string * statement list
type prog = Prog of fun_decl list
