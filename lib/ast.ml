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
  (* unary operators *)
  | NEGATION
  | BWCOMPLIMENT
  | LGNEGATION
  (* binary operators *)
  | ADDITION
  | MULTIPLICATION
  | DIVISION

type unop =
  | NEGATION
  | BWCOMPLIMENT
  | LGNEGATION

type binop =
  | ADDITION
  | MULTIPLICATION
  | DIVISION

type exp =
  | Const of int
  | UnOp of unop * exp
  | BinOp of exp * binop * exp

type statement = Return of exp
type fun_decl = Fun of string * statement list
type prog = Prog of fun_decl list
