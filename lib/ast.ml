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
  | MINUS
  | BWCOMPLIMENT
  | LGNEGATION
  (* binary operators *)
  | ADD
  | SUBTRACT
  | MULTIPLY
  | DIVIDE
  | AND
  | OR
  | EQUAL
  | NOT_EQUAL
  | LESS
  | LESS_EQUAL
  | GREATER
  | GREATER_EQUAL
  | MODULO
  | BW_AND
  | BW_OR
  | BW_XOR
  | LSHIFT
  | RSHIFT

type unop =
  | NEGATION
  | BWCOMPLIMENT
  | LGNEGATION

type binop =
  | Add
  | Subtract
  | Multiply
  | Divide
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | LessEqual
  | Greater
  | GreaterEqual
  | Modulo
  | BitwiseAnd
  | BitwiseOr
  | BitwiseXor
  | LShift
  | RShift

type exp =
  | Const of int
  | UnOp of unop * exp
  | BinOp of exp * binop * exp

type statement = Return of exp
type fun_decl = Fun of string * statement list
type prog = Prog of fun_decl list
