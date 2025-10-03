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
  | ASSIGN
  | COMMA
  | INCREMENT
  | DECREMENT
  | COMPOUND_ADD
  | COMPOUND_SUB
  | COMPOUND_MUL
  | COMPOUND_DIV
  | COMPOUND_MOD
  | COMPOUND_LSHIFT
  | COMPOUND_RSHIFT
  | COMPOUND_AND
  | COMPOUND_OR
  | COMPOUND_XOR
  | IF
  | ELSE
  | QUESTION
  | COLON

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
  | Comma

type exp =
  | Const of int
  | UnOp of unop * exp
  | BinOp of exp * binop * exp
  | Var of string
  | Assign of string * exp
  | CompoundAssign of string * binop * exp
  | PostfixInc of string
  | PostfixDec of string
  | PrefixInc of string
  | PrefixDec of string
  | Conditional of exp * exp * exp

type statement =
  | Return of exp
  | Exp of exp
  | If of exp * statement * statement option

type declaration = Declare of string * exp option

type block_item =
  | Statement of statement
  | Declaration of declaration

type fun_decl = Fun of string * block_item list
type prog = Prog of fun_decl list
