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
  | FOR
  | WHILE
  | DO
  | BREAK
  | CONTINUE

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
  | FunCall of string * exp list
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
  | Exp of exp option
  | If of exp * statement * statement option
  | Block of block_item list
  | For of exp option * exp * exp option * statement
  | ForDecl of declaration * exp * exp option * statement
  | While of exp * statement
  | Do of statement * exp
  | Break
  | Continue

and declaration = Declare of string * exp option

and block_item =
  | Statement of statement
  | Declaration of declaration

type fun_decl = Fun of string * string list * block_item list option
type prog = Prog of fun_decl list
