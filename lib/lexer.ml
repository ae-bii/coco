open Re
open Ast

let word_re = seq [ alpha; rep (alt [ alnum; char '_' ]) ]
let single_chars_re = set "[]{}();~!-+*/<>=,?:%"
let integer_re = rep1 digit
let hex_digits_manual_re = set "0-9a-fA-F"
let hex_re = seq [ alt [ str "0x"; str "0X" ]; rep1 hex_digits_manual_re ]
let octal_digits_manual_re = set "0-7"
let octal_re = seq [ str "0"; rep1 octal_digits_manual_re ]
let whitespace_re = rep1 space

let multi_char_op_re =
  alt
    [
      str "&&";
      str "||";
      str "==";
      str "!=";
      str "<=";
      str ">=";
      str "<<";
      str ">>";
      str "+=";
      str "-=";
      str "*=";
      str "/=";
      str "%=";
      str "<<=";
      str ">>=";
      str "&=";
      str "|=";
      str "^=";
      str "++";
      str "--";
    ]

let re =
  alt
    [
      word_re;
      integer_re;
      hex_re;
      octal_re;
      multi_char_op_re;
      single_chars_re;
      whitespace_re;
    ]
  |> compile

let read_file filename =
  let input_file = In_channel.with_open_text filename In_channel.input_all in
  input_file

let string_to_token s =
  if String.trim s = "" then
    WHITESPACE
  else
    match s with
    (* keywords *)
    | "int" -> INT
    | "return" -> RETURN
    | "if" -> IF
    | "else" -> ELSE
    | "for" -> FOR
    | "while" -> WHILE
    | "do" -> DO
    | "break" -> BREAK
    | "continue" -> CONTINUE
    (* syntax *)
    | "(" -> LPAREN
    | ")" -> RPAREN
    | ";" -> SEMICOLON
    | "[" -> LBRACKET
    | "]" -> RBRACKET
    | "{" -> LBRACE
    | "}" -> RBRACE
    | "?" -> QUESTION
    | ":" -> COLON
    (* unary operators *)
    | "-" -> MINUS
    | "~" -> BWCOMPLIMENT
    | "!" -> LGNEGATION
    (* binary operators *)
    | "+" -> ADD
    | "*" -> MULTIPLY
    | "/" -> DIVIDE
    | "&&" -> AND
    | "||" -> OR
    | "==" -> EQUAL
    | "!=" -> NOT_EQUAL
    | "<" -> LESS
    | "<=" -> LESS_EQUAL
    | ">" -> GREATER
    | ">=" -> GREATER_EQUAL
    | "%" -> MODULO
    | "&" -> BW_AND
    | "|" -> BW_OR
    | "^" -> BW_XOR
    | "<<" -> LSHIFT
    | ">>" -> RSHIFT
    | "=" -> ASSIGN
    | "," -> COMMA
    | "++" -> INCREMENT
    | "--" -> DECREMENT
    | "+=" -> COMPOUND_ADD
    | "-=" -> COMPOUND_SUB
    | "*=" -> COMPOUND_MUL
    | "/=" -> COMPOUND_DIV
    | "%=" -> COMPOUND_MOD
    | "<<=" -> COMPOUND_LSHIFT
    | ">>=" -> COMPOUND_RSHIFT
    | "&=" -> COMPOUND_AND
    | "|=" -> COMPOUND_OR
    | "^=" -> COMPOUND_XOR
    (* default for variable data (Numbers and IDs) *)
    | _ ->
        if String.length s > 0 then
          let first_char = s.[0] in
          if first_char >= '0' && first_char <= '9' then
            NUM (int_of_string s)
          else
            ID s
        else
          failwith "Lexer produced an empty token"

let lex filename =
  let file = read_file filename in
  matches re file |> List.map string_to_token
