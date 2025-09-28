open Re
open Ast

let word_re = seq [ alpha; rep (alt [ alnum; char '_' ]) ]
let special_chars_re = set "[]{}();"
let integer_re = rep1 digit
let hex_digits_manual_re = set "0-9a-fA-F"
let hex_re = seq [ alt [ str "0x"; str "0X" ]; rep1 hex_digits_manual_re ]
let octal_digits_manual_re = set "0-7"
let octal_re = seq [ str "0"; rep1 octal_digits_manual_re ]
let whitespace_re = rep1 space

let re =
  alt [ word_re; special_chars_re; integer_re; hex_re; octal_re; whitespace_re ]
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
    (* syntax *)
    | "(" -> LPAREN
    | ")" -> RPAREN
    | ";" -> SEMICOLON
    | "[" -> LBRACKET
    | "]" -> RBRACKET
    | "{" -> LBRACE
    | "}" -> RBRACE
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
