open Re

type keyword =
  | Int
  | Return

let keywords = ["int"; "return"]
let keywords_re = alt (List.map str keywords)
let special_chars_re = set "[]{}();"
let identifier_re = rep1 alpha
let integer_re = rep1 digit

let hex_digits_manual_re = set "0-9a-fA-F"
let hex_re = seq [alt [str "0x"; str "0X"]; rep1 hex_digits_manual_re]

let octal_digits_manual_re = set "0-7"
let octal_re = seq [ str "0"; rep1 octal_digits_manual_re]

let re =
  alt [
    keywords_re;
    special_chars_re;
    identifier_re;
    integer_re;
    hex_re;
    octal_re]
  |> compile

let read_file filename =
  let input_file = In_channel.with_open_text filename In_channel.input_all in input_file

let lex filename =
  let file = read_file filename in matches re file
