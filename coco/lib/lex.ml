type keyword =
  | Int
  | Return

let file_name = Sys.argv.(1)
let input_file = In_channel.with_open_text file_name In_channel.input_all;;

print_endline input_file
