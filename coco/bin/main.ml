open Coco

let () =
  let filename = Sys.argv.(1) in
  let tokens = Lex.lex filename in
  let ast = Parse.parse tokens in

  print_endline (Ast_printer.string_of_prog ast)
