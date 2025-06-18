let tokens = Coco.Lex.lex Sys.argv.(1);;

print_endline (String.concat " " tokens)
