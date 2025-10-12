open Coco

let write_to_file filename content =
  let oc = open_out filename in
  Printf.fprintf oc "%s" content;
  close_out oc

let () =
  let input_filename = Sys.argv.(1) in
  let output_filename =
    try Filename.chop_extension input_filename ^ ".s"
    with Invalid_argument _ -> input_filename ^ ".s"
  in

  let tokens = Lexer.lex input_filename in
  let ast = Parser.parse tokens in
  (* validate AST for functions and calls *)
  (try Coco.Validator.validate_prog ast
   with Failure msg ->
     Printf.eprintf "%s\n" msg;
     exit 255);
  let asm_code = Generator.generate ast in

  write_to_file output_filename asm_code;
  Printf.printf "Generated assembly file: %s\n" output_filename
