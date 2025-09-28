open Coco

let write_to_file filename content =
  let oc = open_out filename in
  Printf.fprintf oc "%s" content;
  close_out oc

let () =
  let intput_filename = Sys.argv.(1) in
  let output_filename = Filename.chop_extension intput_filename ^ ".s" in

  let tokens = Lexer.lex intput_filename in
  let ast = Parser.parse tokens in
  let asm_code = Generator.generate ast in

  write_to_file output_filename asm_code;
  Printf.printf "Generated assembly file: %s\n" output_filename
