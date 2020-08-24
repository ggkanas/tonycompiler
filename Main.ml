let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    let ast = Parser.program Lexer.lexer lexbuf in
        exit 0
  with Error.Terminate ->
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d\n" !Lexer.linecount;
    exit 1
