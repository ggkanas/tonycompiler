open Parser

(*let main =
  let lexbuf = Lexing.from_channel stdin in
  try
    Parser.program Lexer.lexer lexbuf;
    exit 0
  with Error.Terminate ->
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "syntax error on line %d\n" !Lexer.linecount;
    exit 1*)

    let string_of_token token =
      match token with
        | T_eof    -> "T_eof"
        | T_cons  -> "T_cons"
        | T_and    -> "T_and"
        | T_or  -> "T_or"
        | T_char    -> "T_char"
        | T_for    -> "T_for"
        | T_decl     -> "T_decl"
        | T_def  -> "T_def"
        | T_end    -> "T_end"
        | T_if     -> "T_if"
        | T_elsif  -> "T_elsif"
        | T_else   -> "T_else"
        | T_exit   -> "T_exit"
        | T_eq     -> "T_eq"
        | T_lparen -> "T_lparen"
        | T_rparen -> "T_rparen"
        | T_plus   -> "T_plus"
        | T_minus  -> "T_minus"
        | T_times  -> "T_times"
        | T_list -> "T_list"
        | T_head -> "T_head"
        | T_tail -> "T_tail"
        | T_mod -> "T_mod"
        | T_new -> "T_new"
        | T_nil -> "T_nil"
        | T_nil_qm -> "T_nil_qm"
        | T_not -> "T_not"
        | T_ref -> "T_ref"
        | T_return -> "T_return"
        | T_skip -> "T_skip"
        | T_int -> "T_int"
        | T_bool -> "T_bool"
        | T_true -> "T_true"
        | T_false -> "T_false"
        | T_intconst(x) -> "T_intcost"
        | T_id(x) -> "T_id"
        | T_charconst(x) -> "T_charconst"
        | T_stringconst(x) -> "T_stringconst"
        | T_uneq -> "T_uneq"
        | T_leq -> "T_leq"
        | T_geq -> "T_geq"
        | T_lower -> "T_lower"
        | T_greater -> "T_greater"
        | T_div -> "T_div"
        | T_lbrack -> "T_lbrack"
        | T_rbrack -> "T_rbrack"
        | T_comma -> "T_comma"
        | T_semicol -> "T_semicol"
        | T_colon -> "T_colon"
        | T_assign -> "T_assign"

    let main =
      let lexbuf = Lexing.from_channel stdin in
      try
      let rec loop () =
    let token = Lexer.lexer lexbuf in
    Printf.printf "token=%s, lexeme=\"%s\"\n"
      (string_of_token token) (Lexing.lexeme lexbuf);
    if token <> T_eof then loop () in
        loop ();
        exit 0
      with Error.Terminate ->
        exit 1
      | Parsing.Parse_error ->
        Printf.eprintf "syntax error on line %d\n" !Lexer.linecount;
        exit 1
