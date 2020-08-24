type token =
  | T_and
  | T_or
  | T_char
  | T_decl
  | T_def
  | T_exit
  | T_list
  | T_head
  | T_tail
  | T_mod
  | T_new
  | T_nil
  | T_nil_qm
  | T_not
  | T_ref
  | T_return
  | T_skip
  | T_for
  | T_end
  | T_if
  | T_else
  | T_elsif
  | T_int
  | T_bool
  | T_true
  | T_false
  | T_intconst of (int)
  | T_id of (Ast.id)
  | T_charconst of (char)
  | T_stringconst of (string)
  | T_eq
  | T_uneq
  | T_leq
  | T_geq
  | T_lower
  | T_greater
  | T_plus
  | T_minus
  | T_times
  | T_div
  | T_cons
  | T_rparen
  | T_lparen
  | T_lbrack
  | T_rbrack
  | T_comma
  | T_semicol
  | T_assign
  | T_colon
  | T_eof

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.ast_defdecl
