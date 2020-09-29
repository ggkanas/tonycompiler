{
open Lexing
open Parser
open String
open Error
open LC
let escape_to_char s =
    match s with
    | "\\n" -> '\n'
    | "\\t" -> '\t'
    | "\\r" -> '\r'
    | "\\0" -> '\x00'
    | "\\\\" -> '\\'
    | "\\\'" -> '\''
    | "\\\"" -> '"'
    | str -> Char.chr (int_of_string (String.concat "" ["0";  String.sub str 1 3]))
}

let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let white  = [' ' '\t' '\r']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let escapeseq = "\\" (['n' 't' 'r' '0']) | "\\\\" | "\\\'" | "\\\"" | ('x' hex hex)
let commonchar = ['a'-'z' 'A'-'Z' '0'-'9' '!' '#' '$' '%' '&' '(' ')' '*' '+' ',' '-' '.' '/' ':' ';' '<' '=' '>' '?' '@' '[' ']' ' ' '^' '_' '`' '{' '|' '}' '~']



rule lexer = parse
    "and"    { T_and }
  | "or"     { T_or }
  | "char"   { T_char }
  | "decl"   { T_decl}
  | "def"    { T_def }
  | "exit"   { T_exit }
  | "list"   { T_list }
  | "head"   { T_head }
  | "tail"   { T_tail }
  | "mod"    { T_mod }
  | "new"    { T_new }
  | "nil"    { T_nil }
  | "nil?"   { T_nil_qm }
  | "not"    { T_not }
  | "ref"    { T_ref }
  | "return" { T_return }
  | "skip"   { T_skip }
  | "for"    { T_for }
  | "end"    { T_end }
  | "if"     { T_if }
  | "else"   { T_else }
  | "elsif"  { T_elsif }
  | "int"    { T_int }
  | "bool"   { T_bool }
  | "false"  { T_false }
  | "true"   { T_true }

  | digit+                               { T_intconst (int_of_string (lexeme lexbuf)) }
  | letter (letter | digit | ['_' '?'])* { T_id (lexeme lexbuf) }
  | "\'" (commonchar) "\'"   { let str = lexeme lexbuf in T_charconst(String.get str 2) }
  | "\'" (escapeseq) "\'"   { let str = lexeme lexbuf in T_charconst(escape_to_char (sub str 1 ((length str)-2))) }
  | "\"" (commonchar | escapeseq)* "\""  {let str = lexeme lexbuf in T_stringconst(sub str 1 ((length str)-2)) }

  | '='      { T_eq }
  | "<>"     { T_uneq }
  | "<="     { T_leq }
  | ">="     { T_geq }
  | '<'      { T_lower }
  | '>'      { T_greater }
  | '+'      { T_plus }
  | '-'      { T_minus }
  | '*'      { T_times }
  | '/'      { T_div }
  | '#'      { T_cons }

  | '('      { T_lparen }
  | ')'      { T_rparen }
  | '['      { T_lbrack }
  | ']'      { T_rbrack }
  | ','      { T_comma }
  | ';'      { T_semicol }
  | ":="     { T_assign }
  | ':'      { T_colon }

  | '\n'                 { incr LC.linecount; lexer lexbuf }
  | white+               { lexer lexbuf }
  | "%" [^ '\n']* "\n"   { incr LC.linecount; lexer lexbuf }
  | "<*"                 { comments 0 lexbuf}

  |  eof          { T_eof }
  |  _ as chr     { fatal "invalid character: '%c' (ascii: %d) at line %d\n"
                      chr (Char.code chr) !LC.linecount;
                      lexer lexbuf }
and comments level = parse
  | "*>"      { if level = 0 then lexer lexbuf else comments (level-1) lexbuf }
  | "<*"      { comments (level+1) lexbuf }
  | '\n'      { incr LC.linecount; comments level lexbuf }
  | _         { comments level lexbuf }
  | eof       { raise End_of_file }
