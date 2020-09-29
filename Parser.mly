%{
    open Ast
    open Types
    open Error
    open LC

    let second t =
        match t with
        | (_, b, _) -> b
        (*| _ -> raise Terminate in
            Printf.eprintf ("Internal error occurred at %s:%d,@ tuple has no second element" "Parser.mly" 9)*)
    let third t =
        match t with
        | (_, _, c) -> c
        (*| _ -> raise Terminate in
            Printf.eprintf ("Internal error occurred at %s:%d,@ tuple has no third element" "Parser.mly" 9)*)
%}



%token T_and
%token T_or
%token T_char
%token T_decl
%token T_def
%token T_exit
%token T_list
%token T_head
%token T_tail
%token T_mod
%token T_new
%token T_nil
%token T_nil_qm
%token T_not
%token T_ref
%token T_return
%token T_skip
%token T_for
%token T_end
%token T_if
%token T_else
%token T_elsif
%token T_int
%token T_bool
%token T_true
%token T_false

%token<int> T_intconst
%token<Ast.id> T_id
%token<char> T_charconst
%token<string> T_stringconst

%token T_eq
%token T_uneq
%token T_leq
%token T_geq
%token T_lower
%token T_greater
%token T_plus
%token T_minus
%token T_times
%token T_div
%token T_cons


%token T_rparen
%token T_lparen
%token T_lbrack
%token T_rbrack
%token T_comma
%token T_semicol
%token T_assign
%token T_colon

%token T_eof

%left T_or
%left T_and
%nonassoc T_not
%nonassoc T_eq T_leq T_geq T_greater T_lower T_uneq
%right T_cons
%left T_plus T_minus
%left T_times T_div T_mod
%nonassoc T_NEG

%start program
%type <Ast.ast_defdecl_lc> program
%type <ast_defdecl> func_def
%type <ast_header> header
%type <ast_formal> formal
%type <typ> type
%type <ast_defdecl> func_decl
%type <ast_defdecl> var_def
%type <ast_stmt_lc> stmt
%type <ast_simple> simple
%type <ast_simple list> simple_list
%type <ast_call> call
%type <ast_atom> atom
%type <ast_expr_lc> expr

%type <ast_defdecl_lc list> defdecl_list
%type <ast_stmt_lc list> stmt_list
%type <ast_header> header_rest
%type <ast_formal list> formal_list
%type <id list> id_list
%type <(ast_expr_lc * ast_stmt_lc list) list> elsif_list
%type <ast_stmt_lc list> else_part
%type <ast_expr_lc list> expr_list
%type <ast_expr_lc list> call_rest

%%

program      : func_def { ($1, !linecount) }

func_def     : T_def header T_colon defdecl_list stmt stmt_list T_end { D_func_def ($2, $4, ($5::$6)) }

header       : type header_rest { ($1, second $2, third $2) }
             | header_rest { (TY_proc, second $1, third $1) }

header_rest  : T_id T_lparen formal_list T_rparen { (TY_none, $1, $3) }
             | T_id T_lparen T_rparen { (TY_none, $1, []) }

formal_list  : formal { ([$1]) }
             | formal T_semicol formal_list { ($1 :: $3) }

formal       : T_ref var_def { (Symbol.PASS_BY_REFERENCE, ($2, !linecount)) }
             | var_def { (Symbol.PASS_BY_VALUE, ($1, !linecount)) }

type         : T_int { TY_int }
             | T_bool { TY_bool }
             | T_char { TY_char }
             | type T_lbrack T_rbrack { TY_array ($1, 0) }
             | T_list T_lbrack type T_rbrack { TY_list ($3) }

defdecl_list : /* nothing */ { ([]) }
             | func_def defdecl_list { (($1, !linecount) :: $2) }
             | func_decl defdecl_list { (($1, !linecount) :: $2) }
             | var_def defdecl_list { (($1, !linecount) :: $2) }

func_decl    : T_decl header { D_func_decl ($2) }

id_list      : /* nothing */ { ([]) }
             | T_comma T_id id_list { ($2 :: $3) }

var_def      : type T_id id_list { D_var_def($1, $2 :: $3) }

stmt_list    : /* nothing */ { ([]) }
             | stmt stmt_list { ($1 :: $2) }

elsif_list   : /* nothing */ { ([]) }
             | T_elsif expr T_colon stmt stmt_list elsif_list { (($2, ($4 :: $5)) :: $6) }

else_part    : /* nothing */ { ([]) }
             | T_else  T_colon stmt stmt_list { ($3 :: $4) }

stmt         : simple { (ST_simple ($1), !linecount) }
             | T_exit { (ST_exit, !linecount) }
             | T_return expr { (ST_return($2), !linecount) }
             | T_if expr T_colon stmt stmt_list elsif_list else_part T_end { (ST_if($2, ($4 :: $5), $6, $7), !linecount) }
             | T_for simple_list T_semicol expr T_semicol simple_list T_colon stmt stmt_list T_end
                { (ST_for($2, $4, $6, ($8 :: $9)), !linecount) }

simple       : T_skip { S_skip }
             | atom T_assign expr { S_assign($1, $3) }
             | call { S_call($1) }

simple_list  : simple { ([$1]) }
             | simple T_comma simple_list { ($1 :: $3) }

call         : T_id T_lparen call_rest { ($1, $3) }

call_rest    : T_rparen { ([]) }
             | expr_list T_rparen { ($1) }

expr_list    : expr { ([$1]) }
             | expr T_comma expr_list { ($1 :: $3) }

atom         : T_id { A_id($1) }
             | T_stringconst { A_string($1) }
             | atom T_lbrack expr T_rbrack { A_atom_el($1, $3) }
             | call { A_call($1) }



expr         : atom { (E_atom($1), !linecount) }
             | T_intconst { (E_int_const($1), !linecount) }
             | T_charconst { (E_char_const($1), !linecount) }
             | T_lparen expr T_rparen { ($2) }
             | T_plus expr %prec T_NEG { (E_unary_op(UP_plus, $2), !linecount) }
             | T_minus expr %prec T_NEG { (E_unary_op(UP_minus, $2), !linecount) }

             | expr T_plus expr { (E_binary_op($1, BP_plus, $3), !linecount) }
             | expr T_minus expr { (E_binary_op($1, BP_minus, $3), !linecount) }
             | expr T_times expr { (E_binary_op($1, BP_times, $3), !linecount) }
             | expr T_div expr { (E_binary_op($1, BP_div, $3), !linecount) }
             | expr T_mod expr { (E_binary_op($1, BP_mod, $3), !linecount) }
             | expr T_cons expr { (E_binary_op($1, BP_cons, $3), !linecount) }
             | expr T_eq expr { (E_binary_op($1, BP_eq, $3), !linecount) }
             | expr T_uneq expr { (E_binary_op($1, BP_uneq, $3), !linecount) }
             | expr T_lower expr { (E_binary_op($1, BP_lower, $3), !linecount) }
             | expr T_greater expr { (E_binary_op($1, BP_greater, $3), !linecount) }
             | expr T_leq expr { (E_binary_op($1, BP_leq, $3), !linecount) }
             | expr T_geq expr { (E_binary_op($1, BP_geq, $3), !linecount) }
             | expr T_and expr { (E_binary_op($1, BP_and, $3), !linecount) }
             | expr T_or expr { (E_binary_op($1, BP_or, $3), !linecount) }
             | T_true { (E_bool_const(true), !linecount) }
             | T_false { (E_bool_const(false), !linecount) }
             | T_not expr { (E_unary_op(UP_not, $2), !linecount) }
             | T_new type T_lbrack expr T_rbrack { (E_new($2, $4), !linecount) }
             | T_nil { (E_nil, !linecount) }
             | T_nil_qm T_lparen expr T_rparen { (E_unary_op(UP_nil, $3), !linecount) }
             | T_head T_lparen expr T_rparen { (E_unary_op(UP_head, $3), !linecount) }
             | T_tail T_lparen expr T_rparen { (E_unary_op(UP_tail, $3), !linecount) }
