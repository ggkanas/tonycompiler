%{
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
%token<string> T_id
%token<string> T_charconst
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
%type <unit> program
%type <unit> func_def
%type <unit> header
%type <unit> formal
%type <unit> type
%type <unit> func_decl
%type <unit> var_def
%type <unit> stmt
%type <unit> simple
%type <unit> simple_list
%type <unit> call
%type <unit> atom
%type <unit> expr

%type <unit> defdecl_list
%type <unit> stmt_list
%type <unit> header_rest
%type <unit> formal_list
%type <unit> id_list
%type <unit> elsif_list
%type <unit> else_part
%type <unit> expr_list
%type <unit> operator
%type <unit> call_rest

%%

program      : func_def { () }

func_def     : T_def header T_colon defdecl_list stmt stmt_list T_end { () }

header       : type header_rest { () }
             | header_rest { () }

header_rest  : T_id T_lparen formal_list T_rparen { () }
             | T_id T_lparen T_rparen { () }

formal_list  : formal { () }
             | formal T_semicol formal_list { () }

formal       : T_ref var_def { () }
             | var_def { () }

type         : T_int { () }
             | T_bool { () }
             | T_char { () }
             | type T_lbrack T_rbrack { () }
             | T_list T_lbrack type T_rbrack { () }

defdecl_list : /* nothing */ { () }
             | func_def defdecl_list { () }
             | func_decl defdecl_list { () }
             | var_def defdecl_list { () }

func_decl    : T_decl header { () }

id_list      : /* nothing */ { () }
             | T_comma T_id id_list { () }

var_def      : type T_id id_list { () }

stmt_list    : /* nothing */ { () }
             | stmt stmt_list { () }

elsif_list   : /* nothing */ { () }
             | T_elsif expr T_colon stmt stmt_list elsif_list { () }

else_part    : /* nothing */ { () }
             | T_else  T_colon stmt stmt_list { () }

stmt         : simple { () }
             | T_exit { () }
             | T_return expr { () }
             | T_if expr T_colon stmt stmt_list elsif_list else_part T_end { () }
             | T_for simple_list T_semicol expr T_semicol simple_list T_colon stmt stmt_list T_end { () }

simple       : T_skip { () }
             | atom T_assign expr { () }
             | call { () }

simple_list  : simple { () }
             | simple T_comma simple_list { () }

call         : T_id T_lparen call_rest { () }

call_rest    : T_rbrack { () }
             | expr_list T_rparen { () }

expr_list    : expr { () }
             | expr T_comma expr_list { () }

atom         : T_id { () }
             | T_stringconst { () }
             | atom T_lbrack expr T_rbrack { () }
             | call { () }

operator     : T_plus { () }
             | T_minus { () }
             | T_times { () }
             | T_div { () }
             | T_mod { () }
             | T_cons { () }
             | T_eq { () }
             | T_uneq { () }
             | T_lower { () }
             | T_greater { () }
             | T_leq { () }
             | T_geq { () }
             | T_and { () }
             | T_or { () }

expr         : atom { () }
             | T_intconst { () }
             | T_charconst { () }
             | T_lparen expr T_rparen { () }
             | T_plus expr %prec T_NEG { () }
             | T_minus expr %prec T_NEG { () }
             | expr operator expr { () }
             | T_true { () }
             | T_false { () }
             | T_not expr { () }
             | T_new type T_lbrack expr T_rbrack { () }
             | T_nil { () }
             | T_nil_qm T_lparen expr T_rparen { () }
             | T_head T_lparen expr T_rparen { () }
             | T_tail T_lparen expr T_rparen { () }
