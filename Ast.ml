type id = string
type typ = TY_void
         | TY_int
         | TY_char
         | TY_bool
         | TY_array of typ * int
         | TY_list of typ
         | TY_proc
         | TY_string

type u_oper = UP_plus | UP_minus | UP_not | UP_nil | UP_head | UP_tail
type b_oper = BP_plus | BP_minus | BP_times | BP_div | BP_mod | BP_eq
            | BP_uneq | BP_geq | BP_leq | BP_lower | BP_greater | BP_and
            | BP_or | BP_cons



type ast_simple =
| S_skip
| S_assign of ast_atom * ast_expr
| S_call of ast_call

and ast_stmt =
| ST_simple of ast_simple
| ST_exit
| ST_return of ast_expr
| ST_if of ast_expr * ast_stmt list * (ast_expr * ast_stmt list) list * ast_stmt list
| ST_for of ast_simple list * ast_expr * ast_simple list * ast_stmt list

and ast_call = id * ast_expr list


and ast_atom =
| A_id of id * typ
| A_string of string
| A_atom_el of ast_atom * ast_expr
| A_call of ast_call

and ast_expr =
| E_atom of ast_atom
| E_int_const of int
| E_char_const of char
| E_unary_op of u_oper * ast_expr
| E_binary_op of ast_expr * b_oper * ast_expr
| E_bool_const of bool
| E_nil
| E_new of typ * ast_expr

type ast_formal = bool * ast_defdecl

and ast_header = typ * id * ast_formal list

and ast_defdecl =
| D_func_decl of ast_header
| D_func_def of ast_header * ast_defdecl list * ast_stmt list
| D_var_def of typ * id list
