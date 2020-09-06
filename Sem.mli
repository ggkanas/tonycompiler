exception TypeError of Types.typ * Types.typ * int
exception IndexBoundError of int
exception IndexTypeError of int
exception NullPtrError of int
exception ZeroDivError of int
exception InternalError of int
exception ExitError of int
exception LValueError of int * int              (*1 = assignment to non-lvalue,       *)
                                                (*2 = non-lvalue reference argument   *)
exception IgnoredResultError of int
exception WrongNumberArgsError of int * int     (*1 = Too Few Arguments, 2 = Too Many *)
exception ForError of int * int                 (*1 = Init Error, 2 = Increment Error *)
exception WrongIdError of int * string * int    (*1 = not variable, 2 = not function  *)
exception TypeError2 of Types.typ * string * int
exception TypeError3 of string * Types.typ * int
exception MainParamError of int

val sem : Ast.ast_defdecl_lc -> unit
val sem_init : unit -> unit
