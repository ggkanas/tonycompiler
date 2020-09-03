exception TypeError of Types.typ * Types.typ
exception IndexBoundError
exception IndexTypeError
exception NullPtrError
exception ZeroDivError
exception InternalError
exception ExitError
exception LValueError of int            (*1 = assignment to non-lvalue,       *)
                                        (*2 = non-lvalue reference argument   *)
exception IgnoredResultError
exception WrongNumberArgsError of int   (*1 = Too Few Arguments, 2 = Too Many *)
exception ForError of int               (*1 = Init Error, 2 = Increment Error *)
exception WrongIdError of int * string  (*1 = not variable, 2 = not function  *)
                                        (*3 = not array *)
exception TypeError2 of Types.typ * string
exception TypeError3 of string * Types.typ

val sem : Ast.ast_defdecl -> unit
