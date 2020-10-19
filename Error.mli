(* Error handling *)

exception Terminate
exception TypeError of Types.typ * Types.typ * int
exception IndexBoundError of int
exception NullPtrError of int
exception ZeroDivError of int
exception InternalError of int
exception ExitError of int
exception ResultError of int
exception LValueError of int * int              (*1 = assignment to non-lvalue,       *)
                                                (*2 = non-lvalue reference argument   *)
exception IgnoredResultError of int
exception WrongNumberArgsError of int * int     (*1 = Too Few Arguments, 2 = Too Many *)
exception ForError of int * int                 (*1 = Init Error, 2 = Increment Error *)
exception WrongIdError of int * string * int    (*1 = not variable, 2 = not function  *)
exception TypeError2 of Types.typ * string * int
exception TypeError3 of string * Types.typ * int
exception MainParamError of int
exception NoReturnError of int

type verbose = Vquiet | Vnormal | Vverbose

val flagVerbose      : verbose ref

val numErrors        : int ref
val maxErrors        : int ref
val flagWarnings     : bool ref
val numWarnings      : int ref
val maxWarnings      : int ref

val internal_raw     : (string * int) ->
                         ('a, Format.formatter, unit) format -> 'a
val fatal            : ('a, Format.formatter, unit) format -> 'a
val fatal2           : ('a, Format.formatter, unit) format -> 'a
val error            : ('a, Format.formatter, unit) format -> 'a
val warning          : ('a, Format.formatter, unit) format -> 'a
val message          : ('a, Format.formatter, unit) format -> 'a

type position

val position_point   : Lexing.position -> position
val position_context : Lexing.position -> Lexing.position -> position
val position_dummy   : position
val print_position   : Format.formatter -> position -> unit
