open Sem
open Error
open Types

let main =
  let inchannel = if Array.length Sys.argv < 2 then stdin
  else open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel inchannel in
  try
  try
    let ast = Parser.program Lexer.lexer lexbuf in
        Symbol.initSymbolTable 1024;
        Sem.sem_init();
        Sem.sem ast
  with Parsing.Parse_error ->
    error "syntax error on line %d" !Lexer.linecount;
    exit 1
  | Exit -> exit 1
  | TypeError (t1, t2, lc) ->
    error "on line %d: expression is of incorrect type.\nExpected type %s, but got %s"
    lc (toString t1) (toString t2)
  | NullPtrError lc -> error "on line %d: trying to access nil" lc
  | ZeroDivError lc -> error "on line %d: division by zero" lc
  | InternalError lc -> internal "on line %d: error has occured during semantic analysis" lc
  | ExitError lc -> error "on line %d: trying to exit from function with a result" lc
  | LValueError (1, lc) -> error "on line %d: assignment to non-Lvalue" lc
  | LValueError (2, lc) -> error "on line %d: non-Lvalue argument given when pass mode is pass-by-reference" lc
  | IgnoredResultError lc -> error "on line %d: the result of this function is ignored" lc
  | WrongNumberArgsError (1, lc) -> error "on line %d: too few arguments were given" lc
  | WrongNumberArgsError (2, lc) -> error "on line %d: too many arguments were given" lc
  | ForError (1, lc) -> error "on line %d: for inits should be either skip or assignments" lc
  | ForError (2, lc) -> error "on line %d: for increments shoud be either skip or assignments" lc
  | WrongIdError (1,x, lc) -> error "on line %d: %s is not a variable" lc x
  | WrongIdError(2, x, lc) -> error "on line %d: %s is not a function" lc x
  | IndexBoundError lc -> error "on line %d: index out of bounds" lc
  | IndexTypeError lc -> error "on line %d: index type must be integer" lc
  | TypeError2 (t, s, lc) ->
    error "on line %d: expression is of incorrect type.\nExpected type %s, but got %s" lc (toString t) s
  | TypeError3 (s, t, lc) ->
    error "on line %d: expression is of incorrect type.\nExpected type %s, but got %s" lc s (toString t)
  | MainParamError(lc) -> error "on line %d: main function must have no parameters" lc
  with Error.Terminate ->
    exit 1
