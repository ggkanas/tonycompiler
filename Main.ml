open Sem
open Error
open Types

let main =
  let inchannel = if Array.length Sys.argv < 2 then stdin
  else open_in (Sys.argv.(1)) in
  let lexbuf = Lexing.from_channel inchannel in
  try
    let ast = Parser.program Lexer.lexer lexbuf in
        Symbol.initSymbolTable 1024;
        Sem.sem ast
  with Error.Terminate ->
    exit 1
  | Parsing.Parse_error ->
    error "syntax error on line %d" !Lexer.linecount;
    exit 1
  | Exit -> exit 1
  | TypeError (t1, t2) ->
    error "expression is of incorrect type.\nExpected type %s, but got %s" (toString t1) (toString t2)
  | NullPtrError -> error "trying to access null pointer"
  | ZeroDivError -> error "division by zero"
  | InternalError -> internal "error has occured during semantic analysis"
  | ExitError -> error "trying to exit from function with a result"
  | LValueError 1 -> error "assignment to non-Lvalue"
  | LValueError 2 -> error "non-Lvalue argument given when pass mode is pass-by-reference"
  | IgnoredResultError -> error "the result of this function is ignored"
  | WrongNumberArgsError 1 -> error "too few arguments were given"
  | WrongNumberArgsError 2 -> error "too many arguments were given"
  | ForError 1 -> error "for inits should be either skip or assignments"
  | ForError 2 -> error "for increments shoud be either skip or assignments"
  | WrongIdError (1,x) -> error "%s is not a variable" x
  | WrongIdError(2, x) -> error "%s is not a function" x
  | WrongIdError(3, x) -> error "%s is not an array" x
  | IndexBoundError -> error "index out of bounds"
  | IndexTypeError -> error "index type must be integer"
  | TypeError2 (t, s) -> error "expression is of incorrect type.\nExpected type %s, but got %s" (toString t) s
  | TypeError3 (s, t) -> error "expression is of incorrect type.\nExpected type %s, but got %s" s (toString t)
