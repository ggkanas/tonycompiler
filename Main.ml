open Sem
open Error
open Types
open IRep
open Cmdliner

let rec split str =
    match str with
    | [] -> ""
    | c::s -> if c <> '.' then (String.make 1 c) ^ (split s) else ""

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


let tony opt imm final src =
  let inchannel = if imm || final then stdin
  else open_in src in
  let lexbuf = Lexing.from_channel inchannel in
  try
  try
    let ast = Parser.program Lexer.lexer lexbuf in
        Symbol.initSymbolTable 1024;
        Sem.sem_init();
        Sem.sem ast;
        Symbol.clearSymbolTable();
        Symbol.initSymbolTable 1024;
        llvm_compile_and_dump ast opt imm final src
  with Parsing.Parse_error ->
    error "syntax error on line %d" !LC.linecount;
    exit 1
  | Exit -> exit 1
  | TypeError (t1, t2, lc) ->
    error "on line %d: expression is of incorrect type.\nExpected type %s, but got %s"
    lc (toString t1) (toString t2)
  | NullPtrError lc -> error "on line %d: trying to access nil" lc
  | ZeroDivError lc -> error "on line %d: division by zero" lc
  | InternalError lc -> internal "on line %d: internal error has occured during semantic analysis" lc
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


let opt =
  let doc = "Add optimisation passes." in
  Arg.(value & flag & info ["O"; "optimise"] ~doc)

let imm =
  let doc = "Generate intermediate representation." in
  Arg.(value & flag & info ["i"; "intermediate"] ~doc)

let final =
  let doc = "Generate assembly code." in
  Arg.(value & flag & info ["f"; "final"] ~doc)

let src =
  let doc = "Source file to be compiled." in
  Arg.(value & pos 0 string "a.tony" & info [] ~docv:"SRC"~doc)

let tony_t = Term.(const tony $ opt $ imm $ final $ src)

let info =
  let doc = "Tony compiler" in
  let man = [
    ]
  in
  Term.info "tony" ~doc ~man

let () = Term.exit  @@ Term.eval (tony_t, info)
