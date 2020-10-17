open Format
open Lexing

exception Terminate
exception TypeError of Types.typ * Types.typ * int
exception IndexBoundError of int
exception IndexTypeError of int
exception NullPtrError of int
exception ZeroDivError of int
exception InternalError of int
exception ExitError of int
exception LValueError of int * int
exception IgnoredResultError of int
exception WrongNumberArgsError of int * int
exception ForError of int * int
exception WrongIdError of int * string * int
exception TypeError2 of Types.typ * string * int
exception TypeError3 of string * Types.typ * int
exception MainParamError of int
exception NoReturnError of int

type verbose = Vquiet | Vnormal | Vverbose

let flagVerbose = ref Vnormal

let numErrors = ref 0
let maxErrors = ref 10
let flagWarnings = ref true
let numWarnings = ref 0
let maxWarnings = ref 200

type position =
    PosPoint   of Lexing.position
  | PosContext of Lexing.position * Lexing.position
  | PosDummy

let position_point lpos = PosPoint lpos
let position_context lpos_start lpos_end = PosContext (lpos_start, lpos_end)
let position_dummy = PosDummy

let print_position ppf pos =
  match pos with
  | PosPoint lpos ->
      fprintf ppf "@[file \"%s\",@ line %d,@ character %d:@]@ "
        lpos.pos_fname lpos.pos_lnum (lpos.pos_cnum - lpos.pos_bol)
  | PosContext (lpos_start, lpos_end) ->
      if lpos_start.pos_fname != lpos_end.pos_fname then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     file %s,@ line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_fname lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_lnum != lpos_end.pos_lnum then
        fprintf ppf "@[file \"%s\",@ line %d,@ character %d to@ \
                     line %d,@ character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          lpos_end.pos_lnum
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else if lpos_start.pos_cnum - lpos_start.pos_bol !=
              lpos_end.pos_cnum - lpos_end.pos_bol then
        fprintf ppf "@[file \"%s\",@ line %d,@ characters %d to %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
          (lpos_end.pos_cnum - lpos_end.pos_bol)
      else
        fprintf ppf "@[file \"%s\", line %d, character %d:@]@ "
          lpos_start.pos_fname lpos_start.pos_lnum
          (lpos_start.pos_cnum - lpos_start.pos_bol)
  | PosDummy ->
      ()

let no_out buf pos len = ()
let no_flush () = ()
let null_formatter = make_formatter no_out no_flush

let internal_raw (fname, lnum) fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  eprintf "Internal error occurred at %s:%d,@ " fname lnum;
  kfprintf cont err_formatter fmt

and fatal fmt =
  let fmt = "@[<v 2>Fatal error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  kfprintf cont err_formatter fmt

and fatal2 fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  let cont ppf =
    raise Terminate in
  kfprintf cont err_formatter fmt


and error fmt =
  let fmt = "@[<v 2>Error: " ^^ fmt ^^ "@]@;@?" in
  incr numErrors;
  if !numErrors >= !maxErrors then
    let cont ppf =
      eprintf "Too many errors, aborting...\n";
      raise Terminate in
    kfprintf cont err_formatter fmt
  else
    eprintf fmt

and warning fmt =
  let fmt = "@[<v 2>Warning: " ^^ fmt ^^ "@]@;@?" in
  if !flagWarnings then
  begin
    incr numWarnings;
    if !numWarnings >= !maxWarnings then
      let cont ppf =
        eprintf "Too many warnings, no more will be shown...\n";
        flagWarnings := false in
      kfprintf cont err_formatter fmt
    else
      eprintf fmt
  end
  else
    fprintf null_formatter fmt

and message fmt =
  let fmt = "@[<v 2>" ^^ fmt ^^ "@]@;@?" in
  eprintf fmt
