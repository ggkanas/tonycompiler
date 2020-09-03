open Ast
open Symbol
open Types
open Identifier

exception TypeError of Types.typ * Types.typ
exception IndexBoundError
exception IndexTypeError
exception NullPtrError
exception ZeroDivError
exception InternalError
exception ExitError
exception LValueError of int
exception IgnoredResultError
exception WrongNumberArgsError of int
exception ForError of int
exception WrongIdError of int * string
exception TypeError2 of Types.typ * string
exception TypeError3 of string * Types.typ


let getVariableType x =
    let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
        match p.entry_info with
        | ENTRY_variable vi -> Some(vi.variable_type)
        | _ -> None

let getFunctionType x =
    let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
        match p.entry_info with
        | ENTRY_function fi -> Some(fi.function_result)
        | _ -> None


let rec param_walk es params f =
    match es, params with
    | [], [] -> ()
    | [], _ -> raise (WrongNumberArgsError 1)
    | _, [] -> raise (WrongNumberArgsError 2)
    | e::es, p::params -> (
        match p.entry_info with
        | ENTRY_parameter pi -> (let pt = pi.parameter_type in f e pt;
          if pi.parameter_mode = PASS_BY_REFERENCE then
            match e with
            | E_atom a -> (
                match a with
                | A_id _ -> ()
                | A_atom_el _ -> ()
                | _ -> raise (LValueError 2)
            )
            | _ -> if true then raise (LValueError 2)
            ;
          param_walk es params f
        )
        | _ -> raise (WrongIdError (3, (id_name p.entry_id)))
    )


let rec type_check e t =
    let rec atom_el_walk (ae, index, t) =
        match ae with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> (
                match vt with
                | TY_array (ty, limit) -> (if not (equalType t ty) then raise (TypeError (t, ty))
                else match index with
                    | E_int_const n -> () (*if n >= limit then raise IndexBoundError*)
                    | _ -> raise IndexTypeError
                )
                | ty -> raise (TypeError (TY_array (t, 0), ty))
            )
            | None -> raise (WrongIdError (1, x))
        )
        | A_string s -> (if not (equalType t TY_char) then raise (TypeError (t, TY_char))
            else match index with
            | E_int_const n -> if n >= String.length s then raise IndexBoundError
            | _ -> raise IndexTypeError
        )
        | A_atom_el (ae2, index2) -> atom_el_walk (ae2, index2, t)
        | A_call (x, params) -> (let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> (
                match fi.function_result with
                | TY_array (ty, limit) -> if not(equalType t ty) then raise (TypeError (t, ty))
                | ty -> if true then raise (TypeError (t, ty)) (*Why does it give warning when no if?*)
                ;
                param_walk params fi.function_paramlist type_check
            )
            | _ -> raise (WrongIdError (2, x))
        )
    in
    let rec atom_el_walk2 (ae, t) =
        match ae with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> (
                match vt with
                | TY_array (TY_list ty, _) -> if not(equalType t ty) then raise (TypeError (t, ty))
                | ty -> raise (TypeError (TY_array (t, 0), ty))
            )
            | None -> raise (WrongIdError(1, x))
        )
        | A_atom_el (ae2, index2) -> atom_el_walk2 (ae2, t)
        | A_call (x, params) -> ( let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> (
                match fi.function_result with
                | TY_array (TY_list ty, limit) -> if not(equalType t ty) then raise (TypeError (t, ty))
                | ty -> raise (TypeError (TY_array (t, 0), ty))
            )
            | _ -> raise (WrongIdError (2, x))
        )
        | _ -> raise (TypeError2 (t, "char"))
    in
    let rec check_head e t =
        match e with
        | E_atom (A_id x) -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some (TY_list ty) -> if not(equalType t ty) then raise (TypeError (t, ty)) else ty
            | Some ty -> raise (TypeError (t, ty))
            | None -> raise (WrongIdError (1, x))
        )
        | E_atom (A_atom_el (ae, index)) -> atom_el_walk2(ae, t); t
        | E_atom (A_call (x, _)) -> ( let ft_opt = getFunctionType x in
            match ft_opt with
            | Some (TY_list ty) -> if not(equalType t ty) then raise (TypeError (t, ty)) else ty
            | Some ty -> raise (TypeError (t, ty))
            | None -> raise (WrongIdError(2, x))
        )
        | E_atom (A_string _) -> raise (TypeError2 (t, "string"))
        | E_unary_op (UP_tail, lst) -> check_head lst t
        | E_binary_op (head, BP_cons, tail) -> type_check head t; t
        | E_nil -> raise NullPtrError
        | E_int_const _ -> raise (TypeError2(TY_list t, "int"))
        | E_char_const _ -> raise (TypeError2(TY_list t, "char"))
        | E_bool_const _ -> raise (TypeError2(TY_list t, "bool"))
        | E_new _ -> raise (TypeError2(TY_list t, "new expression"))
        | _ -> raise (TypeError2(TY_list t, "non-list operation"))
    in
    match e with
    | E_int_const n -> if not(equalType t TY_int) then raise (TypeError (t, TY_int))
    | E_char_const c -> if not(equalType t TY_char) then raise (TypeError (t, TY_char))
    | E_bool_const b -> if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
    | E_atom a -> (
        match a with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> if not(equalType t vt) then raise (TypeError (t, vt))
            | None -> raise (WrongIdError(1, x))
        )
        | A_string s -> (
            match t with
            | TY_array(TY_char, _) -> ()
            | _ -> raise (TypeError (t, TY_array(TY_char, String.length s)))
        )
        | A_atom_el (ae, index) -> atom_el_walk (ae, index, t)
        | A_call (x, params) -> (let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> if not(equalType t fi.function_result) then raise (TypeError (t, fi.function_result)) else
              param_walk params fi.function_paramlist type_check
            | _ -> raise (WrongIdError(2, x))
        )
    )
    | E_unary_op (op, e) -> (
        match op with
        | UP_plus -> type_check e TY_int; if not(equalType t TY_int) then raise (TypeError (t, TY_int))
        | UP_minus -> type_check e TY_int; if not(equalType t TY_int) then raise (TypeError (t, TY_int))
        | UP_not -> type_check e TY_bool; if not(equalType t TY_bool) then raise (TypeError (t, TY_int))
        | UP_nil -> type_check (e) (TY_list(TY_any)); if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        | UP_head -> type_check e (TY_list(check_head e t))
        | UP_tail -> type_check e (TY_list(TY_any)); if not(equalType t (TY_list(TY_any))) then raise (TypeError (t, TY_list(TY_any)))
    )
    | E_binary_op (e1, op, e2) -> (
        match op with
        | BP_plus -> type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int))
        | BP_minus -> type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int))
        | BP_times -> type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int))
        | BP_div -> (type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int)) else
            match e2 with
            | E_int_const 0 -> raise ZeroDivError
            | _ -> ()
        )
        | BP_mod -> ( type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int)) else
            match e2 with
            | E_int_const 0 -> raise ZeroDivError
            | _ -> ()
        )
        | BP_eq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        )
        | BP_uneq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        )
        | BP_geq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        )
        | BP_leq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        )
        | BP_lower -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        )
        | BP_greater -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        )
        | BP_and -> type_check e1 TY_bool; type_check e2 TY_bool;
          if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        | BP_or -> type_check e1 TY_bool; type_check e2 TY_bool;
          if not(equalType t TY_bool) then raise (TypeError (t, TY_bool))
        | BP_cons ->  type_check e2 (TY_list(TY_any)); let head_type = check_head e1 t in
            if not(equalType t (TY_list(head_type))) then raise (TypeError (t, TY_list(head_type)))
    )
    | E_nil -> if not(equalType t (TY_list(TY_any))) then raise (TypeError (t, TY_list(TY_any)))
    | E_new (ty, e) -> type_check e TY_int;
        match t with
        | TY_array(ty2, _) -> if not (equalType ty2 ty) then raise (TypeError (ty2, ty))
        | _ -> raise (TypeError2 (t, "new expression"))

let sem_param p rf t id = ignore (newParameter (id_make id) t rf p true)

let sem_formal p formal =
    match formal with
    | (rf, D_var_def (t, ids)) -> List.iter (sem_param p rf t) ids
    | _ -> raise InternalError

let sem_header (t, id, formals) def = let p = newFunction (id_make id) true in
    openScope();
    List.iter (sem_formal p) formals;
    if def then endFunctionHeader p t else forwardFunction p

let sem_id t id = ignore (newVariable (id_make id) t true)

let rec atom_el_walk3 arr =
    match arr with
    | A_id x -> (let vt_opt = getVariableType x in
        match vt_opt with
        | Some vt -> (
            match vt with
            | TY_array (ty, limit) -> ty (* (ty, limit) *)
            | ty -> raise (TypeError3 ("array", ty))
        )
        | None -> raise (WrongIdError (1, x))
    )
    | A_atom_el (arr2, _) -> atom_el_walk3 arr2
    | _ -> raise (LValueError 1)


let get_type atom =
    match atom with
    | A_id x -> (
        match getVariableType x with
        | Some vt -> vt
        | None -> raise (WrongIdError(1, x))
    )
    | A_atom_el (arr, index) -> (let tupl = atom_el_walk3 arr in
        match tupl with
        | t -> (
            match index with
            | E_int_const n -> () (*if n >= lim then raise IndexBoundError*)
            | _ -> raise IndexTypeError
        ); t
    )
    | _ -> raise (LValueError 1)

let rec sem_stmt stmt =
    match stmt with
    | ST_simple simple -> (
        match simple with
        | S_skip -> ()
        | S_assign (atom, e) -> type_check e (get_type atom)
        | S_call (id, es) -> let p = lookupEntry (id_make id) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi ->
                if fi.function_result <> TY_proc then raise IgnoredResultError
                else param_walk es fi.function_paramlist type_check
            | _ -> raise (WrongIdError(2, id))
    )
    | ST_exit -> (
        match !currentScope.sco_parent with
        | Some sco -> (
            match (List.hd sco.sco_entries).entry_info with
            | ENTRY_function fi -> if fi.function_result <> TY_proc then raise ExitError
            | _ -> raise InternalError
        )
        | None -> raise InternalError
    )
    | ST_return result -> (
        match !currentScope.sco_parent with
        | Some sco -> (
            match (List.hd sco.sco_entries).entry_info with
            | ENTRY_function fi -> let t = fi.function_result in
                type_check result t
            | _ -> raise InternalError
        )
        | None -> raise InternalError
    )
    | ST_if (cond, then_stmts, elsifs, else_stmts) ->
        let sem_elsif (cond, stmts) = type_check cond TY_bool; List.iter sem_stmt stmts
        in
        type_check cond TY_bool;
        List.iter sem_stmt then_stmts;
        List.iter sem_elsif elsifs;
        List.iter sem_stmt else_stmts
    | ST_for (inits, cond, incrs, stmts) -> (
        let sem_init init =
            match init with
            | S_assign _ -> sem_stmt (ST_simple init)
            | S_skip -> ()
            | _ -> raise (ForError 1)
        in
        let sem_incr incr =
            match incr with
            | S_assign _ -> sem_stmt (ST_simple incr)
            | S_skip -> ()
            | _ -> raise (ForError 2)
        in
        List.iter sem_init inits;
        type_check cond TY_bool;
        List.iter sem_incr incrs;
        List.iter sem_stmt stmts
    )

let rec sem d =
    match d with
    | D_func_def (header, defdecls, stmts) -> sem_header header true;
                                              (*openScope();*)
                                              List.iter sem defdecls;
                                              List.iter sem_stmt stmts;
                                              closeScope()
    | D_func_decl header -> sem_header header false; closeScope()
    | D_var_def (t, ids) -> List.iter (sem_id t) ids
