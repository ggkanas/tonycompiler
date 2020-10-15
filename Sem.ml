open Ast
open Symbol
open Types
open Identifier
open Error


let getVariableType x =
    let p = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
        match p.entry_info with
        | ENTRY_variable vi -> Some(vi.variable_type)
        | ENTRY_parameter pi -> Some(pi.parameter_type)
        | _ -> None

let getFunctionType x =
    let p = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
        match p.entry_info with
        | ENTRY_function fi -> Some(fi.function_result)
        | _ -> None


let rec param_walk es params f lc =
    match es, params with
    | [], [] -> ()
    | [], _ -> raise (WrongNumberArgsError (1, lc))
    | _, [] -> raise (WrongNumberArgsError (2, lc))
    | (e, lc2)::es, p::params -> (
        match p.entry_info with
        | ENTRY_parameter pi -> (let pt = pi.parameter_type in f (e, lc2) pt;
          if pi.parameter_mode = PASS_BY_REFERENCE then
            (match e with
            | E_atom a -> (
                match a with
                | A_id _ -> ()
                | A_atom_el _ -> ()
                | _ -> raise (LValueError (2, lc2))
            )
            | _ -> raise (LValueError (2, lc2)))
            ;
          param_walk es params f lc2
        )
        | _ -> raise (InternalError lc)
    )




let rec type_check (e, lc) t =
    let rec match_arr_elem_type t acc =
        if acc > 0 then
        match t with
        | TY_array (ty, limit) -> match_arr_elem_type ty (acc - 1)
        | ty -> raise (TypeError (TY_array (t, 0), ty, lc))
        else t
    in
    let rec atom_el_walk (ae, (index, lc2), t, lc) acc =
        match ae with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> (
                let ty = match_arr_elem_type vt acc in
                if not (equalType t ty) then raise (TypeError (t, ty, lc))
                else type_check (index, lc2) TY_int
            )
            | None -> raise (WrongIdError (1, x, lc))
        )
        | A_string s -> (if not (equalType t TY_char) then raise (TypeError (t, TY_char, lc))
            else match index with
            | E_int_const n -> if n >= String.length s then raise (IndexBoundError lc)
            | _ -> type_check (index, lc2) TY_int
        )
        | A_atom_el (ae2, index2) -> atom_el_walk (ae2, index2, t, lc2) (acc + 1)
        | A_call _ -> raise (TypeError2 ((TY_array(t, 0)),  "a function call",  lc2))
    in
    let rec atom_el_walk2 (ae, t, lc) acc =
        match ae with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> (
                let ty = match_arr_elem_type vt acc in
                if not (equalType (TY_list t) ty) then raise (TypeError (t, ty, lc))
            )
            | None -> raise (WrongIdError(1, x, lc))
        )
        | A_atom_el (ae2, index2) -> atom_el_walk2 (ae2, t, lc) (acc + 1)
        | A_call _ -> raise (TypeError2((TY_array(t, 0)), "a function call", lc))
        | _ -> raise (TypeError2 (t, "char", lc))
    in
    let rec check_head (e, lc) t =
        try
        match e with
        | E_atom (A_id x) -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some (TY_list ty) -> if not(equalType t ty) then raise (TypeError (t, ty, lc)) else ty
            | Some ty -> raise (TypeError (t, ty, lc))
            | None -> raise (WrongIdError (1, x, lc))
        )
        | E_atom (A_atom_el (ae, index)) -> atom_el_walk2(ae, t, lc) 1; t
        | E_atom (A_call (x, _)) -> ( let ft_opt = getFunctionType x in
            match ft_opt with
            | Some (TY_list ty) -> if not(equalType t ty) then raise (TypeError (t, ty, lc)) else ty
            | Some ty -> raise (TypeError (t, ty, lc))
            | None -> raise (WrongIdError(2, x, lc))
        )
        | E_atom (A_string _) -> raise (TypeError2 (t, "string", lc))
        | E_unary_op (UP_tail, lst) -> check_head lst t
        | E_binary_op (head, BP_cons, tail) -> type_check head t; t
        | E_nil -> raise (NullPtrError lc)
        | E_int_const _ -> raise (TypeError2(TY_list t, "int", lc))
        | E_char_const _ -> raise (TypeError2(TY_list t, "char", lc))
        | E_bool_const _ -> raise (TypeError2(TY_list t, "bool", lc))
        | E_new _ -> raise (TypeError2(TY_list t, "new expression", lc))
        | _ -> raise (TypeError2(TY_list t, "non-list operation", lc))
        with Exit -> fatal2 "on line %d" lc; t
    in
    (*let rec get_type_e (e, lc) =
        match e with
        | E_int_const _ -> TY_int
        | E_char_const _ -> TY_char
        | E_bool_const _ -> TY_bool
        | E_nil -> TY_list(TY_any)
        | E_new (t, _) -> TY_array(t, 0)
        | E_unary_op (op, exp) -> (
            match op with
            | UP_nil -> TY_bool
            | UP_not -> TY_bool
            | UP_plus -> TY_int
            | UP_minus -> TY_int
            | UP_head -> check_head exp TY_any
            | UP_tail -> get_type_e exp
        )
        | E_binary_op (e1, op, e2) -> (
            match op with
            | BP_plus -> TY_int
            | BP_minus -> TY_int
            | BP_times -> TY_int
            | BP_div -> TY_int
            | BP_mod -> TY_int
            | BP_and -> TY_bool
            | BP_or -> TY_bool
            | BP_eq -> get_type_e e1
            | BP_uneq -> get_type_e e1
            | BP_leq -> get_type_e e1
            | BP_geq -> get_type_e e1
            | BP_lower -> get_type_e e1
            | BP_greater -> get_type_e e1
            | BP_cons -> TY_list(get_type_e e1)
        )
        | E_atom a -> (
            match a with
            | A_id x -> (let vt_opt = getVariableType x in
                match vt_opt with
                | Some vt -> vt
                | None -> raise (InternalError lc)
            )
            | A_string s -> TY_array(TY_char, 0)
            | A_atom_el (a, exp) -> get_type_e (E_atom a, lc)
            | A_call (x, _) -> (let ft_opt = getFunctionType x in
                match ft_opt with
                | Some ft -> ft
                | None -> raise (InternalError lc)
            )
        )
    in*)
    try
    match e with
    | E_int_const n -> if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc))
    | E_char_const c -> if not(equalType t TY_char) then raise (TypeError (t, TY_char, lc))
    | E_bool_const b -> if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
    | E_atom a -> (
        match a with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> if not(equalType t vt) then raise (TypeError (t, vt, lc))
            | None -> raise (WrongIdError(1, x, lc))
        )
        | A_string s -> (
            match t with
            | TY_array(TY_char, _) -> ()
            | _ -> raise (TypeError (t, TY_array(TY_char, String.length s), lc))
        )
        | A_atom_el (ae, index) -> atom_el_walk (ae, index, t, lc) 1
        | A_call (x, params) -> (let p = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> if not(equalType t fi.function_result) then raise (TypeError (t, fi.function_result, lc)) else
              param_walk params fi.function_paramlist type_check lc
            | _ -> raise (WrongIdError(2, x, lc))
        )
    )
    | E_unary_op (op, e) -> (
        match op with
        | UP_plus -> type_check e TY_int; if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc))
        | UP_minus -> type_check e TY_int; if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc))
        | UP_not -> type_check e TY_bool; if not(equalType t TY_bool) then raise (TypeError (t, TY_int, lc))
        | UP_nil -> type_check (e) (TY_list(TY_any)); if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        | UP_head -> type_check e (TY_list(check_head e t))
        | UP_tail -> type_check e (TY_list(TY_any)); if not(equalType t (TY_list(TY_any))) then raise (TypeError (t, TY_list(TY_any), lc))
    )
    | E_binary_op (e1, op, e2) -> (
        match op with
        | BP_plus -> type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc))
        | BP_minus -> type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc))
        | BP_times -> type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc))
        | BP_div -> (type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc)) else
            match e2 with
            | (E_int_const 0, lc2) -> raise (ZeroDivError lc2)
            | _ -> ()
        )
        | BP_mod -> ( type_check e1 TY_int; type_check e2 TY_int;
          if not(equalType t TY_int) then raise (TypeError (t, TY_int, lc)) else
            match e2 with
            | (E_int_const 0, lc2) -> raise (ZeroDivError lc2)
            | _ -> ()
        )
        | BP_eq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2, lc) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2, lc))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        )
        | BP_uneq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2, lc) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2, lc))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        )
        | BP_geq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2, lc) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2, lc))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        )
        | BP_leq -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2, lc) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2, lc))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        )
        | BP_lower -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2, lc) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2, lc))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        )
        | BP_greater -> (
            let ty =
            try
                type_check e1 TY_int; TY_int
            with TypeError (t1, t2, lc) -> (
                if t2 = TY_char then TY_char else
                if t2 = TY_bool then TY_bool else
                raise (TypeError3("int/char/bool", t2, lc))
            )
            in type_check e2 ty;
            if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        )
        | BP_and -> type_check e1 TY_bool; type_check e2 TY_bool;
          if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        | BP_or -> type_check e1 TY_bool; type_check e2 TY_bool;
          if not(equalType t TY_bool) then raise (TypeError (t, TY_bool, lc))
        | BP_cons ->
            match t with
            | TY_list(head_type) -> type_check e1 head_type; type_check e2 (TY_list(head_type))
            | _ -> raise (InternalError lc)
    )
    | E_nil -> if not(equalType t (TY_list(TY_any))) then raise (TypeError (t, TY_list(TY_any), lc))
    | E_new (ty, e) -> type_check e TY_int;
        match t with
        | TY_array(ty2, _) -> if not (equalType ty2 ty) then raise (TypeError (ty2, ty, lc))
        | _ -> raise (TypeError2 (t, "new expression", lc))
    with Exit -> fatal2 "on line %d" lc

let sem_param p rf t id = ignore (newParameter (id_make id) t rf p true)

let sem_formal p lc formal =
    match formal with
    | (rf, (D_var_def (t, ids), lc2)) -> (
        try
        List.iter (sem_param p rf t) ids
        with Exit -> fatal2 "on line %d" lc
    )
    | _ -> raise (InternalError lc)

let sem_header (t, id, formals) def lc = let p = newFunction (id_make id) true in
    openScope();
    let fs = if def then formals else List.rev formals in
    List.iter (sem_formal p lc) fs;
    if def then endFunctionHeader p t else forwardFunction p t

let sem_id t id = ignore (newVariable (id_make id) t true)

let rec atom_el_walk3 arr lc acc =
    let rec match_arr_elem_type t acc =
        if acc > 0 then
        match t with
        | TY_array (ty, limit) -> match_arr_elem_type ty (acc - 1)
        | ty -> raise (TypeError3 ("array", ty, lc))
        else t
    in
    match arr with
    | A_id x -> (let vt_opt = getVariableType x in
        match vt_opt with
        | Some vt -> (
            match_arr_elem_type vt acc
        )
        | None -> raise (WrongIdError (1, x, lc))
    )
    | A_atom_el (arr2, _) -> atom_el_walk3 arr2 lc (acc + 1)
    | _ -> raise (LValueError (1, lc))


let get_type atom lc =
    match atom with
    | A_id x -> (
        match getVariableType x with
        | Some vt -> vt
        | None -> raise (WrongIdError(1, x, lc))
    )
    | A_atom_el (arr, (index, lc2)) -> (let tupl = atom_el_walk3 arr lc 1 in
        match tupl with
        | t -> (type_check (index, lc2) TY_int
            (*
            match index with
            | E_int_const n -> () (*if n >= lim then raise IndexBoundError*)
            | _ -> raise (IndexTypeError lc2)*)
        ); t
    )
    | _ -> raise (LValueError (1, lc))

let rec sem_stmt id (stmt, lc) =
    try
    match stmt with
    | ST_simple simple -> (
        match simple with
        | S_skip -> ()
        | S_assign (atom, e) -> type_check e (get_type atom lc)
        | S_call (id, es) -> let p = lookupEntry (id_make id) LOOKUP_ALL_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi ->
                if fi.function_result <> TY_proc then (raise (IgnoredResultError lc))
                else param_walk es fi.function_paramlist type_check lc
            | _ -> raise (WrongIdError(2, id, lc))
    )
    | ST_exit -> (
        let ft_opt = getFunctionType id in
        match ft_opt with
        | Some ft -> if ft <> TY_proc then raise (ExitError lc)
        | None -> raise (InternalError lc)
    )
    | ST_return result -> (
        match !currentScope.sco_parent with
        | Some sco -> (
            match (List.hd sco.sco_entries).entry_info with
            | ENTRY_function fi -> let t = fi.function_result in
                type_check result t
            | _ -> raise (InternalError lc)
        )
        | None -> raise (InternalError lc)
    )
    | ST_if (cond, then_stmts, elsifs, else_stmts) ->
        let sem_elsif (cond, stmts) = type_check cond TY_bool; List.iter (sem_stmt id) stmts
        in
        type_check cond TY_bool;
        List.iter (sem_stmt id) then_stmts;
        List.iter sem_elsif elsifs;
        List.iter (sem_stmt id) else_stmts
    | ST_for (inits, cond, incrs, stmts) -> (
        let sem_init lc init =
            match init with
            | S_assign _ -> sem_stmt id (ST_simple init, lc)
            | S_skip -> ()
            | _ -> raise (ForError (1, lc))
        in
        let sem_incr lc incr =
            match incr with
            | S_assign _ -> sem_stmt id (ST_simple incr, lc)
            | S_skip -> ()
            | _ -> raise (ForError (2, lc))
        in
        List.iter (sem_init lc) inits;
        type_check cond TY_bool;
        List.iter (sem_incr lc) incrs;
        List.iter (sem_stmt id) stmts
    )
    with Exit -> fatal2 "on line %d" lc

let rec sem_defdecl (d, lc) =
    try
    match d with
    | D_func_def (header, defdecls, stmts) -> sem_header header true lc;
                                              (*openScope();*)
                                              List.iter sem_defdecl defdecls;
                                              let (_, id, _) = header in
                                              List.iter (sem_stmt id) stmts;
                                              closeScope()
    | D_func_decl header -> sem_header header false lc; closeScope()
    | D_var_def (t, ids) -> List.iter (sem_id t) ids
    with Exit -> fatal2 "on line %d" lc

let sem (d, lc) =
        (
        match d with
        | D_func_def ((t, id, formals), _, _) ->
            if not (equalType TY_proc t) then raise (TypeError3("none", t, lc))
            else let l = List.length formals in if l <> 0 then raise (MainParamError(lc))
        | _ -> raise (InternalError lc))
        ;
        sem_defdecl (d, lc)

let sem_init () =
    let add_func id params ret_t =
        let f = newFunction (id_make id) true in
        openScope();
        let rec add_params f params =
            match params with
            | [] -> ()
            | (pid, t)::ps -> ignore (newParameter (id_make pid) t PASS_BY_VALUE f true); add_params f ps
        in
        add_params f params;
        endFunctionHeader f ret_t;
        closeScope()
    in
    add_func "puti" [("n", TY_int)] TY_proc;
    add_func "putb" [("b", TY_bool)] TY_proc;
    add_func "putc" [("c", TY_char)] TY_proc;
    add_func "puts" [("s", TY_array(TY_char, 0))] TY_proc;

    add_func "geti" [] TY_int;
    add_func "getb" [] TY_bool;
    add_func "getc" [] TY_char;
    add_func "gets" [("n", TY_int); ("s", TY_array(TY_char, 0))] TY_proc;

    add_func "abs" [("n", TY_int)] TY_int;
    add_func "ord" [("c", TY_char)] TY_int;
    add_func "chr" [("n", TY_int)] TY_char;

    add_func "strlen" [("s", TY_array(TY_char, 0))] TY_int;
    add_func "strcmp" [("s1", TY_array(TY_char, 0)); ("s2", TY_array(TY_char, 0))] TY_int;
    add_func "strcpy" [("trg", TY_array(TY_char, 0)); ("src", TY_array(TY_char, 0))] TY_proc;
    add_func "strcat" [("trg", TY_array(TY_char, 0)); ("src", TY_array(TY_char, 0))] TY_proc
