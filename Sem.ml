open Ast
open Symbol
open Types
open Identifier

exception TypeError
exception IndexBoundError
exception IndexTypeError
exception NullPtrError
exception ZeroDivError
exception InternalError
exception TooFewArgumentsError
exception TooManyArgumentsError

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









let rec type_check e t =
    let rec param_walk ps pes = (
        match ps, pes with
        | [], [] -> ()
        | [], _ -> raise TooFewArgumentsError
        | _, [] -> raise TooManyArgumentsError
        | p::params, pe::param_entries -> (
            match pe.entry_info with
            | ENTRY_parameter pi -> let pt = pi.parameter_type in type_check p pt;
              param_walk params param_entries
            | _ -> raise InternalError
        )
    )
    in
    let rec atom_el_walk (ae, index, t) =
        match ae with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> (
                match vt with
                | TY_array (ty, limit) -> (if t <> ty then raise TypeError
                else match index with
                    | E_int_const n -> if n >= limit then raise IndexBoundError
                    | _ -> raise IndexTypeError
                )
                | _ -> raise TypeError
            )
            | None -> raise TypeError
        )
        | A_string s -> (if t <> TY_char then raise TypeError
            else match index with
            | E_int_const n -> if n >= String.length s then raise IndexBoundError
            | _ -> raise IndexTypeError
        )
        | A_atom_el (ae2, index2) -> atom_el_walk (ae2, index2, t)
        | A_call (x, params) -> (let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> (
                match fi.function_result with
                | TY_array (ty, limit) -> if t <> ty then raise TypeError
                | _ -> raise TypeError
                ;
                param_walk params fi.function_paramlist
            )
            | _ -> raise TypeError
        )
    in
    let rec atom_el_walk2 (ae, t) =
        match ae with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> (
                match vt with
                | TY_array (TY_list ty, _) -> if t <> ty then raise TypeError
                | _ -> raise TypeError
            )
            | None -> raise TypeError
        )
        | A_atom_el (ae2, index2) -> atom_el_walk2 (ae2, t)
        | A_call (x, params) -> ( let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> (
                match fi.function_result with
                | TY_array (TY_list ty, limit) -> if t <> ty then raise TypeError
                | _ -> raise TypeError
            )
            | _ -> raise TypeError
        )
        | _ -> raise TypeError
    in
    let rec check_head e t =
        match e with
        | E_atom (A_id x) -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some (TY_list ty) -> if t <> ty then raise TypeError else ty
            | _ -> raise InternalError
        )
        | E_atom (A_atom_el (ae, index)) -> atom_el_walk2(ae, t); t
        | E_atom (A_call (x, _)) -> ( let vt_opt = getVariableType x in
            match vt_opt with
            | Some (TY_list ty) -> if t <> ty then raise TypeError else ty
            | _ -> raise InternalError
        )
        | E_unary_op (UP_tail, lst) -> check_head lst t
        | E_binary_op (head, BP_cons, tail) -> type_check head t; t
        | E_nil -> raise NullPtrError
        | _ -> raise InternalError
    in
    match e with
    | E_int_const n -> if t <> TY_int then raise TypeError
    | E_char_const c -> if t <> TY_char then raise TypeError
    | E_bool_const b -> if t <> TY_bool then raise TypeError
    | E_atom a -> (
        match a with
        | A_id x -> (let vt_opt = getVariableType x in
            match vt_opt with
            | Some vt -> if t <> vt then raise TypeError
            | None -> raise TypeError
        )
        | A_string s -> (
            match t with
            | TY_array(TY_char, _) -> ()
            | _ -> raise TypeError
        )
        | A_atom_el (ae, index) -> atom_el_walk (ae, index, t)
        | A_call (x, params) -> (let p = lookupEntry (id_make x) LOOKUP_ANCESTOR_SCOPES true in
            match p.entry_info with
            | ENTRY_function fi -> if t <> fi.function_result then raise TypeError else
              param_walk params fi.function_paramlist
            | _ -> raise TypeError
        )
    )
    | E_unary_op (op, e) -> (
        match op with
        | UP_plus -> type_check e TY_int; if t <> TY_int then raise TypeError
        | UP_minus -> type_check e TY_int; if t <> TY_int then raise TypeError
        | UP_not -> type_check e TY_bool; if t <> TY_bool then raise TypeError
        | UP_nil -> type_check (e) (TY_list(Types.typ)); if t <> TY_bool then raise TypeError
        | UP_head -> type_check e TY_list; check_head e t
        | UP_tail -> type_check e TY_list; if t <> TY_list then raise TypeError
    )
    | E_binary_op (e1, op, e2) -> (
        match op with
        | BP_plus -> type_check e1 TY_int; type_check e2 TY_int;
          if t <> TY_int then raise TypeError
        | BP_minus -> type_check e1 TY_int; type_check e2 TY_int;
          if t <> TY_int then raise TypeError
        | BP_times -> type_check e1 TY_int; type_check e2 TY_int;
          if t <> TY_int then raise TypeError
        | BP_div -> type_check e1 TY_int; type_check e2 TY_int;
          if t <> TY_int then raise TypeError else
            match e2 with
            | E_int_const 0 -> raise ZeroDivError
            | _ -> raise InternalError
        | BP_mod -> type_check e1 TY_int; type_check e2 TY_int;
          if t <> TY_int then raise TypeError else
            match e2 with
            | E_int_const 0 -> raise ZeroDivError
            | _ -> raise InternalError
        | BP_eq ->
            try
                type_check e1 TY_int;
                type_check e2 TY_int
            with TypeError -> try
                type_check e1 TY_char;
                type_check e2 TY_char
            with TypeError -> (
                type_check e1 TY_bool;
                type_check e2 TY_bool
            );
            if t <> TY_bool then raise TypeError
        | BP_uneq ->
            try
                type_check e1 TY_int;
                type_check e2 TY_int
            with TypeError -> try
                type_check e1 TY_char;
                type_check e2 TY_char
            with TypeError -> (
                type_check e1 TY_bool;
                type_check e2 TY_bool
            );
            if t <> TY_bool then raise TypeError
        | BP_geq ->
            try
                type_check e1 TY_int;
                type_check e2 TY_int
            with TypeError -> try
                type_check e1 TY_char;
                type_check e2 TY_char
            with TypeError -> (
                type_check e1 TY_bool;
                type_check e2 TY_bool
            );
            if t <> TY_bool then raise TypeError
        | BP_leq ->
            try
                type_check e1 TY_int;
                type_check e2 TY_int
            with TypeError -> try
                type_check e1 TY_char;
                type_check e2 TY_char
            with TypeError -> (
                type_check e1 TY_bool;
                type_check e2 TY_bool
            );
            if t <> TY_bool then raise TypeError
        | BP_lower ->
            try
                type_check e1 TY_int;
                type_check e2 TY_int
            with TypeError -> try
                type_check e1 TY_char;
                type_check e2 TY_char
            with TypeError -> (
                type_check e1 TY_bool;
                type_check e2 TY_bool
            );
            if t <> TY_bool then raise TypeError
        | BP_greater ->
            try
                type_check e1 TY_int;
                type_check e2 TY_int
            with TypeError -> try
                type_check e1 TY_char;
                type_check e2 TY_char
            with TypeError -> (
                type_check e1 TY_bool;
                type_check e2 TY_bool
            );
            if t <> TY_bool then raise TypeError
        | BP_and -> type_check e1 TY_bool; type_check e2 TY_bool;
          if t <> TY_bool then raise TypeError
        | BP_or -> type_check e1 TY_bool; type_check e2 TY_bool;
          if t <> TY_bool then raise TypeError
        | BP_cons -> type_check e2 TY_list; let head_type = check_head e1 t in
            if t <> TY_list(head_type) then raise TypeError
    )
    | E_nil -> if t <> TY_list then raise TypeError
    | E_new (ty, e) -> type_check e TY_int;
        match t with
        | TY_array(ty, _) -> ()
        | _ -> raise TypeError
