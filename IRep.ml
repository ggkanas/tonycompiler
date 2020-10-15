open Llvm
open Ast
open Types
open Error
open Symbol
open Identifier
open String

type llvm_info = {
  context          : llcontext;
  the_module       : llmodule;
  builder          : llbuilder;
  i1               : lltype;
  i8               : lltype;
  i16              : lltype;
  i32              : lltype;
  i64              : lltype;
  c1               : int -> llvalue;
  c8               : int -> llvalue;
  c16              : int -> llvalue;
  c32              : int -> llvalue;
  c64              : int -> llvalue;
}




let arr_ind_type t lc =
    match t with
    | TY_array (ty, n) -> ty
    | _ -> internal  "Reference to index type, but not array. At progam line %d" lc; t

let rec tolltype info t lc =
    match t with
    | TY_none -> internal "Reference to TY_none at progam line %d" lc; info.i8
    | TY_int -> info.i16
    | TY_char -> info.i8
    | TY_bool -> info.i1
    | TY_array (ty, n) -> pointer_type((tolltype info ty lc))
    | TY_list ty -> pointer_type (struct_type  info.context [|(tolltype info ty lc); pointer_type (info.i64)|])
    | TY_proc -> (*void_type info.context*) info.i8
    | TY_any -> internal "TY_any conversion to lltype at progam line %d" lc; info.i8

let rec compile_expr info (e, lc) =
    try
    let rec compile_atom info a inds lc =
        match a with
        | A_id x -> (let en = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
            match en.entry_info with
            | ENTRY_llvalue l->
                let ptr = build_gep l.llvalue (Array.append [| |] inds) (x ^ "_ptr") info.builder in
                if l.llvalue_pmode = PASS_BY_REFERENCE then build_load ptr (x ^ "_val") info.builder else ptr
            | _ -> internal "on program line %d" lc; raise (InternalError lc)
        )
        | A_atom_el (a2, e) -> let ptr = compile_atom info a2 (Array.append [| info.c32(0)|] inds) lc in
            let arrptr = build_load ptr "arrptr" info.builder in
            let i = (compile_expr info e) in
            build_gep arrptr [| i |] "arrelemptr" info.builder
        | _ -> internal "on program line %d" lc; raise (InternalError lc)
    in
    let rec compile_arr info a inds lc =
        match a with
        | A_id x -> (let en = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
            match en.entry_info with
            | ENTRY_llvalue l-> let v = build_gep l.llvalue (Array.append [| |] inds) (x ^ "_ptr") info.builder in
                let va = build_load v (x ^ "_tmp") info.builder in
                if l.llvalue_pmode = PASS_BY_REFERENCE then build_load va (x ^ "_val") info.builder else va
            | _ -> internal "on program line %d" lc; raise (InternalError lc)
        )
        | A_atom_el (a2, e) ->
            let arrptr = compile_arr info a2 (Array.append [| |] inds) lc in
            let i = (compile_expr info e) in
            let arrelemptr = build_gep arrptr [| i |] "arrelemptr" info.builder in
            let arrelem = build_load arrelemptr "arrelem" info.builder in arrelem

        | A_string s -> build_global_stringptr s ("str" ^ s) info.builder
        | _ -> internal "on program line %d" lc; raise (InternalError lc)
    in
    match e with
    | E_atom a -> (
        match a with
        | A_id x -> (let en = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
            match en.entry_info with
            | ENTRY_llvalue l -> let v = build_gep l.llvalue [| info.c32(0) |] (x ^ "_ptr") info.builder in
                let va = build_load v (x ^ "_tmp") info.builder in
                if l.llvalue_pmode = PASS_BY_REFERENCE then build_load va (x ^ "_val") info.builder else va
            | _ -> internal "on program line %d" lc; raise (InternalError lc)
        )
        | A_string s -> build_global_stringptr s ("str" ^ s) info.builder
        | A_atom_el (a, e) -> compile_arr info (A_atom_el (a, e)) [| |] lc
        | A_call (id, args) -> (
            let param_aux info f ind arg =
                if Array.length (function_attrs f (AttrIndex.Param(ind))) > 0 then
                (match arg with
                | (E_atom a, _) -> compile_atom info a [| |] lc
                | _ -> internal "on program line %d" lc; raise (InternalError lc))
                else compile_expr info arg
            in
            let newid = if id = "main" then "main$" else id in
            let fn = lookup_function newid info.the_module in
            match fn with
            | Some f -> let argvals = Array.of_list (List.mapi (param_aux info f) args) in
            build_call f argvals (newid ^ "_res") info.builder
            | None -> internal "on program line %d" lc; raise (InternalError lc)
        )
    )

    | E_int_const n -> info.c16 n
    | E_char_const c -> info.c8 (Char.code c)
    | E_bool_const b -> info.c1 (if b then 1 else 0)
    | E_unary_op (op, e) -> (
        let v = compile_expr info e in
        match op with
        | UP_plus -> v
        | UP_minus -> build_neg v ((value_name v) ^ "_neg") info.builder
        | UP_not -> build_not v ((value_name v) ^ "_not") info.builder
        | UP_nil -> (*let h = build_gep v [|info.c32(0); info.c32(1)  |] ((value_name v) ^ "_tail_ptr") info.builder in
            let tail = build_load h ((value_name v) ^ "_tail") info.builder in*)
            build_is_null v "isnil" info.builder
        | UP_head ->
            let h = build_gep v [|info.c32(0); info.c32(0)  |] ((value_name v) ^ "_head_ptr") info.builder in
            build_load h ((value_name v) ^ "_head") info.builder
        | UP_tail ->
            let h = build_gep v [|info.c32(0); info.c32(1)  |] ((value_name v) ^ "_tail_ptr") info.builder in
            let tail_int = build_load h ((value_name v) ^ "_tail_int") info.builder in
            let tail = build_intcast tail_int (type_of v) ((value_name v) ^ "_tail") info.builder in
            (*build_load tail ((value_name v) ^ "_tail_tmp") info.builder*) tail
    )
    | E_binary_op (e1, op, e2) -> (
        let v1 = compile_expr info e1 in
        let v2 = compile_expr info e2 in
        match op with
        | BP_plus -> build_add v1 v2 "addtmp" info.builder
        | BP_minus -> build_sub v1 v2 "subtmp" info.builder
        | BP_times -> build_mul v1 v2 "multmp" info.builder
        | BP_div -> build_sdiv v1 v2 "divtmp" info.builder
        | BP_mod -> build_srem v1 v2 "modtmp" info.builder
        | BP_eq -> build_icmp Icmp.Eq v1 v2 "eqtmp" info.builder
        | BP_uneq -> build_icmp Icmp.Ne v1 v2 "neqtmp" info.builder
        | BP_geq -> build_icmp Icmp.Sge v1 v2 "geqtmp" info.builder
        | BP_leq -> build_icmp Icmp.Sle v1 v2 "leqtmp" info.builder
        | BP_lower -> build_icmp Icmp.Slt v1 v2 "lowertmp" info.builder
        | BP_greater -> build_icmp Icmp.Sgt v1 v2 "greatertmp" info.builder
        | BP_and -> build_and v1 v2 "andtmp" info.builder
        | BP_or -> build_or v1 v2 "ortmp" info.builder
        | BP_cons -> let hd = build_malloc (element_type (type_of v2)) "constmp" info.builder in
            (*let hdstruct = build_load hd "conshd" info.builder in*)
            let hdptr = build_struct_gep hd 0 "conshd" info.builder in
            ignore (build_store v1 hdptr info.builder);
            let tlptr = build_struct_gep hd 1 "constlptr" info.builder in
            (*let v2tl = build_load v2 "consv2" info.builder in*)
            let v2cast = build_intcast v2  (pointer_type info.i64) "consv2cast" info.builder in
            ignore (build_store v2cast tlptr info.builder); hd
    )
    | E_nil -> const_null (pointer_type (struct_type info.context [|info.i16 ; pointer_type info.i64 |]))
    | E_new (t, e) ->
        let arr = build_array_malloc (tolltype info t lc)  (compile_expr info e) "newarr" info.builder in
        arr
    with Exit -> fatal2 "on line %d" lc; raise Exit


let rec compile_stmt info (stmt, lc) =
    try
    let rec compile_atom info a inds lc =
        match a with
        | A_id x -> (let en = lookupEntry (id_make x) LOOKUP_ALL_SCOPES true in
            match en.entry_info with
            | ENTRY_llvalue l->
                if l.llvalue_pmode = PASS_BY_REFERENCE then
                let ptr = build_gep l.llvalue (Array.append [| |] inds) (x ^ "_ptr") info.builder in
                build_load ptr (x ^ "_val") info.builder else l.llvalue
            | _ -> internal "on program line %d" lc; raise (InternalError lc)
        )
        | A_atom_el (a2, e) -> let ptr = compile_atom info a2 (Array.append [| info.c32(0)|] inds) lc in
            let arrptr = build_load ptr "arrptr" info.builder in
            let i = (compile_expr info e) in
            build_gep arrptr [| i |] "arrelemptr" info.builder
        | _ -> internal "on program line %d" lc; raise (InternalError lc)
    in
    match stmt with
    | ST_simple s -> (
        match s with
        | S_skip -> ()
        | S_assign (a, e) -> let lhs = compile_atom info a [| |] lc in
            let rhs =
            (match e with
            | (E_nil, lc) -> let t = (struct_element_types (element_type(element_type (type_of lhs)))).(0) in
             const_null (pointer_type (struct_type info.context
                [| t; pointer_type info.i64 |]))
            | _ -> compile_expr info e
            ) in
            (*let castrhs = build_intcast rhs (type_of lhs) ((value_name rhs) ^ "cast") info.builder in*)
            ignore(build_store rhs lhs info.builder)
        | S_call (id, args) -> (
            let param_aux info f ind arg =
                if Array.length (function_attrs f (AttrIndex.Param(ind))) > 0 then
                (match arg with
                | (E_atom a, _) -> compile_atom info a [| |] lc
                | _ -> internal "on program line %d" lc; raise (InternalError lc))
                else compile_expr info arg
            in
            let newid = if id = "main" then "main$" else id in
            let fn = lookup_function newid info.the_module in
            match fn with
            | Some f -> let argvals = Array.of_list (List.mapi (param_aux info f) args) in
            ignore(build_call f argvals (newid ^ "_res") info.builder)
            | None -> internal "on program line %d" lc
    )
    )
    | ST_exit -> ignore (build_ret(*_void*) (const_int info.i8 0) info.builder)
    | ST_return e -> let v = compile_expr info e in ignore(build_ret v info.builder)
    | ST_if (cond, then_stmts, elsifs, else_stmts) ->
        let elsif_block info f name ind elsif =
            append_block info.context (name ^ (string_of_int ind) ^ "_") f
        in
        let terminate_block info next_bb stmts =
            if (List.length stmts) > 0 then
            (match List.hd (List.rev stmts) with
                | (ST_exit, _) -> true
                | (ST_return _, _) -> true
                | _ -> (ignore (build_br next_bb info.builder); false)
            )
            else  (ignore (build_br next_bb info.builder); false)
        in

        let rec compile_elsifs info else_bb after_bb elsifthen_bbs elsif_bbs elsifs =
            match elsifthen_bbs, elsif_bbs, elsifs with
            | [], [], [] -> true
            | _, _, [] -> internal "on program line %d" lc; raise (InternalError lc)
            | [thenbb], [], [(cond, stmts)] -> let condv = compile_expr info cond in
                ignore (build_cond_br condv thenbb else_bb info.builder);
                position_at_end thenbb info.builder;
                List.iter (compile_stmt info) stmts;
                terminate_block info after_bb stmts
            | _, [], _ -> internal "on program line %d" lc; raise (InternalError lc)
            | [], _, _ -> internal "on program line %d" lc; raise (InternalError lc)
            | thenbb::thens, bb::bbs, (cond, stmts)::elsifs -> let condv = compile_expr info cond in
                ignore (build_cond_br condv thenbb bb info.builder);
                position_at_end thenbb info.builder;
                List.iter (compile_stmt info) stmts;
                let b = terminate_block info after_bb stmts in
                position_at_end bb info.builder;
                b && (compile_elsifs info else_bb after_bb thens bbs elsifs)
        in
        let condv = compile_expr info cond in
        let bb = insertion_block info.builder in
        let f = block_parent bb in
        let then_bb = append_block info.context "then" f in
        let elsif_bbs = List.mapi (elsif_block info f "elsif") elsifs in
        let elsifthen_bbs = List.mapi (elsif_block info f "elsifthen") elsifs in
        let else_bb = append_block info.context "else" f in
        let after_bb = append_block info.context "endif" f in
        let next_bb = if (List.length elsifs) > 0 then List.hd elsif_bbs
        else if (List.length else_stmts) > 0 then else_bb else after_bb in
        let bbs = if (List.length elsifs) > 0 then (List.tl elsif_bbs) else [] in
        ignore (build_cond_br condv then_bb next_bb info.builder);
        position_at_end then_bb info.builder;
        List.iter (compile_stmt info) then_stmts;
        let b1 = terminate_block info after_bb then_stmts in
        position_at_end next_bb info.builder;
        let b2 = compile_elsifs info else_bb after_bb elsifthen_bbs bbs elsifs in
        position_at_end else_bb info.builder;
        List.iter (compile_stmt info) else_stmts;
        let b3 = terminate_block info after_bb else_stmts in
        if b1 && b2 && b3 then (remove_block after_bb) else position_at_end after_bb info.builder
    | ST_for (inits, cond, incrs, stmts) ->
        let compile_simple info s = compile_stmt info (ST_simple s, lc) in
        let bb = insertion_block info.builder in
        let f = block_parent bb in
        let loop_bb = append_block info.context "loop" f in
        let body_bb = append_block info.context "body" f in
        let after_bb = append_block info.context "endfor" f in
        List.iter (compile_simple info) inits;
        ignore (build_br loop_bb info.builder);
        position_at_end loop_bb info.builder;
        (*let phi_iter = build_phi [(n, bb)] "iter" info.builder in*)
        let loop_cond = compile_expr info cond in
        ignore (build_cond_br loop_cond body_bb after_bb info.builder);
        position_at_end body_bb info.builder;
        (*let remaining = build_sub phi_iter (info.c64 1)
                                     "remaining" info.builder in*)
        List.iter (compile_stmt info) stmts;
        List.iter (compile_simple info) incrs;
        (*add_incoming
        (remaining, insertion_block info.builder) phi_iter;*)
        ignore (build_br loop_bb info.builder);
        position_at_end after_bb info.builder
    with Exit -> fatal2 "on line %d" lc

let compile_id info t lc id = let v = build_alloca (tolltype info t lc) id info.builder in
    ignore(newLlvalue (id_make id) v false PASS_BY_VALUE true)

let compile_param info ty pmode lc id =
    let t = tolltype info ty lc in
    if pmode = PASS_BY_REFERENCE then pointer_type t else t

let compile_formal info t (pmode, (d, lc)) =
    match d with
    | D_var_def(ty, ids) -> List.map (compile_param info ty pmode lc) ids
    | _ -> internal "on program line %d" lc; raise (InternalError lc)

let formal_ids_aux pmode t id = (pmode, t, id)

let formal_ids (pmode, (d, lc)) =
    match d with
    | D_var_def(t, ids) -> List.map (formal_ids_aux pmode t) ids
    | _ -> internal "on program line %d" lc; raise (InternalError lc)

let compile_param2 info lc f ind (param, (pmode, t, id)) =
    let ty = if pmode = PASS_BY_REFERENCE then pointer_type (tolltype info t lc) else (tolltype info t lc) in
    if pmode = PASS_BY_REFERENCE then add_function_attr f (create_enum_attr info.context "dereferenceable" (Int64.of_int (sizeOfType t))) (AttrIndex.Param(ind));
    let ptr = build_alloca ty id info.builder in
    ignore(build_store param ptr info.builder);
    ignore(newLlvalue (id_make id) ptr true pmode true)
    (*ignore(newLlparam (id_make id) ind f true)*)

let tupleof a b = (a, b)


let compile_params info f formals lc =
    let params = Array.to_list (params f) in
    let typeids = List.concat (List.map formal_ids formals) in
    let lists = List.map2 tupleof params typeids in
    List.iteri (compile_param2 info lc f) lists

let compile_header info (t, id, formals) def lc =
    let params = Array.of_list (List.concat (List.map (compile_formal info t) formals)) in
    let f_type = function_type (tolltype info t lc) params in
    let f_opt = lookup_function id info.the_module in
    (match f_opt with
    | Some f -> (let bb = append_block info.context ("entry") f in
        position_at_end bb info.builder; compile_params info f formals lc)
    | None -> let newid = if id = "main" then "main$" else id in
        let f = (declare_function newid f_type info.the_module) in
        if def then (let bb = append_block info.context "entry" f in
        position_at_end bb info.builder; compile_params info f formals lc))

let compile_var info (d, lc) =
    match d with
    | D_var_def (t, ids) -> List.iter (compile_id info t lc) ids
    | _ -> ()

let rec compile_func info (d, lc) =
    match d with
    | D_func_def (header, defdecls, stmts) -> List.iter (compile_func info) defdecls;
                                              openScope();
                                              compile_header info header true lc;
                                              List.iter (compile_var info) defdecls;
                                              List.iter (compile_stmt info) stmts;
                                              let (t, _, _) = header in
                                              (match List.hd (List.rev stmts) with
                                              | (ST_exit, _) -> ()
                                              | _ -> if t = TY_proc then ignore(build_ret (info.c8 0) info.builder));
                                              closeScope()
    | D_func_decl header -> openScope(); compile_header info header false lc; closeScope()
    | _ -> ()


let rec compile info (d, lc) =
    match d with
    | D_func_def (header, defdecls, stmts) -> compile_func info (d, lc);
                                              (* Define and start and main function *)
                                              let main_type = function_type info.i32 [| |] in
                                              let main = declare_function "main" main_type info.the_module in
                                              let bb = append_block info.context "entry" main in
                                              position_at_end bb info.builder;
                                              let (_, id, _) = header in
                                              let newid = if id = "main" then "main$" else id in
                                              let f_opt = lookup_function newid info.the_module in
                                              (match f_opt with
                                              | Some f -> ignore(build_call f [||] (id ^ "_res") info.builder)
                                              | None -> internal "on program line %d" lc; raise (InternalError lc));
                                              ignore (build_ret (info.c32 0) info.builder)
    | _ -> internal "on program line %d" lc; raise (InternalError lc)

let llvm_compile_and_dump ast opt imm final name =

  (* Initialize *)
  Llvm_all_backends.initialize ();
  let context = global_context () in
  let the_module = create_module context name in
  let builder = builder context in
  let pm = PassManager.create () in
  List.iter (fun f -> f pm) [
    Llvm_scalar_opts.add_memory_to_register_promotion;
    Llvm_scalar_opts.add_instruction_combination;
    Llvm_scalar_opts.add_reassociation;
    Llvm_scalar_opts.add_gvn;
    Llvm_scalar_opts.add_cfg_simplification;
  ];
  (* Initialize types *)
  let i1 = i1_type context in
  let i8 = i8_type context in
  let i16 = i16_type context in
  let i32 = i32_type context in
  let i64 = i64_type context in
  (* Initialize constant functions *)
  let c1 = const_int i1 in
  let c8 = const_int i8 in
  let c16 = const_int i16 in
  let c32 = const_int i32 in
  let c64 = const_int i64 in


  let info = {
    context          = context;
    the_module       = the_module;
    builder          = builder;
    i1               = i1;
    i8               = i8;
    i16              = i16;
    i32              = i32;
    i64              = i64;
    c1               = c1;
    c8               = c8;
    c16              = c16;
    c32              = c32;
    c64              = c64;
  } in
  (* Initialize library functions *)
  let puti_type =
    (*function_type (void_type context) [| i16 |] in*)
    function_type (i8) [| i16 |] in
  let the_puti =
    declare_function "puti" puti_type the_module in
  let putb_type =
    (*function_type (void_type context) [| i1 |] in*)
    function_type (i8) [| i1 |] in
  let the_putb =
    declare_function "putb" putb_type the_module in
  let putc_type =
  (*function_type (void_type context) [| i8 |] in*)
  function_type (i8) [| i8 |] in
  let the_putc =
    declare_function "putc" putc_type the_module in
  let puts_type =
  (*function_type (void_type context) [| tolltype info (TY_array (TY_char, 0)) 0 |] in*)
  function_type (i8) [| tolltype info (TY_array (TY_char, 0)) 0 |] in
  let the_puts =
    declare_function "puts" puts_type the_module in

  let geti_type =
    function_type i16 [| |] in
  let the_geti =
    declare_function "geti" geti_type the_module in
  let getb_type =
    function_type i1 [| |] in
  let the_getb =
    declare_function "getb" getb_type the_module in
  let getc_type =
    function_type i8 [| |] in
  let the_getc =
    declare_function "getc" getc_type the_module in
  let gets_type =
    (*function_type (void_type context) [| i16; tolltype info (TY_array (TY_char, 0)) 0 |] in*)
    function_type (i8) [| i16; tolltype info (TY_array (TY_char, 0)) 0 |] in
  let the_gets =
    declare_function "gets" gets_type the_module in

  let abs_type = function_type i16 [| i16 |] in
  let the_abs = declare_function "abs" abs_type the_module in
  let ord_type = function_type i16 [| i8 |] in
  let the_ord = declare_function "ord" ord_type the_module in
  let chr_type = function_type i8 [| i16 |] in
  let the_chr = declare_function "chr" chr_type the_module in

  let strlen_type = function_type i16 [| tolltype info (TY_array (TY_char, 0)) 0 |] in
  let the_strlen = declare_function "strlen" strlen_type the_module in
  let strcmp_type =
    function_type i16 [| tolltype info (TY_array (TY_char, 0)) 0; tolltype info (TY_array (TY_char, 0)) 0 |] in
  let the_strcmp = declare_function "strcmp" strcmp_type the_module in
  let strcpy_type =
    (*function_type (void_type context) [| tolltype info (TY_array (TY_char, 0)) 0; tolltype info (TY_array (TY_char, 0)) 0 |] in*)
    function_type (i8) [| tolltype info (TY_array (TY_char, 0)) 0; tolltype info (TY_array (TY_char, 0)) 0 |] in
  let the_strcpy = declare_function "strcpy" strcpy_type the_module in
  let strcat_type =
    (*function_type (void_type context) [| tolltype info (TY_array (TY_char, 0)) 0; tolltype info (TY_array (TY_char, 0)) 0 |] in*)
    function_type (i8) [| tolltype info (TY_array (TY_char, 0)) 0; tolltype info (TY_array (TY_char, 0)) 0 |] in
  let the_strcat = declare_function "strcat" strcat_type the_module in
  ignore(the_puti);
  ignore(the_putb);
  ignore(the_putc);
  ignore(the_puts);
  ignore(the_geti);
  ignore(the_getb);
  ignore(the_getc);
  ignore(the_gets);
  ignore(the_abs);
  ignore(the_ord);
  ignore(the_chr);
  ignore(the_strlen);
  ignore(the_strcat);
  ignore(the_strcpy);
  ignore(the_strcmp);

  (* Emit the program code *)
  compile info ast;
  (* Verify *)
  Llvm_analysis.assert_valid_module the_module;
  (* Optimize *)
  if opt then ignore (PassManager.run_module the_module pm);
  (* Print out the IR *)
  if (imm || (not (final))) then print_module (name ^ ".ll") the_module;
  if (final || (not (imm))) then
  ignore(Llvm_bitwriter.write_bitcode_file the_module (name ^ ".bc"))
