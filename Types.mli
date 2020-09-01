 type typ = TY_none         (* no type (should not be used)             *)
          | TY_int          (* int                                      *)
          | TY_char         (* byte                                     *)
          | TY_bool         (* bool                                     *)
          | TY_array of     (* array                                    *)
            typ *           (*   element type                           *)
            int             (*   size of array, if known, or zero       *)
          | TY_list of typ  (* list, of element type                    *)
          | TY_proc         (* proc (return type)                       *)
          | TY_string       (* A string                                 *)
          | TY_any          (*Any type, used for abstract comparisons   *)

val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
