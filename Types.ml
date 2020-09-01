type typ = TY_none
         | TY_int
         | TY_char
         | TY_bool
         | TY_array of typ * int
         | TY_list of typ
         | TY_proc
         | TY_string
         | TY_any

let rec sizeOfType t =
   match t with
   | TY_int            -> 2
   | TY_char           -> 1
   | TY_bool           -> 1
   | TY_array (et, sz) -> sz * sizeOfType et
   | TY_list et        -> 2 + sizeOfType et
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TY_array (et1, sz1), TY_array (et2, sz2) -> equalType et1 et2
   | _                                            -> t1 = TY_any || t2 = TY_any || t1 = t2
