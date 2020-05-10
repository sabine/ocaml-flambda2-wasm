(* 5888 = block length 5, color 11, tag 0 
 * Note: caml_obj_dup is called to duplicate the array, so that
 * the value accessible via the symbol 'arr' lives on the heap.
 * 
 * It seems that this is done because an array is always a mutable
 * data structure.
 *)
let arr = [|1;2;3;4;5|]
