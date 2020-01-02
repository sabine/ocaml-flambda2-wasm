(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

type t = {
  init : Cmm.expression;
  current_data : Cmm.data_item list;
  other_data : Cmm.data_item list list;
  gc_roots : Symbol.t list;
  functions : Cmm.phrase list;
}

let empty = {
  init = C.void;
  current_data = [];
  other_data = [];
  gc_roots = [];
  functions = [];
}

let add_if_not_empty x l =
  match x with
  | [] -> l
  | _ :: _ -> x :: l

(* CR mshinwell: Label the arguments so the evaluation order is clear *)
let combine r t = {
  init = C.sequence r.init t.init;
  current_data = [];
  other_data =
    add_if_not_empty r.current_data (
      add_if_not_empty t.current_data (
        (r.other_data @ t.other_data)));
  gc_roots = r.gc_roots @ t.gc_roots;
  functions = r.functions @ t.functions;
}

let archive_data r =
  { r with current_data = [];
           other_data = add_if_not_empty r.current_data r.other_data; }

let wrap_init f r =
  { r with init = f r.init; }

let add_data d r =
  { r with current_data = d @ r.current_data; }

let update_data f r =
  { r with current_data = f r.current_data; }

let add_gc_roots l r =
  { r with gc_roots = l @ r.gc_roots; }

let add_function f r =
  { r with functions = f :: r.functions; }

let to_cmm r =
  let entry =
    let dbg = Debuginfo.none in
    let fun_name = Compilenv.make_symbol (Some "entry") in
    let fun_codegen =
      if Config.flambda then
        [ Cmm.Reduce_code_size;
          Cmm.No_CSE ]
      else
        [ Cmm.Reduce_code_size ]
    in
    let init = C.sequence r.init (C.unit ~dbg) in
    C.cfunction (C.fundecl fun_name [] init fun_codegen dbg)
  in
  let data_list = add_if_not_empty r.current_data r.other_data in
  let data = List.map C.cdata data_list in
  data, entry, r.gc_roots, r.functions
