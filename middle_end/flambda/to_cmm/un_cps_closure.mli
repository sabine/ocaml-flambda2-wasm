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

(* Compute offsets for elements in sets of closures *)

val compute_offsets : Exported_offsets.t -> Flambda_unit.t -> Exported_offsets.t
(** Compute offsets for a whole compilation unit.
    Takes the offsets from all cmx files read as input. *)

val closure_name : Closure_id.t -> string
(** Returns a cmm name for a closure id. *)

val closure_code : string -> string
(** Returns the address for a function code from the global name of
    a closure. *)

(* val map_on_function_decl :
 *   (string -> Closure_id.t -> Flambda.Function_declaration.t -> 'a) ->
 *   Flambda_unit.t -> 'a Closure_id.Map.t
 * (\** Map a function on each function body exactly once, and return the
 *     resulting mapping. *\) *)


type layout_slot =
  | Env_var of Var_within_closure.t
  | Infix_header
  | Closure of Closure_id.t (**)
(** Layout slots, aka what might be found in a block at a given offset.
    A layout slot can take up more than one word of memory (this is the case
    for closures, which can take either 2 or 3 words depending on arity). *)

type layout = (int * layout_slot) list
(** Alias for complete layouts. The list is sorted according to offsets
    (in increasing order). *)

val layout :
  Exported_offsets.t -> Closure_id.t list -> Var_within_closure.t list -> layout
(** Order the given closure ids and env vars into a list of layout slots
    together with their respective offset. Note that there may be holes
    between the offsets. *)

val print_layout : Format.formatter -> layout -> unit
val print_layout_slot : Format.formatter -> layout_slot -> unit
(** Printing functions for layout slots and layouts. *)

