(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Language terms that represent statically-allocated values. *)

module Field_of_block : sig
  (** Inhabitants (of kind [Value]) of fields of statically-allocated blocks. *)
  type t =
    | Symbol of Symbol.t
      (** The address of the given symbol. *)
    | Tagged_immediate of Immediate.t
      (** The given tagged immediate. *)
    | Dynamically_computed of Variable.t
      (** The value of the given variable. *)

  (** Printing, total ordering, etc. *)
  include Identifiable.S with type t := t
end

type 'a or_variable =
  | Const of 'a
  | Var of Variable.t

(** The mutability status of a block field. *)
type mutable_or_immutable = Mutable | Immutable

(** A piece of code, comprising of the parameters and body of a function,
    together with a field indicating whether the piece of code is a newer
    version of one that existed previously (and may still exist), for
    example after a round of simplification. *)
type code = {
  params_and_body : Function_params_and_body.t or_deleted;
  newer_version_of : Code_id.t option;
}
and 'a or_deleted =
  | Present of 'a
  | Deleted

val print_code : Format.formatter -> code -> unit

(** The possibly-recursive declaration of pieces of code and any associated set
    of closures. *)
type code_and_set_of_closures = {
  code : code Code_id.Map.t;
  (* CR mshinwell: Check the free names of the set of closures *)
  set_of_closures : Set_of_closures.t option;
}

(** The static structure of a symbol, possibly with holes, ready to be filled
    with values computed at runtime. *)
type t =
  | Block of Tag.Scannable.t * mutable_or_immutable * (Field_of_block.t list)
  | Code_and_set_of_closures of code_and_set_of_closures
  | Boxed_float of Numbers.Float_by_bit_pattern.t or_variable
  | Boxed_int32 of Int32.t or_variable
  | Boxed_int64 of Int64.t or_variable
  | Boxed_nativeint of Targetint.t or_variable
  | Immutable_float_array of Numbers.Float_by_bit_pattern.t or_variable list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

include Identifiable.S with type t := t
include Contains_names.S with type t := t

val get_pieces_of_code
   : t
  -> (Function_params_and_body.t * (Code_id.t option)) Code_id.Map.t

val is_fully_static : t -> bool

val disjoint_union : t -> t -> t

val can_share : t -> bool
