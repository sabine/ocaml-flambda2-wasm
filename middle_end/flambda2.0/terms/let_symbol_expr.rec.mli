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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The form of expression that binds symbols to statically-allocated
    constants. *)

module Bound_symbols : sig
  type t =
    | Singleton : Symbol.t -> t
      (** A binding of a single symbol of kind [Value]. *)
    | Code_and_set_of_closures of {
        code_ids : Code_id.Set.t;
        closure_symbols : Symbol.t Closure_id.Map.t;
      }
      (** A recursive binding of possibly multiple symbols to the individual
          closures within a set of closures; and/or bindings of code to
          code IDs. *)

  val being_defined : t -> Symbol.Set.t

  val code_being_defined : t -> Code_id.Set.t

  val closure_symbols_being_defined : t -> Symbol.Set.t

  val disjoint_union : t -> t -> t

  include Expr_std.S with type t := t
end

type t

val create : Bound_symbols.t -> Static_const.t -> Expr.t -> t

val bound_symbols : t -> Bound_symbols.t

val defining_expr : t -> Static_const.t

val body : t -> Expr.t

include Expr_std.S with type t := t

(** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
    version of [id2]. *)
val pieces_of_code
   : ?newer_versions_of:Code_id.t Code_id.Map.t
  -> ?set_of_closures:(Symbol.t Closure_id.Map.t * Set_of_closures.t)
  -> Function_params_and_body.t Code_id.Map.t
  -> Bound_symbols.t * Static_const.t
