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

(** The Flambda representation of a single compilation unit's code. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

(** Perform well-formedness checks on the unit. *)
val invariant : t -> unit

(** Print a unit to a formatter. *)
val print : Format.formatter -> t -> unit

(** Create a unit. *)
val create
   : imported_symbols:Flambda_kind.t Symbol.Map.t
  -> root_symbol:Symbol.t
  -> return_continuation:Continuation.t
  -> exn_continuation:Continuation.t
  -> body:Expr.t
  -> t

(** All closure variables used in the given unit. *)
val used_closure_vars : t -> Var_within_closure.Set.t

(** All symbols imported from other compilation units by the given unit. *)
val imported_symbols : t -> Flambda_kind.t Symbol.Map.t

(** The module block symbol for the given unit (the only symbol that can never
    be eliminated). *)
val root_symbol : t -> Symbol.t

val body : t -> Expr.t
