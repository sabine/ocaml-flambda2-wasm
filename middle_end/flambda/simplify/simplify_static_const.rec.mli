(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Simplification of statically-allocated constants bound to symbols. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_static_const
   : Downwards_acc.t
  -> Flambda.Let_symbol_expr.Bound_symbols.t
  -> Static_const.t
  -> Flambda.Let_symbol_expr.Bound_symbols.t * Static_const.t * Downwards_acc.t
