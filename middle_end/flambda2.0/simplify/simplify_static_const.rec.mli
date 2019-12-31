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

(** Simplification of statically-allocated constants bound to symbols. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_set_of_closures0
    : Downwards_acc.t
   -> Flambda.Set_of_closures.t
   -> closure_symbols:Symbol.t Closure_id.Map.t
   -> closure_elements:Simple.t Var_within_closure.Map.t
   -> closure_element_types:Flambda_type.t Var_within_closure.Map.t
   -> Flambda.Set_of_closures.t
        * Downwards_acc.t
        * Flambda_type.t Symbol.Map.t
        * Flambda.Let_symbol_expr.Bound_symbols.t
        * Static_const.t

val simplify_static_const
   : Downwards_acc.t
  -> Flambda.Let_symbol_expr.Bound_symbols.t
  -> Static_const.t
  -> Static_const.t * Downwards_acc.t
