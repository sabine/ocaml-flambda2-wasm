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

(** Simplification of the right-hand sides of [Let] bindings. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type simplify_named_result = private {
  bindings_outermost_first : (Bindable_let_bound.t * Reachable.t) list;
  dacc : Downwards_acc.t;
}

val simplify_named
   : Downwards_acc.t
  -> bound_vars:Bindable_let_bound.t
  -> Flambda.Named.t
  -> simplify_named_result

(** The following are only for the use of [Simplify_static]. *)

type simplify_set_of_closures0_result = private {
  set_of_closures : Flambda.Set_of_closures.t;
  closure_types_by_bound_name : Flambda_type.t Name_in_binding_pos.Map.t;
  newer_versions_of : Code_id.t Code_id.Map.t;
  code : Flambda.Function_params_and_body.t Code_id.Map.t;
  dacc : Downwards_acc.t;
}

val simplify_set_of_closures0
   : Downwards_acc.t
  -> Flambda.Set_of_closures.t
  -> closure_bound_names:Name_in_binding_pos.t Closure_id.Map.t
  -> closure_elements:Simple.t Var_within_closure.Map.t
  -> closure_element_types:Flambda_type.t Var_within_closure.Map.t
  -> old_to_new_code_ids_all_sets:Code_id.t Code_id.Map.t
  -> simplify_set_of_closures0_result

type lifting_decision_result = private {
  can_lift : bool;
  closure_elements : Simple.t Var_within_closure.Map.t;
  closure_element_types : Flambda_type.t Var_within_closure.Map.t;
}

val type_closure_elements_for_previously_lifted_set
   : Downwards_acc.t
  -> min_name_mode:Name_mode.t
  -> Flambda.Set_of_closures.t
  -> lifting_decision_result
