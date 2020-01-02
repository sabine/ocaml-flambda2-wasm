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

open! Simplify_import

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  let env = DE.typing_env (DA.denv dacc) in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Reachable.invalid (), TEE.empty (), dacc
  | Ok env_extension ->
    Reachable.reachable original_term, env_extension, dacc

type cse =
  | Invalid of T.t
  | Applied of (Reachable.t * TEE.t * DA.t)
  | Not_applied of DA.t

let apply_cse dacc ~original_prim ~min_name_mode =
  match P.Eligible_for_cse.create original_prim with
  | None -> None
  | Some with_fixed_value ->
    let typing_env = DE.typing_env (DA.denv dacc) in
    match TE.find_cse typing_env with_fixed_value with
    | None ->
      None
    | Some simple ->
      match TE.get_canonical_simple typing_env ~min_name_mode simple with
      | Bottom | Ok None -> None
      | Ok (Some simple) ->
        Some simple

let try_cse dacc ~original_prim ~result_kind ~min_name_mode
      ~result_var : cse =
  (* CR mshinwell: Use [meet] and [reify] for CSE?  (discuss with lwhite) *)
  match apply_cse dacc ~original_prim ~min_name_mode with
  | Some replace_with ->
    let named = Named.create_simple replace_with in
    let ty = T.alias_type_of result_kind replace_with in
    let env_extension = TEE.one_equation (Name.var result_var) ty in
    Applied (Reachable.reachable named, env_extension, dacc)
  | None ->
    let dacc =
      match P.Eligible_for_cse.create original_prim with
      | None -> dacc
      | Some eligible_prim ->
        let bound_to = Simple.var result_var in
        DA.map_denv dacc ~f:(fun denv ->
          DE.with_typing_env denv
           (TE.add_cse (DE.typing_env denv) eligible_prim ~bound_to))
    in
    Not_applied dacc

let add_wrapper_for_fixed_arity_continuation0 uacc cont ~use_id arity =
  let uenv = UA.uenv uacc in
  let original_cont = cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  match UE.find_apply_cont_rewrite uenv original_cont with
  | None -> None
  | Some rewrite when Apply_cont_rewrite.does_nothing rewrite ->
    let arity_in_rewrite = Apply_cont_rewrite.original_params_arity rewrite in
    if not (Flambda_arity.equal arity arity_in_rewrite) then begin
      Misc.fatal_errorf "Arity %a provided to fixed-arity-wrapper \
          addition function does not match arity %a in rewrite:@ %a"
        Flambda_arity.print arity
        Flambda_arity.print arity_in_rewrite
        Apply_cont_rewrite.print rewrite
    end;
    None
  | Some rewrite ->
    let params = List.map (fun _kind -> Variable.create "param") arity in
    let kinded_params =
      List.map2 (fun param kind -> KP.create (Parameter.wrap param) kind)
        params arity
    in
    let args = List.map (fun param -> Simple.var param) params in
    let apply_cont_expr, _apply_cont, _extra_args =
      Apply_cont_rewrite.rewrite_use rewrite use_id
        (Apply_cont.create cont ~args ~dbg:Debuginfo.none)
    in
    let new_cont = Continuation.create () in
    let new_handler =
      let params_and_handler =
        Continuation_params_and_handler.create kinded_params
          ~handler:apply_cont_expr
      in
      Continuation_handler.create ~params_and_handler
        ~stub:false
        ~is_exn_handler:false
    in
    Some (new_cont, new_handler)

let add_wrapper_for_fixed_arity_continuation uacc cont ~use_id arity ~around =
  let new_let_cont =
    add_wrapper_for_fixed_arity_continuation0 uacc cont ~use_id arity
  in
  match new_let_cont with
  | None -> around cont
  | Some (new_cont, new_handler) ->
    Let_cont.create_non_recursive new_cont new_handler
      ~body:(around new_cont)

let add_wrapper_for_fixed_arity_apply uacc ~use_id arity apply =
  let cont = Apply.continuation apply in
  add_wrapper_for_fixed_arity_continuation uacc cont ~use_id arity
    ~around:(fun return_cont ->
      let return_cont =
        UE.resolve_continuation_aliases (UA.uenv uacc) return_cont
      in
      let exn_cont =
        UE.resolve_exn_continuation_aliases (UA.uenv uacc)
          (Apply.exn_continuation apply)
      in
      let apply = Apply.with_continuations apply return_cont exn_cont in
      Expr.create_apply apply)

let update_exn_continuation_extra_args uacc ~exn_cont_use_id apply =
  let exn_cont_rewrite =
    UE.find_apply_cont_rewrite (UA.uenv uacc)
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
  in
  match exn_cont_rewrite with
  | None -> apply
  | Some rewrite ->
    Apply.with_exn_continuation apply
      (Apply_cont_rewrite.rewrite_exn_continuation rewrite exn_cont_use_id
        (Apply.exn_continuation apply))

(* CR mshinwell: Should probably move [Reachable] into the [Flambda] recursive
   loop and then move this into [Expr].  Maybe this could be tidied up a bit
   too? *)
let bind_let_bound ~bindings ~body =
  List.fold_left
    (fun expr
         ((bound : Bindable_let_bound.t), (defining_expr : Reachable.t)) ->
      match defining_expr with
      | Invalid _ -> Expr.create_invalid ()
      | Reachable defining_expr ->
        match bound with
        | Singleton var -> Expr.bind ~bindings:[var, defining_expr] ~body:expr
        | Set_of_closures _ -> Expr.create_pattern_let bound defining_expr expr)
    body
    (List.rev bindings)

let create_let_symbol code_age_relation (bound_symbols : Bound_symbols.t)
      (static_const : Static_const.t) body =
  let free_names_after = Expr.free_names body in
  let symbols_after = Name_occurrences.symbols free_names_after in
  match bound_symbols with
  | Singleton sym ->
    if Symbol.Set.mem sym symbols_after then
      Let_symbol.create bound_symbols static_const body
      |> Expr.create_let_symbol
    else
      body
  | Code_and_set_of_closures { code_ids; closure_symbols; } ->
    let code, set_of_closures =
      (* CR mshinwell: Move to [Static_const.must_be_code] or something *)
      match static_const with
      | Code_and_set_of_closures { code; set_of_closures; } -> 
        code, set_of_closures
      | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_array _ | Mutable_string _
      | Immutable_string _ ->
        Misc.fatal_errorf "Illegal [Let_symbol] binding of %a@ to@ %a"
          Bound_symbols.print bound_symbols
          Static_const.print static_const
    in
    let free_names_in_const = Static_const.free_names static_const in
    let code =
      Code_id.Map.mapi
        (fun code_id
             ({ params_and_body; newer_version_of; } : Static_const.code)
             : Static_const.code ->
          let params_and_body : _ Static_const.or_deleted =
            match params_and_body with
            | Deleted -> Deleted
            | Present _ ->
              let code_unused =
                (not (Name_occurrences.mem_code_id free_names_after code_id))
                  && (not (Name_occurrences.mem_code_id free_names_in_const
                    code_id))
              in
              let can_delete_code =
                (* We cannot delete code unless it is certain that a
                   non-trivial join operation between later versions of it
                   cannot happen. *)
                code_unused
                  && Code_age_relation.newer_versions_form_linear_chain
                       code_age_relation code_id
              in
              if can_delete_code then Deleted
              else params_and_body
          in
          { params_and_body; newer_version_of; })
        code
    in
    (* For the moment we don't delete individual closures from sets of closures.
       We could do this, but we would be relying on 100% conversion of
       [Select_closure] expressions to symbols (which should happen, but even
       still). *)
    let all_closure_symbols_unused =
      Symbol.Set.for_all (fun symbol ->
          (not (Name_occurrences.mem_symbol free_names_after symbol))
            && (not (Name_occurrences.mem_symbol free_names_in_const symbol)))
        (Bound_symbols.closure_symbols_being_defined bound_symbols)
    in
    let set_of_closures =
      if all_closure_symbols_unused then None
      else set_of_closures
    in
    let static_const : Static_const.t =
      Code_and_set_of_closures {
        code;
        set_of_closures;
      }
    in
    let closure_symbols =
      if all_closure_symbols_unused then Closure_id.Map.empty
      else closure_symbols
    in
    let bound_symbols : Bound_symbols.t =
      Code_and_set_of_closures { code_ids; closure_symbols; }
    in
    if Code_id.Set.is_empty code_ids && Closure_id.Map.is_empty closure_symbols
    then body
    else
      Let_symbol.create bound_symbols static_const body
      |> Expr.create_let_symbol
