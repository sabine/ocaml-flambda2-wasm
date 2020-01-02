(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let lift_non_closure_discovered_via_reified_continuation_param_types dacc
      var to_lift ~reified_continuation_params_to_symbols
      ~reified_definitions ~closure_symbols_by_set =
  let static_const = Reification.create_static_const to_lift in
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name var))
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      DE.add_equation_on_name (DE.define_symbol denv symbol K.value)
        (Name.var var)
        (T.alias_type_of K.value (Simple.symbol symbol)))
  in
  let reified_definitions =
    (Let_symbol.Bound_symbols.Singleton symbol, static_const)
      :: reified_definitions
  in
  let reified_continuation_params_to_symbols =
    Variable.Map.add var symbol reified_continuation_params_to_symbols
  in
  dacc, reified_continuation_params_to_symbols, reified_definitions,
    closure_symbols_by_set

let lift_set_of_closures_discovered_via_reified_continuation_param_types dacc
      var closure_id function_decls ~closure_vars
      ~reified_continuation_params_to_symbols
      ~reified_definitions ~closure_symbols_by_set =
  let module I = T.Function_declaration_type.Inlinable in
  let set_of_closures =
    let function_decls =
      Closure_id.Map.map (fun inlinable ->
        Function_declaration.create ~code_id:(I.code_id inlinable)
          ~params_arity:(I.param_arity inlinable)
          ~result_arity:(I.result_arity inlinable)
          ~stub:(I.stub inlinable)
          ~dbg:(I.dbg inlinable)
          ~inline:(I.inline inlinable)
          ~is_a_functor:(I.is_a_functor inlinable)
          ~recursive:(I.recursive inlinable))
        function_decls
      |> Function_declarations.create
    in
    Set_of_closures.create function_decls ~closure_elements:closure_vars
  in
  let bind_continuation_param_to_symbol dacc ~closure_symbols =
    let dacc, symbol =
      DA.map_denv2 dacc ~f:(fun denv ->
        match Closure_id.Map.find closure_id closure_symbols with
        | exception Not_found ->
          Misc.fatal_errorf "Variable %a claimed to hold closure with \
              closure ID %a, but no symbol was found for that closure ID"
            Variable.print var
            Closure_id.print closure_id
        | symbol ->
          let denv =
            DE.add_equation_on_name denv (Name.var var)
              (T.alias_type_of K.value (Simple.symbol symbol))
          in
          denv, symbol)
    in
    dacc, Variable.Map.add var symbol reified_continuation_params_to_symbols
  in
  match Set_of_closures.Map.find set_of_closures closure_symbols_by_set with
  | exception Not_found ->
    let closure_symbols =
      Closure_id.Map.mapi (fun closure_id _function_decl ->
          (* CR mshinwell: share name computation with
             [Simplify_named] *)
          let name =
            closure_id
            |> Closure_id.rename
            |> Closure_id.to_string
            |> Linkage_name.create 
          in
          Symbol.create (Compilation_unit.get_current_exn ()) name)
        function_decls
    in
    let definition =
      (* We don't need to assign new code IDs, since we're not changing the
         code. The code will actually be re-simplified (when we reach the new
         [Let_symbol] bindings)---at that point, new code IDs may well be
         assigned. (That is also the point at which references to the closures
         being lifted, via the continuation's parameters, will be changed to go
         via symbols.) *)
      Let_symbol.pieces_of_code
        ~newer_versions_of:Code_id.Map.empty
        ~set_of_closures:(closure_symbols, set_of_closures)
        Code_id.Map.empty
    in
    let reified_definitions = definition :: reified_definitions in
    let closure_symbols_by_set =
      Set_of_closures.Map.add set_of_closures closure_symbols
        closure_symbols_by_set
    in
    let dacc, reified_continuation_params_to_symbols =
      bind_continuation_param_to_symbol dacc ~closure_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set
  | closure_symbols ->
    let dacc, reified_continuation_params_to_symbols =
      bind_continuation_param_to_symbol dacc ~closure_symbols
    in
    dacc, reified_continuation_params_to_symbols, reified_definitions,
      closure_symbols_by_set

let reify_types_of_continuation_param_types dacc
      reified_continuation_param_types =
  let orig_typing_env = DE.typing_env (DA.denv dacc) in
  Variable.Set.fold
    (fun var (dacc, reified_continuation_params_to_symbols, reified_definitions,
              closure_symbols_by_set) ->
      let ty = TE.find orig_typing_env (Name.var var) in
      let existing_symbol =
        (* We must avoid attempting to create aliases between symbols or
           (equivalently) defining static parts that already have symbols.
           This could happen if [var] is actually equal to another of the
           continuation's parameters. *)
        TE.get_canonical_simple (DE.typing_env (DA.denv dacc))
          ~min_name_mode:NM.normal (Simple.var var)
        |> Or_bottom.value_map ~bottom:None
             ~f:(Option.map Simple.must_be_symbol)
      in
      match existing_symbol with
      | Some _ ->
        dacc, reified_continuation_params_to_symbols, reified_definitions,
          closure_symbols_by_set
      | None ->
        match
          T.reify ~allowed_free_vars:reified_continuation_param_types
            (DE.typing_env (DA.denv dacc)) ~min_name_mode:NM.normal ty
        with
        | Lift to_lift ->
          lift_non_closure_discovered_via_reified_continuation_param_types
            dacc var to_lift ~reified_continuation_params_to_symbols
            ~reified_definitions ~closure_symbols_by_set
        | Lift_set_of_closures { closure_id; function_decls; closure_vars; } ->
          lift_set_of_closures_discovered_via_reified_continuation_param_types
            dacc var closure_id function_decls ~closure_vars
            ~reified_continuation_params_to_symbols
            ~reified_definitions ~closure_symbols_by_set
        | Simple _ | Cannot_reify | Invalid ->
          dacc, reified_continuation_params_to_symbols, reified_definitions,
            closure_symbols_by_set)
    reified_continuation_param_types
    (dacc, Variable.Map.empty, [], Set_of_closures.Map.empty)

module Bindings_top_sort =
  Top_closure.Make
    (struct
      type t = Symbol.Set.Set.t
      type elt = Symbol.Set.t
      let empty = Symbol.Set.Set.empty
      let add t elt = Symbol.Set.Set.add elt t
      let mem t elt = Symbol.Set.Set.mem elt t
    end)
    (struct
      type 'a t = 'a
      let return t = t
      let (>>=) t f = f t
    end)

let lift_via_reification_of_continuation_param_types dacc ~params
      ~(extra_params_and_args : Continuation_extra_params_and_args.t)
      ~(handler : Expr.t) =
  let allowed_free_vars =
    Variable.Set.union (KP.List.var_set params)
      (KP.List.var_set extra_params_and_args.extra_params)
  in
  let dacc, reified_continuation_params_to_symbols, reified_definitions,
      _closure_symbols_by_set =
    reify_types_of_continuation_param_types dacc allowed_free_vars
  in
  (* CR mshinwell: If recursion extends beyond that which can be handled
     by the set-of-closures cases, then we would need a strongly connected
     components analysis, prior to the top sort.  Any set arising from SCC
     that has more than one element must be a complicated recursive case,
     which could be dealt with using the "symbol placeholder" approach
     (variables that are substituted for the continuation's parameters, which
     are in turn substituted for symbols at the Cmm translation phase).
     (Any case containing >1 set of closures is maybe a bug?) *)
  let reified_definitions =
    let sorted =
      Bindings_top_sort.top_closure reified_definitions
        ~key:(fun (bound_syms, _static_const) ->
          Let_symbol.Bound_symbols.being_defined bound_syms)
        ~deps:(fun (bound_syms, static_const) ->
          let var_deps =
            static_const
            |> Static_const.free_names
            |> Name_occurrences.variables
            |> Variable.Set.elements
          in
          let sym_deps =
            List.fold_left (fun sym_deps var ->
                match
                  Variable.Map.find var reified_continuation_params_to_symbols
                with
                | exception Not_found ->
                  Misc.fatal_errorf "No symbol for continuation parameter %a"
                    Variable.print var
                | symbol -> Symbol.Set.add symbol sym_deps)
              Symbol.Set.empty
              var_deps
          in
          let sym_deps =
            let closure_symbols_being_defined =
              (* Closure symbols are bound recursively within the same set. We
                 need to remove them from the dependency sets to avoid the
                 topological sort seeing cycles. We don't unilaterally remove
                 all symbols in [Bound_symbols.being_defined syms] (as opposed
                 to [closure_symbols_being_defined]) as, in the case where
                 illegal recursion has been constructed, it could cause the
                 topological sort to succeed and the fatal error below to be
                 concealed. *)
              Let_symbol.Bound_symbols.closure_symbols_being_defined bound_syms
            in
            Symbol.Set.diff sym_deps closure_symbols_being_defined
          in
          Symbol.Set.fold (fun sym deps ->
              let dep =
                List.find_opt (fun (bound_symbols, _static_const) ->
                    Symbol.Set.mem sym
                      (Let_symbol.Bound_symbols.being_defined bound_symbols))
                  reified_definitions
              in
              match dep with
              | Some dep -> dep :: deps
              | None ->
                Misc.fatal_errorf "Couldn't find definition for %a"
                  Symbol.print sym)
            sym_deps
            [])
    in
    match sorted with
    | Ok sorted -> sorted
    | Error _ ->
      Misc.fatal_errorf "Potential [Let_symbol] bindings arising from reified \
          types of continuation parameters contain recursion that cannot be \
          compiled:@ %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          (fun ppf (bound_symbols, defining_expr) ->
            Format.fprintf ppf "@[<hov 1>%a@ %a@]"
              Let_symbol.Bound_symbols.print bound_symbols
              Static_const.print defining_expr))
        reified_definitions
  in
  (* By effectively reversing the list during the fold, we rely on the
     following property:
       Let the list L be a topological sort of a directed graph G.
       Then the reverse of L is a topological sort of the transpose of G.
  *)
  let handler =
    List.fold_left (fun handler (bound_symbols, defining_expr) ->
        Let_symbol.create bound_symbols defining_expr handler
        |> Expr.create_let_symbol)
      handler
      reified_definitions
  in
  dacc, handler
