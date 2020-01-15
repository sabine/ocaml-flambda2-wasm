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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

module SCC_lifted_constants =
  Strongly_connected_components.Make (Code_id_or_symbol)

type result = {
  bindings_outermost_last : (Bound_symbols.t * Static_const.t) list;
}

let build_dep_graph lifted_constants =
  let all_symbols_being_defined =
    Symbol.Set.union_list
      (List.map (fun (bound_symbols, _) ->
          Bound_symbols.being_defined bound_symbols)
        lifted_constants)
  in
  List.fold_left
    (fun (dep_graph, code_id_or_symbol_to_const)
         (bound_symbols, defining_expr) ->
      (*
      Format.eprintf "Input: %a = %a\n%!"
        Bound_symbols.print bound_symbols
        Static_const.print defining_expr;
      *)
      Code_id_or_symbol.Set.fold
        (fun (being_defined : Code_id_or_symbol.t)
             (dep_graph, code_id_or_symbol_to_const) ->
          let free_names =
            match being_defined with
            | Code_id code_id ->
              begin match (defining_expr : Static_const.t) with
              | Code_and_set_of_closures { code; set_of_closures = _; } ->
                assert (Code_id.Map.mem code_id code);
                let code = Code_id.Map.find code_id code in
                let free_names_params_and_body =
                  match code.params_and_body with
                  | Deleted -> Name_occurrences.empty
                  | Present params_and_body ->
                    Function_params_and_body.free_names params_and_body
                in
                let free_names_newer_version_of =
                  match code.newer_version_of with
                  | None -> Name_occurrences.empty
                  | Some newer_version_of ->
                    Name_occurrences.add_newer_version_of_code_id
                      Name_occurrences.empty newer_version_of NM.normal
                in
                Name_occurrences.union free_names_params_and_body
                  free_names_newer_version_of
              | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
              | Boxed_nativeint _ | Immutable_float_array _ | Mutable_string _
              | Immutable_string _ ->
                Misc.fatal_errorf "Bad defining expression@ %a@ for@ %a"
                  Static_const.print defining_expr
                  Bound_symbols.print bound_symbols
              end
            | Symbol symbol ->
              begin match (bound_symbols : Bound_symbols.t) with
              | Singleton _ -> Static_const.free_names defining_expr
              | Sets_of_closures sets ->
                List.fold_left
                  (fun free_names
                       ({ code_ids = _; closure_symbols; }
                         : Bound_symbols.Code_and_set_of_closures.t) ->
                    let closure_symbols =
                      closure_symbols
                      |> Closure_id.Map.bindings
                      |> List.map (fun (closure_id, sym) -> sym, closure_id)
                      |> Symbol.Map.of_list
                    in
                    assert (Symbol.Map.mem symbol closure_symbols);
                    let closure_id = Symbol.Map.find symbol closure_symbols in
                    match (defining_expr : Static_const.t) with
                    | Code_and_set_of_closures
                        { code = _; set_of_closures; } ->
                      begin match set_of_closures with
                      | None ->
                        Misc.fatal_errorf "No set of closures given for@ %a"
                          Symbol.print symbol
                      | Some set_of_closures ->
                        (* CR mshinwell: Make sure the closure ID exists in
                           the set of closures *)
                        ignore closure_id;
                        (* We take the free names of the whole set of closures
                           for every closure symbol defined by it.  We also
                           add explicit dependencies between every member of
                           a set of closures and every other member of the
                           same set, to avoid individual sets of closures
                           being pulled apart. *)
                        Symbol.Map.fold (fun symbol _ free_names ->
                            Name_occurrences.add_symbol free_names symbol
                              NM.normal)
                          closure_symbols
                          (Set_of_closures.free_names set_of_closures)
                      end
                    | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
                    | Boxed_nativeint _ | Immutable_float_array _
                    | Mutable_string _ | Immutable_string _ ->
                      Misc.fatal_errorf "Bad defining expression@ %a@ for@ %a"
                        Static_const.print defining_expr
                        Bound_symbols.print bound_symbols)
                  free_names
                  sets
              end
          in
          let free_syms = Name_occurrences.symbols free_names in
          (* Beware: when coming from [Reify_continuation_params] the
             sets of closures may have dependencies on variables that are
             now equal to symbols in the environment.  (They haven't been
             changed to symbols yet as the simplifier hasn't been run on
             the definitions.)  Some of these symbols may be the ones
             involved in the current SCC calculation.  For this latter set,
             we must explicitly add them as dependencies. *)
          let free_syms =
            Variable.Set.fold (fun var free_syms ->
                let typing_env = DE.typing_env (DA.denv dacc) in
                let canonical =
                  TE.get_canonical_simple typing_env
                    ~min_name_mode:NM.normal
                    (Simple.var var)
                in
                match canonical with
                | Bottom | Ok None -> free_syms
                | Ok (Some canonical) ->
                  match Simple.descr canonical with
                  | Name (Var _) | Const _ -> free_syms
                  | Name (Symbol sym) ->
                    if Symbol.Set.mem sym all_symbols_being_defined then
                      Symbol.Set.add sym free_syms
                    else
                      free_syms)
              (Name_occurrences.variables free_names)
              free_syms
          in
          let free_code_ids =
            Code_id.Set.union (Name_occurrences.code_ids free_names)
              (Name_occurrences.newer_version_of_code_ids free_names)
          in
          let free_syms =
            Symbol.Set.fold (fun sym free_syms ->
                Code_id_or_symbol.Set.add (Symbol sym) free_syms)
              free_syms
              Code_id_or_symbol.Set.empty
          in
          let free_code_ids =
            Code_id.Set.fold (fun code_id free_code_ids ->
                Code_id_or_symbol.Set.add (Code_id code_id) free_code_ids)
              free_code_ids
              Code_id_or_symbol.Set.empty
          in
          let deps = Code_id_or_symbol.Set.union free_syms free_code_ids in
          let dep_graph =
            Code_id_or_symbol.Map.add being_defined deps dep_graph
          in
          let code_id_or_symbol_to_const =
            Code_id_or_symbol.Map.add being_defined
              (bound_symbols, defining_expr)
              code_id_or_symbol_to_const
          in
          dep_graph, code_id_or_symbol_to_const)
        (Bound_symbols.everything_being_defined bound_symbols)
        (dep_graph, code_id_or_symbol_to_const))
    (Code_id_or_symbol.Map.empty, Code_id_or_symbol.Map.empty)
    lifted_constants

let sort dacc lifted_constants =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code).  We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph, code_id_or_symbol_to_const =
    build_dep_graph lifted_constants
  in
  (*
  Format.eprintf "SCC graph is:@ %a\n%!"
    (Code_id_or_symbol.Map.print Code_id_or_symbol.Set.print)
    lifted_constants_dep_graph;
  *)
  let connected_components =
    SCC_lifted_constants.connected_components_sorted_from_roots_to_leaf
      lifted_constants_dep_graph
  in
  let bindings_outermost_last =
    Array.fold_left (fun bindings (group : SCC_lifted_constants.component) ->
        let binding =
          match group with
          | No_loop code_id_or_symbol ->
            Code_id_or_symbol.Map.find code_id_or_symbol
              code_id_or_symbol_to_const
          | Has_loop members ->
            let sets_of_closures_bound_symbols, code_and_sets_of_closures =
              List.fold_left
                (fun ((defining_expr_already_seen,
                       sets_of_closures_bound_symbols_acc,
                       code_and_sets_of_closures_acc) as acc)
                     code_id_or_symbol ->
                  if Code_id_or_symbol.Set.mem code_id_or_symbol
                       defining_expr_already_seen
                  then acc
                  else
                    let bound_symbols, defining_expr =
                      Code_id_or_symbol.Map.find code_id_or_symbol
                        code_id_or_symbol_to_const
                    in
                    (* We may encounter the same defining expression more
                       than once (e.g. a set of closures via a code ID and
                       a symbol), but we don't want duplicates in the result
                       list. *)
                    let defining_expr_already_seen =
                      Code_id_or_symbol.Set.union
                        (Bound_symbols.everything_being_defined bound_symbols)
                        defining_expr_already_seen
                    in
                    let sets_of_closures_bound_symbols =
                      match bound_symbols with
                      | Sets_of_closures sets -> sets
                      | Singleton _ ->
                        Misc.fatal_errorf "Code ID or symbol %a was involved \
                            in (non-closure) recursion that cannot be compiled"
                          Code_id_or_symbol.print code_id_or_symbol
                    in
                    let code_and_set_of_closures_list =
                      match defining_expr with
                      | Sets_of_closures code_and_set_of_closures_list ->
                        code_and_set_of_closures_list
                      | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
                      | Boxed_nativeint _ | Immutable_float_array _
                      | Mutable_string _ | Immutable_string _ ->
                        Misc.fatal_errorf "Bad defining expression for@ %a:@ %a"
                          Bound_symbols.print bound_symbols
                          Static_const.print defining_expr
                    in
                    defining_expr_already_seen,
                      sets_of_closures_bound_symbols
                        :: sets_of_closures_bound_symbols_acc,
                      code_and_set_of_closures_list
                        @ code_and_sets_of_closures_acc)
                (Code_id_or_symbol.Set.empty, [], [])
                members
            in
            let bound_symbols : Bound_symbols.t =
              Sets_of_closures sets_of_closures_bound_symbols
            in
            let defining_expr : Static_const.t =
              Sets_of_closures code_and_sets_of_closures
            in
            bound_symbols, defining_expr
        in
        binding :: bindings)
      []
      (Array.of_list (List.rev (Array.to_list connected_components)))
  in
  (* By effectively reversing the list during a subsequent fold on this
     result, we rely on the following property:
       Let the list L be a topological sort of a directed graph G.
       Then the reverse of L is a topological sort of the transpose of G.
  *)
  (*
  Format.eprintf "Result, outermost first:@ %a\n%!"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
      Bound_symbols.print)
    (List.rev (List.map fst bindings_outermost_last));
  *)
  { bindings_outermost_last; }
