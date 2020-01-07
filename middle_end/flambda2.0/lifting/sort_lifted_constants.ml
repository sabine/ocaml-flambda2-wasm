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

type non_closure_or_closure_symbol =
  | Non_closure_symbol of Static_const.t
  | Closure_symbol of Closure_id.t * Set_of_closures.t

type result = {
  bindings_outermost_last : (Bound_symbols.t * Static_const.t) list;
}

let sort lifted_constants =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code).  We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph =
    List.fold_left (fun dep_graph (bound_symbols, defining_expr) ->
        Code_id_or_symbol.Set.fold
          (fun (being_defined : Code_id_or_symbol.t) dep_graph ->
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
                | Code_and_set_of_closures { code_ids = _; closure_symbols; } ->
                  let closure_symbols =
                    closure_symbols
                    |> Closure_id.Map.bindings
                    |> List.map (fun (closure_id, sym) -> sym, closure_id)
                    |> Symbol.Map.of_list
                  in
                  assert (Symbol.Map.mem symbol closure_symbols);
                  let closure_id = Symbol.Map.find symbol closure_symbols in
                  match (defining_expr : Static_const.t) with
                  | Code_and_set_of_closures { code = _; set_of_closures; } ->
                    begin match set_of_closures with
                    | None ->
                      Misc.fatal_errorf "No set of closures given for@ %a"
                        Symbol.print symbol
                    | Some set_of_closures ->
                      (* CR mshinwell: Make sure the closure ID exists in the
                         set of closures *)
                      ignore closure_id;
                      (* We take the free names of the whole set of closures
                         for every closure symbol defined by it. *)
                      (* CR mshinwell: Think about this more.  We need to
                         prevent sets of closures being split apart somehow. For
                         the moment add explicit dependencies between all
                         closure symbols in the set. *)
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
                      Bound_symbols.print bound_symbols
                end
            in
            let free_syms = Name_occurrences.symbols free_names in
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
            Code_id_or_symbol.Map.add being_defined deps dep_graph)
          (Bound_symbols.everything_being_defined bound_symbols)
          dep_graph)
      Code_id_or_symbol.Map.empty
      lifted_constants
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
  let find_code code_id =
    let result =
      List.find_map (fun (_bound_symbols, (static_const : Static_const.t)) ->
          match static_const with
          | Code_and_set_of_closures { code; set_of_closures = _; } ->
            Code_id.Map.find_opt code_id code
          | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
          | Boxed_nativeint _ | Immutable_float_array _
          | Mutable_string _ | Immutable_string _ -> None)
        lifted_constants
    in
    match result with
    | None ->
      Misc.fatal_errorf "Definition for code ID %a not found"
        Code_id.print code_id
    | Some code -> code
  in
  let find_non_closure_or_closure_symbol sym =
    let result =
      List.find_map
        (fun ((bound_symbols : Bound_symbols.t),
              (static_const : Static_const.t))
             : non_closure_or_closure_symbol option ->
          match bound_symbols with
          | Singleton sym' ->
            if Symbol.equal sym sym' then Some (Non_closure_symbol static_const)
            else None
          | Code_and_set_of_closures { code_ids = _; closure_symbols; } ->
            let closure_symbols =
              (* CR mshinwell: duplicated from above *)
              closure_symbols
              |> Closure_id.Map.bindings
              |> List.map (fun (closure_id, sym) -> sym, closure_id)
              |> Symbol.Map.of_list
            in
            match Symbol.Map.find sym closure_symbols with
            | exception Not_found -> None
            | closure_id ->
              (* CR mshinwell: duplicated from above *)
              match (static_const : Static_const.t) with
              | Code_and_set_of_closures { code = _; set_of_closures; } ->
                begin match set_of_closures with
                | None ->
                  Misc.fatal_errorf "No set of closures given for@ %a@ in \
                      binding of:@ %a@ to:@ %a"
                    Symbol.print sym
                    Bound_symbols.print bound_symbols
                    Static_const.print static_const
                | Some set_of_closures ->
                  (* CR mshinwell: Make sure the closure ID exists in the
                     set of closures *)
                  Some (Closure_symbol (closure_id, set_of_closures))
                end
              | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
              | Boxed_nativeint _ | Immutable_float_array _
              | Mutable_string _ | Immutable_string _ ->
                Misc.fatal_errorf "Bad defining expression@ %a@ for@ %a"
                  Static_const.print static_const
                  Bound_symbols.print bound_symbols)
        lifted_constants
    in
    match result with
    | None ->
      Misc.fatal_errorf "Definition for symbol %a not found" Symbol.print sym
    | Some result -> result
  in
  let bindings_outermost_last =
    Array.fold_left (fun bindings (group : SCC_lifted_constants.component) ->
        let binding =
          match group with
          | No_loop (Code_id code_id) ->
            let code = find_code code_id in
            let bound_symbols : Bound_symbols.t =
              Code_and_set_of_closures {
                code_ids = Code_id.Set.singleton code_id;
                closure_symbols = Closure_id.Map.empty;
              }
            in
            let defining_expr : Static_const.t =
              Code_and_set_of_closures {
                code = Code_id.Map.singleton code_id code;
                set_of_closures = None;
              }
            in
            bound_symbols, defining_expr
          | No_loop (Symbol sym) ->
            (* CR mshinwell: What happens if [sym] is one part of a set
               of closures with more than one closure inside it? *)
            begin match find_non_closure_or_closure_symbol sym with
            | Non_closure_symbol defining_expr ->
              let bound_symbols : Bound_symbols.t = Singleton sym in
              bound_symbols, defining_expr
            | Closure_symbol (closure_id, set_of_closures) ->
              let bound_symbols : Bound_symbols.t =
                Code_and_set_of_closures {
                  code_ids = Code_id.Set.empty;
                  closure_symbols = Closure_id.Map.singleton closure_id sym;
                }
              in
              let defining_expr : Static_const.t =
                Code_and_set_of_closures {
                  code = Code_id.Map.empty;
                  set_of_closures = Some set_of_closures;
                }
              in
              bound_symbols, defining_expr
            end
          | Has_loop members ->
            (* CR mshinwell: It seems like this may end up copying sets of
               closures. *)
            let code_ids, closure_symbols, code, set_of_closures =
              List.fold_left
                (fun (code_ids, closure_symbols, code, set_of_closures)
                     (member : Code_id_or_symbol.t) ->
                  match member with
                  | Code_id code_id ->
                    let code_ids = Code_id.Set.add code_id code_ids in
                    let code =
                      Code_id.Map.add code_id (find_code code_id) code
                    in
                    code_ids, closure_symbols, code, set_of_closures
                  | Symbol symbol ->
                    match find_non_closure_or_closure_symbol symbol with
                    | Closure_symbol (closure_id, set) ->
                      assert (not (Closure_id.Map.mem closure_id
                        closure_symbols));
                      let closure_symbols =
                        Closure_id.Map.add closure_id symbol closure_symbols
                      in
                      let set_of_closures =
                        match set_of_closures with
                        | None -> Some set
                        | Some set' ->
                          if not (Set_of_closures.equal set set') then
                            match Set_of_closures.disjoint_union set set' with
                            | exception (Invalid_argument _) ->
                              Misc.fatal_errorf "Sets of closures for SCC \
                                  component@ {%a}@ are not disjoint:@ %a@ and@ \
                                  %a"
                                (Format.pp_print_list
                                  ~pp_sep:Format.pp_print_space
                                  Code_id_or_symbol.print)
                                members
                                Set_of_closures.print set
                                Set_of_closures.print set'
                            | set -> Some set
                          else
                            set_of_closures
                      in
                      code_ids, closure_symbols, code, set_of_closures
                    | Non_closure_symbol _ ->
                      Misc.fatal_errorf "Symbol %a was involved in recursion \
                          that cannot be compiled"
                        Symbol.print symbol)
                (Code_id.Set.empty, Closure_id.Map.empty,
                  Code_id.Map.empty, None)
                members
            in
            let bound_symbols : Bound_symbols.t =
              Code_and_set_of_closures {
                code_ids;
                closure_symbols;
              }
            in
            let defining_expr : Static_const.t =
              Code_and_set_of_closures {
                code;
                set_of_closures;
              }
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
  { bindings_outermost_last; }
