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

module Bound_symbols = Let_symbol.Bound_symbols
module Field_of_block = Static_const.Field_of_block

(* CR-someday mshinwell: Finish improved simplification using types *)

let simplify_field_of_block dacc (field : Field_of_block.t) =
  match field with
  | Symbol sym -> field, T.alias_type_of K.value (Simple.symbol sym)
  | Tagged_immediate i -> field, T.this_tagged_immediate i
  | Dynamically_computed var ->
    let min_name_mode = Name_mode.normal in
    match S.simplify_simple dacc (Simple.var var) ~min_name_mode with
    | Bottom, ty ->
      assert (K.equal (T.kind ty) K.value);
      (* CR mshinwell: This should be "invalid" and propagate up *)
      field, T.bottom K.value
    | Ok simple, ty ->
      match Simple.descr simple with
      | Name (Symbol sym) -> Field_of_block.Symbol sym, ty
      | Name (Var _) -> field, ty
      | Const (Tagged_immediate imm) -> Field_of_block.Tagged_immediate imm, ty
      | Const (Naked_immediate _ | Naked_float _ | Naked_int32 _
          | Naked_int64 _ | Naked_nativeint _) ->
        (* CR mshinwell: This should be "invalid" and propagate up *)
        field, ty

let simplify_or_variable dacc type_for_const
      (or_variable : _ Static_const.or_variable) =
  let denv = DA.denv dacc in
  match or_variable with
  | Const const -> or_variable, type_for_const const
  | Var var ->
    (* CR mshinwell: This needs to check the type of the variable according
       to the various cases below. *)
    or_variable, DE.find_variable denv var

let simplify_static_const_of_kind_value dacc
      (static_const : Static_const.t) ~result_sym
      : Static_const.t * DA.t =
  let bind_result_sym typ =
    DA.map_denv dacc ~f:(fun denv ->
      let denv =
        (* [result_sym] will already be defined when we are lifting
           reified continuation parameters. *)
        (* CR mshinwell: This is kind of nasty---try to rearrange things
           so this doesn't happen. *)
        DE.define_symbol_if_undefined denv result_sym K.value
      in
      DE.add_equation_on_symbol denv result_sym typ)
  in
  match static_const with
  | Block (tag, is_mutable, fields) ->
    let fields_with_tys =
      List.map (fun field -> simplify_field_of_block dacc field) fields
    in
    let fields, field_tys = List.split fields_with_tys in
    let ty =
      T.immutable_block (Tag.Scannable.to_tag tag) ~field_kind:K.value
        ~fields:field_tys
    in
    let dacc = bind_result_sym ty in
    Block (tag, is_mutable, fields), dacc
  (* CR mshinwell: Need to reify to change Equals types into new terms *)
  | Boxed_float or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_float f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_float or_var, dacc
  | Boxed_int32 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int32 f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_int32 or_var, dacc
  | Boxed_int64 or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_int64 f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_int64 or_var, dacc
  | Boxed_nativeint or_var ->
    let or_var, ty =
      simplify_or_variable dacc (fun f -> T.this_boxed_nativeint f) or_var
    in
    let dacc = bind_result_sym ty in
    Boxed_nativeint or_var, dacc
  | Immutable_float_array fields ->
    let fields_with_tys =
      List.map (fun field ->
          simplify_or_variable dacc (fun f -> T.this_naked_float f) field)
        fields
    in
    let fields, _field_tys = List.split fields_with_tys in
    let dacc = bind_result_sym (T.any_value ()) in
    Immutable_float_array fields, dacc
  | Mutable_string { initial_value; } ->
    let str_ty = T.mutable_string ~size:(String.length initial_value) in
    let static_const : Static_const.t =
      Mutable_string {
        initial_value;
      }
    in
    let dacc = bind_result_sym str_ty in
    static_const, dacc
  | Immutable_string str ->
    let ty = T.this_immutable_string str in
    let dacc = bind_result_sym ty in
    Immutable_string str, dacc
  | Sets_of_closures _ ->
    Misc.fatal_errorf "[Sets_of_closures] cannot be bound by a \
        [Singleton] binding:@ %a"
      SC.print static_const

let simplify_set_of_closures0 dacc set_of_closures ~closure_symbols
      ~closure_elements ~closure_element_types =
  let closure_bound_names =
    Closure_id.Map.map Name_in_binding_pos.symbol closure_symbols
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      Closure_id.Map.fold (fun _closure_id symbol denv ->
          DE.define_symbol_if_undefined denv symbol K.value)
        closure_symbols
        denv)
  in
  let { Simplify_named.
        set_of_closures;
        closure_types_by_bound_name;
        newer_versions_of;
        code;
        dacc;
      } =
    Simplify_named.simplify_set_of_closures0 dacc set_of_closures
      ~closure_bound_names ~closure_elements ~closure_element_types
  in
  (* CR mshinwell: See comment in simplify_non_lifted_set_of_closures about
     the following *)
  let dacc =
    DA.map_r dacc ~f:(fun r ->
      R.new_lifted_constant r
        (Lifted_constant.create_pieces_of_code (DA.denv dacc)
          code ~newer_versions_of))
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      DE.add_lifted_constants denv
        ~lifted:(R.get_lifted_constants (DA.r dacc)))
  in
  let code =
    Code_id.Map.mapi (fun code_id params_and_body : Static_const.code ->
        { params_and_body = Present params_and_body;
          newer_version_of = Code_id.Map.find_opt code_id newer_versions_of;
        })
      code
  in
  let static_const : Static_const.t =
    Code_and_set_of_closures {
      code;
      set_of_closures = Some set_of_closures;
    }
  in
  let bound_symbols : Let_symbol.Bound_symbols.t =
    Code_and_set_of_closures {
      code_ids = Code_id.Map.keys code;
      closure_symbols;
    }
  in
  let static_structure_types =
    Name_in_binding_pos.Map.fold
      (fun name closure_type static_structure_types ->
        let symbol = Name_in_binding_pos.must_be_symbol name in
        Symbol.Map.add symbol closure_type static_structure_types)
      closure_types_by_bound_name
      Symbol.Map.empty
  in
  set_of_closures, dacc, static_structure_types, bound_symbols, static_const

let simplify_sets_of_closures dacc sets_of_closures
      ~closure_symbols =
  let closure_elements, closure_element_types =
  let { Simplify_named.
        can_lift = _;
        closure_elements;
        closure_element_types;
      } =
    Simplify_named.type_closure_elements_and_make_lifting_decision dacc
      ~min_name_mode:Name_mode.normal set_of_closures
  in
  simplify_set_of_closures0 dacc set_of_closures ~closure_symbols
    ~closure_elements ~closure_element_types

let simplify_set_of_closures dacc orig_bound_symbols orig_static_const
      (bound_symbols : Bound_symbols.Code_and_set_of_closures.t list)
      (sets : Static_const.code_and_set_of_closures list) =
  if List.compare_lengths bound_symbols sets <> 0 then begin
    Misc.fatal_errorf "Differing number of bound symbols and static constant \
        set-of-closures definitions for@ %a@ =@ %a"
      Bound_symbols.print orig_bound_symbols
      Static_const.print orig_static_const
  end;
  let dacc =
    List.fold_left2
      (fun dacc
           ({ code_ids; closure_symbols; }
             : Bound_symbols.Code_and_set_of_closures.t)
           ({ code; set_of_closures; }
             : Static_const.code_and_set_of_closures) ->
        (* CR mshinwell: Check closure IDs between [closure_symbols] and
           [set_of_closures] too. *)
        let code_ids' = Code_id.Map.keys code in
        if not (Code_id.Set.equal code_ids code_ids') then begin
          Misc.fatal_errorf "Mismatch on declared code IDs (%a and %a):@ %a"
            Code_id.Set.print code_ids
            Code_id.Set.print code_ids'
            Static_const.print static_const
        end;
        Code_id.Map.fold
          (fun code_id
               ({ params_and_body; newer_version_of; } : Static_const.code)
               dacc ->
            (* CR mshinwell: Add check to ensure there are no
               unbound names in the code, since we're not simplifying on the
               way down. *)
            let define_code denv =
              match params_and_body with
              | Deleted -> denv
              | Present params_and_body ->
                DE.define_code denv ?newer_version_of ~code_id
                  ~params_and_body
            in
            let dacc = DA.map_denv dacc ~f:define_code in
            dacc)
          code
          dacc)
      dacc
      bound_symbols sets
  in

  (* Do we need a result_dacc-like thing here? *)

    begin match set_of_closures with
    | None ->
      bound_symbols,
        Code_and_set_of_closures { code; set_of_closures = None; },
        dacc
    | Some set_of_closures ->
      let _set_of_closures, dacc, _static_structure_types,
          bound_symbols, static_const =
        simplify_set_of_closures dacc set_of_closures ~closure_symbols
      in
      bound_symbols, static_const, dacc
    end
  | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_nativeint _
  | Immutable_float_array _ | Mutable_string _ | Immutable_string _ ->
    Misc.fatal_errorf "Only [Code_and_set_of_closures] can be bound by a \
        [Code_and_set_of_closures] binding:@ %a"
      SC.print static_const

let simplify_static_const dacc (bound_symbols : Bound_symbols.t)
      (static_const : SC.t) : Bound_symbols.t * SC.t * DA.t =
  match bound_symbols with
  | Singleton result_sym ->
    let static_const, dacc =
      simplify_static_const_of_kind_value dacc static_const ~result_sym
    in
    bound_symbols, static_const, dacc
  | Sets_of_closures bound_symbols' ->
    match static_const with
    | Sets_of_closures sets ->
      simplify_sets_of_closures dacc bound_symbols static_const
        bound_symbols' sets
    | Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
    | Boxed_nativeint _ | Immutable_float_array _ | Mutable_string _
    | Immutable_string _ ->
      Misc.fatal_errorf "Only [Sets_of_closures] can be bound by a \
          [Sets_of_closures] binding:@ %a"
        SC.print static_const
