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

[@@@ocaml.warning "+a-30-40-41-42"]

let fprintf = Format.fprintf

module K = Flambda_kind

module Field_of_block = struct
  type t =
    | Symbol of Symbol.t
    | Tagged_immediate of Immediate.t
    | Dynamically_computed of Variable.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Symbol s1, Symbol s2 -> Symbol.compare s1 s2
      | Tagged_immediate t1, Tagged_immediate t2 ->
        Immediate.compare t1 t2
      | Dynamically_computed v1, Dynamically_computed v2 ->
        Variable.compare v1 v2
      | Symbol _, Tagged_immediate _ -> -1
      | Tagged_immediate _, Symbol _ -> 1
      | Symbol _, Dynamically_computed _ -> -1
      | Dynamically_computed _, Symbol _ -> 1
      | Tagged_immediate _, Dynamically_computed _ -> -1
      | Dynamically_computed _, Tagged_immediate _ -> 1

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t =
      match t with
      | Symbol symbol -> Hashtbl.hash (0, Symbol.hash symbol)
      | Tagged_immediate immediate ->
        Hashtbl.hash (1, Immediate.hash immediate)
      | Dynamically_computed var -> Hashtbl.hash (2, Variable.hash var)

    let print ppf t =
      match t with
      | Symbol symbol -> Symbol.print ppf symbol
      | Tagged_immediate immediate -> Immediate.print ppf immediate
      | Dynamically_computed var -> Variable.print ppf var

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let apply_name_permutation t perm =
    match t with
    | Symbol _ | Tagged_immediate _ -> t
    | Dynamically_computed var ->
      let var' = Name_permutation.apply_variable perm var in
      if var == var' then t
      else Dynamically_computed var'

  let free_names t =
    match t with
    | Dynamically_computed var ->
      Name_occurrences.singleton_variable var Name_mode.normal
    | Symbol sym ->
      Name_occurrences.singleton_symbol sym Name_mode.normal
    | Tagged_immediate _ -> Name_occurrences.empty

(*
  let invariant env t =
    let module E = Invariant_env in
    match t with
    | Symbol sym -> E.check_symbol_is_bound env sym
    | Tagged_immediate _ -> ()
    | Dynamically_computed var ->
      E.check_variable_is_bound_and_of_kind env var K.value
*)
end

type 'a or_variable =
  | Const of 'a
  | Var of Variable.t

type mutable_or_immutable = Mutable | Immutable

type code = {
  params_and_body : Function_params_and_body.t or_deleted;
  newer_version_of : Code_id.t option;
}
and 'a or_deleted =
  | Present of 'a
  | Deleted

type code_and_set_of_closures = {
  code : code Code_id.Map.t;
  set_of_closures : Set_of_closures.t option;
}

type t =
  | Block of Tag.Scannable.t * mutable_or_immutable * (Field_of_block.t list)
  | Code_and_set_of_closures of code_and_set_of_closures
  | Boxed_float of Numbers.Float_by_bit_pattern.t or_variable
  | Boxed_int32 of Int32.t or_variable
  | Boxed_int64 of Int64.t or_variable
  | Boxed_nativeint of Targetint.t or_variable
  | Immutable_float_array of Numbers.Float_by_bit_pattern.t or_variable list
  | Mutable_string of { initial_value : string; }
  | Immutable_string of string

let get_pieces_of_code t =
  match t with
  | Code_and_set_of_closures { code; set_of_closures = _; } ->
    Code_id.Map.filter_map code
      ~f:(fun _code_id { params_and_body; newer_version_of; } ->
        match params_and_body with
        | Present params_and_body -> Some (params_and_body, newer_version_of)
        | Deleted -> None)
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_array _
  | Mutable_string _
  | Immutable_string _ -> Code_id.Map.empty

let free_names t =
  match t with
  | Block (_tag, _mut, fields) ->
    List.fold_left (fun fvs field ->
        Name_occurrences.union fvs (Field_of_block.free_names field))
      (Name_occurrences.empty)
      fields
  | Code_and_set_of_closures { code; set_of_closures; } ->
    let from_set_of_closures =
      match set_of_closures with
      | None -> Name_occurrences.empty
      | Some set -> Set_of_closures.free_names set
    in
    Code_id.Map.fold
      (fun code_id { params_and_body; newer_version_of; } free_names ->
        let from_newer_version_of =
          match newer_version_of with
          | None -> Name_occurrences.empty
          | Some older ->
            Name_occurrences.add_newer_version_of_code_id
              Name_occurrences.empty older Name_mode.normal
        in
        let from_params_and_body =
          match params_and_body with
          | Deleted -> Name_occurrences.empty
          | Present params_and_body ->
            Function_params_and_body.free_names params_and_body
        in
        Name_occurrences.union_list [
          (Name_occurrences.add_code_id Name_occurrences.empty
            code_id Name_mode.normal);
          from_params_and_body;
          from_newer_version_of;
          free_names;
        ])
      code
      from_set_of_closures
  | Boxed_float (Var v)
  | Boxed_int32 (Var v)
  | Boxed_int64 (Var v)
  | Boxed_nativeint (Var v) ->
    Name_occurrences.singleton_variable v Name_mode.normal
  | Boxed_float (Const _)
  | Boxed_int32 (Const _)
  | Boxed_int64 (Const _)
  | Boxed_nativeint (Const _)
  | Mutable_string { initial_value = _; }
  | Immutable_string _ -> Name_occurrences.empty
  | Immutable_float_array fields ->
    List.fold_left (fun fns (field : _ or_variable) ->
        match field with
        | Var v ->
          Name_occurrences.add_variable fns v Name_mode.normal
        | Const _ -> fns)
      (Name_occurrences.empty)
      fields

let print_params_and_body_with_cache ~cache ppf params_and_body =
  match params_and_body with
  | Deleted -> Format.fprintf ppf "@[<hov 1>(params_and_body@ Deleted)@]"
  | Present params_and_body ->
    Function_params_and_body.print_with_cache ~cache ppf
      params_and_body

let print_code_with_cache ~cache ppf { params_and_body; newer_version_of; } =
  (* CR mshinwell: elide "newer_version_of" when None *)
  Format.fprintf ppf "@[<hov 1>(\
      @[(newer_version_of@ %a)@]@ \
      %a\
      )@]"
    (Misc.Stdlib.Option.print Code_id.print) newer_version_of
    (print_params_and_body_with_cache ~cache) params_and_body

let print_with_cache ~cache ppf t =
  let print_float_array_field ppf = function
    | Const f -> fprintf ppf "%a" Numbers.Float_by_bit_pattern.print f
    | Var v -> Variable.print ppf v
  in
  match t with
  | Block (tag, mut, fields) ->
    fprintf ppf "@[<hov 1>(@<0>%s%sblock@<0>%s (tag %a) (%a))@]"
      (Flambda_colours.static_part ())
      (match mut with Immutable -> "Immutable_" | Mutable -> "Mutable_")
      (Flambda_colours.normal ())
      Tag.Scannable.print tag
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Field_of_block.print) fields
  | Code_and_set_of_closures { code; set_of_closures; } ->
    fprintf ppf "@[<hov 1>(@<0>%sCode_and_set_of_closures@<0>%s@ (\
        @[<hov 1>(code@ (%a))@]@ \
        @[<hov 1>(set_of_closures@ (%a))@]\
        ))@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Code_id.Map.print (print_code_with_cache ~cache)) code
      (Misc.Stdlib.Option.print
        (Set_of_closures.print_with_cache ~cache))
        set_of_closures
  | Boxed_float (Const f) ->
    fprintf ppf "@[@<0>%sBoxed_float@<0>%s %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Numbers.Float_by_bit_pattern.print f
  | Boxed_float (Var v) ->
    fprintf ppf "@[@<0>%sBoxed_float@<0>%s %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Variable.print v
  | Boxed_int32 (Const n) ->
    fprintf ppf "@[@<0>%sBoxed_int32@<0>%s %ld)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      n
  | Boxed_int32 (Var v) ->
    fprintf ppf "@[@<0>%sBoxed_int32@<0>%s %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Variable.print v
  | Boxed_int64 (Const n) ->
    fprintf ppf "@[@<0>%sBoxed_int64@<0>%s %Ld)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      n
  | Boxed_int64 (Var v) ->
    fprintf ppf "@[@<0>%sBoxed_int64@<0>%s %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Variable.print v
  | Boxed_nativeint (Const n) ->
    fprintf ppf "@[@<0>%sBoxed_nativeint@<0>%s %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Targetint.print n
  | Boxed_nativeint (Var v) ->
    fprintf ppf "@[@<0>%sBoxed_nativeint@<0>%s %a)@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      Variable.print v
  | Immutable_float_array fields ->
    fprintf ppf "@[@<0>%sImmutable_float_array@<0>%s@ @[[| %a |]@])@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "@; ")
         print_float_array_field)
      fields
  | Mutable_string { initial_value = s; } ->
    fprintf ppf "@[@<0>%sMutable_string@<0>%s@ \"%s\")@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      s
  | Immutable_string s ->
    fprintf ppf "@[@<0>%sImmutable_string@<0>%s@ \"%s\")@]"
      (Flambda_colours.static_part ())
      (Flambda_colours.normal ())
      s

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

(*
let _invariant env t =
  try
    let module E = Invariant_env in
    match t with
    | Block (_tag, _mut, fields) ->
      List.iter (fun field -> Field_of_block.invariant env field) fields
    | Set_of_closures set ->
      Set_of_closures.invariant env set
    | Boxed_float (Var v) ->
      E.check_variable_is_bound_and_of_kind env v K.naked_float
    | Boxed_int32 (Var v) ->
      E.check_variable_is_bound_and_of_kind env v K.naked_int32
    | Boxed_int64 (Var v) ->
      E.check_variable_is_bound_and_of_kind env v K.naked_int64
    | Boxed_nativeint (Var v) ->
      E.check_variable_is_bound_and_of_kind env v K.naked_nativeint
    | Mutable_string { initial_value = Var v; }
    | Immutable_string (Var v) ->
      E.check_variable_is_bound_and_of_kind env v K.value
    | Boxed_float (Const _)
    | Boxed_int32 (Const _)
    | Boxed_int64 (Const _)
    | Boxed_nativeint (Const _)
    | Mutable_string { initial_value = Const _; }
    | Immutable_string (Const _) -> ()
    | Immutable_float_array fields ->
      List.iter (fun (field : _ or_variable) ->
          match field with
          | Var v ->
            E.check_variable_is_bound_and_of_kind env v
              K.naked_float
          | Const _ -> ())
        fields
  with Misc.Fatal_error ->
    Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t
*)

let apply_name_permutation t perm =
  if Name_permutation.is_empty perm then t
  else
    match t with
    | Block (tag, mut, fields) ->
      let changed = ref false in
      let fields =
        List.map (fun field ->
            let field' = Field_of_block.apply_name_permutation field perm in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Block (tag, mut, fields)
    | Code_and_set_of_closures { code; set_of_closures; } ->
      let code' =
        Code_id.Map.map_sharing
          (fun ({ params_and_body; newer_version_of; } as code) ->
            let params_and_body' =
              match params_and_body with
              | Deleted -> Deleted
              | Present params_and_body_inner ->
                let params_and_body_inner' =
                  Function_params_and_body.apply_name_permutation
                    params_and_body_inner perm
                in
                if params_and_body_inner == params_and_body_inner' then 
                  params_and_body
                else
                  Present params_and_body_inner'
            in
            if params_and_body == params_and_body' then code
            else
              { params_and_body = params_and_body';
                newer_version_of;
              })
          code
      in
      let set_of_closures' =
        match set_of_closures with
        | None -> None
        | Some set ->
          let set' =
            Set_of_closures.apply_name_permutation set perm
          in
          if set == set' then set_of_closures
          else Some set'
      in
      if code == code' && set_of_closures == set_of_closures' then t
      else
        Code_and_set_of_closures {
          code = code';
          set_of_closures = set_of_closures';
        }
    | Boxed_float (Var v) ->
      let v' = Name_permutation.apply_variable perm v in
      if v == v' then t
      else Boxed_float (Var v')
    | Boxed_int32 (Var v) ->
      let v' = Name_permutation.apply_variable perm v in
      if v == v' then t
      else Boxed_int32 (Var v')
    | Boxed_int64 (Var v) ->
      let v' = Name_permutation.apply_variable perm v in
      if v == v' then t
      else Boxed_int64 (Var v')
    | Boxed_nativeint (Var v) ->
      let v' = Name_permutation.apply_variable perm v in
      if v == v' then t
      else Boxed_nativeint (Var v')
    | Boxed_float (Const _)
    | Boxed_int32 (Const _)
    | Boxed_int64 (Const _)
    | Boxed_nativeint (Const _)
    | Mutable_string { initial_value = _; }
    | Immutable_string _ -> t
    | Immutable_float_array fields ->
      let changed = ref false in
      let fields =
        List.map (fun (field : _ or_variable) ->
            let field' : _ or_variable =
              match field with
              | Var v -> Var (Name_permutation.apply_variable perm v)
              | Const _ -> field
            in
            if not (field == field') then begin
              changed := true
            end;
            field')
          fields
      in
      if not !changed then t
      else Immutable_float_array fields

let get_pieces_of_code t =
  match t with
  | Code_and_set_of_closures { code; set_of_closures = _; } ->
    Code_id.Map.filter_map code
      ~f:(fun _code_id { params_and_body; newer_version_of; } ->
        match params_and_body with
        | Present params_and_body -> Some (params_and_body, newer_version_of)
        | Deleted -> None)
  | Block _
  | Boxed_float _
  | Boxed_int32 _
  | Boxed_int64 _
  | Boxed_nativeint _
  | Immutable_float_array _
  | Mutable_string _
  | Immutable_string _ -> Code_id.Map.empty

let is_fully_static t =
  free_names t
  |> Name_occurrences.variables
  |> Variable.Set.is_empty
