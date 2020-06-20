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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Contents of middle-end-specific portion of .cmx files when using
    Flambda. *)

module Const = Reg_width_things.Const

type table_data = {
  symbols : Symbol.exported Symbol.Map.t;
  variables : Variable.exported Variable.Map.t;
  simples : Simple.exported Simple.Map.t;
  consts : Const.exported Const.Map.t;
  code_ids : Code_id.exported Code_id.Map.t;
}

type t0 = {
  original_compilation_unit : Compilation_unit.t;
  final_typing_env : Flambda_type.Typing_env.Serializable.t;
  all_code : Flambda.Function_params_and_body.t Code_id.Map.t;
  exported_offsets : Exported_offsets.t;
  table_data : table_data;
}

type t = t0 list

let create ~final_typing_env ~all_code ~exported_offsets =
  let typing_env_exported_ids =
    Flambda_type.Typing_env.Serializable.all_ids_for_export final_typing_env
  in
  let exported_ids =
    Code_id.Map.fold (fun code_id params_and_body ids ->
        let ids_for_params_and_body =
          Flambda.Function_params_and_body.all_ids_for_export params_and_body
        in
        Ids_for_export.add_code_id
          (Ids_for_export.union ids ids_for_params_and_body)
          code_id)
      all_code
      typing_env_exported_ids
  in
  let symbols =
    Symbol.Set.fold (fun symbol symbols ->
        Symbol.Map.add symbol (Symbol.export symbol) symbols)
      exported_ids.symbols
      Symbol.Map.empty
  in
  let variables =
    Variable.Set.fold (fun variable variables ->
        Variable.Map.add variable (Variable.export variable) variables)
      exported_ids.variables
      Variable.Map.empty
  in
  let simples =
    Simple.Set.fold (fun simple simples ->
        Simple.Map.add simple (Simple.export simple) simples)
      exported_ids.simples
      Simple.Map.empty
  in
  let consts =
    Const.Set.fold (fun const consts ->
        Const.Map.add const (Const.export const) consts)
      exported_ids.consts
      Const.Map.empty
  in
  let code_ids =
    Code_id.Set.fold (fun code_id code_ids ->
        Code_id.Map.add code_id (Code_id.export code_id) code_ids)
      exported_ids.code_ids
      Code_id.Map.empty
  in
  let table_data =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
    }
  in
  [{ original_compilation_unit = Compilation_unit.get_current_exn ();
    final_typing_env;
    all_code;
    exported_offsets;
    table_data;
   }]

let import_typing_env_and_code0 t =
  (* First create map for data that does not contain ids, i.e. everything
     except simples *)
  let symbols = Symbol.Map.map Symbol.import t.table_data.symbols in
  let variables = Variable.Map.map Variable.import t.table_data.variables in
  let consts = Const.Map.map Const.import t.table_data.consts in
  let code_ids = Code_id.Map.map Code_id.import t.table_data.code_ids in
  (* Build a simple to simple converter from this *)
  let import_map =
    Ids_for_export.Import_map.create
      ~symbols
      ~variables
      ~simples:Simple.Map.empty
      ~consts
      ~code_ids
  in
  let map_simple = Ids_for_export.Import_map.simple import_map in
  (* Then convert the simples *)
  let simples =
    Simple.Map.map (Simple.import map_simple) t.table_data.simples
  in
  let import_map =
    Ids_for_export.Import_map.create
      ~symbols
      ~variables
      ~simples
      ~consts
      ~code_ids
  in
  let typing_env =
    Flambda_type.Typing_env.Serializable.import import_map t.final_typing_env
  in
  let all_code =
    Code_id.Map.fold (fun code_id params_and_body all_code ->
        let code_id = Ids_for_export.Import_map.code_id import_map code_id in
        let params_and_body =
          Flambda.Function_params_and_body.import import_map params_and_body
        in
        Code_id.Map.add code_id params_and_body all_code)
      t.all_code
      Code_id.Map.empty
  in
  typing_env, all_code

let import_typing_env_and_code t =
  match t with
  | [] -> Misc.fatal_error "Flambda cmx info should never be empty"
  | [ t0 ] -> import_typing_env_and_code0 t0
  | t0 :: rem ->
    List.fold_left (fun (typing_env, code) t0 ->
        let (typing_env0, code0) = import_typing_env_and_code0 t0 in
        let typing_env =
          Flambda_type.Typing_env.Serializable.merge typing_env typing_env0
        in
        let code = Code_id.Map.disjoint_union code code0 in
        typing_env, code)
      (import_typing_env_and_code0 t0)
      rem

let exported_offsets t =
  List.fold_left (fun offsets t0 ->
      Exported_offsets.merge offsets t0.exported_offsets)
    Exported_offsets.empty
    t

let with_exported_offsets t exported_offsets =
  match t with
  | [ t0 ] ->  [{ t0 with exported_offsets; }]
  | [] | _ :: _ :: _ ->
    Misc.fatal_error "Cannot set exported offsets on multiple units"

let update_for_pack0 ~pack_units ~pack t =
  let update_cu unit =
    if Compilation_unit.Set.mem unit pack_units
    then pack
    else unit
  in
  let symbols =
    Symbol.Map.map (Symbol.map_compilation_unit update_cu)
      t.table_data.symbols
  in
  let variables =
    Variable.Map.map (Variable.map_compilation_unit update_cu)
      t.table_data.variables
  in
  let simples =
    Simple.Map.map (Simple.map_compilation_unit update_cu)
      t.table_data.simples
  in
  let consts =
    Const.Map.map (Const.map_compilation_unit update_cu)
      t.table_data.consts
  in
  let code_ids =
    Code_id.Map.map (Code_id.map_compilation_unit update_cu)
      t.table_data.code_ids
  in
  let table_data =
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
    }
  in
  { t with table_data; }

let update_for_pack ~pack_units ~pack t_opt =
  match t_opt with
  | None -> None
  | Some t -> Some (List.map (update_for_pack0 ~pack_units ~pack) t)

let merge t1_opt t2_opt =
  match t1_opt, t2_opt with
  | None, None -> None
  | Some _, None
  | None, Some _ ->
    (* CR vlaviron: turn this into a proper user error *)
    Misc.fatal_error "Some pack units do not have their export info set.\n\
      Flambda doesn't support packing opaque and normal units together."
  | Some t1, Some t2 -> Some (t1 @ t2)

let print0 ppf t =
  Format.fprintf ppf "@[<hov>Original unit:@ %a@]@;"
    Compilation_unit.print t.original_compilation_unit;
  Compilation_unit.set_current t.original_compilation_unit;
  let typing_env, code =
    import_typing_env_and_code0 t
  in
  Format.fprintf ppf "@[<hov>Typing env:@ %a@]@;"
    Flambda_type.Typing_env.Serializable.print typing_env;
  Format.fprintf ppf "@[<hov>Code:@ %a@]@;"
    (Code_id.Map.print Flambda.Function_params_and_body.print) code;
  Format.fprintf ppf "@[<hov>Offsets:@ %a@]@;"
    Exported_offsets.print t.exported_offsets

let print ppf t =
  let rec print_rest ppf = function
    | [] -> ()
    | t0 :: t ->
      Format.fprintf ppf "@ (%a)"
        print0 t0;
      print_rest ppf t
  in
  match t with
  | [] -> assert false
  | [ t0 ] -> print0 ppf t0
  | t0 :: t ->
    Format.fprintf ppf "Packed units:@ @[<v>(%a)%a@]"
      print0 t0 print_rest t
