(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Sabine Schmaltz, Tarides                        *)
(*                                                                        *)
(*   Copyright 2020--2020 Tarides                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* 
open! Flambda.Import
*)

open Wasm.Wasm_ast
open Wasm.Wasm_types

module Ece = Effects_and_coeffects

let todo () = failwith "Not yet implemented"

let unit (_unit : Flambda_unit.t) = 
  Profile.record_call "flambda2_to_wasm" (fun () ->
    (*let (functions, instructions) = expr env (Flambda_unit.body unit) in*)
    let module_init_function_type_name = "_t__module_init" in
    let module_init_function_type = FuncType {
      name = Some module_init_function_type_name;
      t = ([I32Type],[])
    } in
    let module_init_function_type_index = {index = 0l; name = Some module_init_function_type_name} in
    let module_init_function_name = "__module_init" in
    let module_init_function_index = {index = 0l; name = Some module_init_function_name} in
    let module_init_function = {
      name = module_init_function_name;
      ftype = module_init_function_type_index;
      locals = [];
      body = [];
    } in
    let module_init_function_export = {
      name = module_init_function_name;
      edesc = FuncExport module_init_function_index
    } in

    { empty_module with
      imports = [];
      types = [module_init_function_type];
      funcs = [module_init_function];
      start = None;
      exports = [module_init_function_export]
    }
  )