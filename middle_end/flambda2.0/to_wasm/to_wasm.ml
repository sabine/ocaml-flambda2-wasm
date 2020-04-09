(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

[@@@ocaml.warning "-32"] (* FIXME Let_code -- just remove this *)

(* 
open! Flambda.Import
*)

open Wasm.Ast
(*open Wasm.Values*)
open Wasm.Types

(*module Env = To_wasm_env*)
module Ece = Effects_and_coeffects
(*module R = To_wasm_result*)


let todo () = failwith "Not yet implemented"


let wasm_module = ref {
  types = [];
  globals = [];
  tables = [{
    ttype = TableType ({min = 0l; max = Some 0l}, AnyFuncType)
  }];
  memories = [{
    (* TODO: this needs to be improved when doing GC *)
    mtype = MemoryType {min = 100l; max = Some 100l}
  }];
  funcs = [];
  start = None;
  elems = [];
  data = [];
  imports = [];
  exports = [];
  symbols = [];
}

let unit _flambda = !wasm_module