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


let wasm_module = ref {
  types = [];
  globals = [];
  tables = [{
    ttype = TableType ({min = 0l; max = Some 0l}, AnyFuncType)
  }];
  memories = [{
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

let unit (flambda : Flambda_unit.t) = !wasm_module