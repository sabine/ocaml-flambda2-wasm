(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   original code by Andreas Rossberg from WebAssembly/spec/interpreter  *)
(*   adapted for use in the OCaml compiler by Sander Spies                *)
(*   modified slightly by Sabine Schmaltz, Tarides                        *)
(*                                                                        *)
(*   Licensed under the Apache License, Version 2.0 (the "License");      *)
(*   you may not use this file except in compliance with the License.     *)
(*   You may obtain a copy of the License at                              *)
(*     https://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(**************************************************************************)

module Arrange = Wasm_arrange
module Sexpr = Wasm_sexpr


let instr oc width e = Sexpr.output oc width (Arrange.instr e)
let func oc width f = Sexpr.output oc width (Arrange.func f)
let module_ oc width m = Sexpr.output oc width (Arrange.module_ m)
