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

(*
 * OCaml lacks 32-bit floats, however we can emulate all the basic operations
 * using 64-bit floats, as described in the paper
 * "When is double rounding innocuous?" by Samuel A. Figueroa.
 *)

include Wasm_float.Make
  (struct
    include Int32
    let pos_nan = 0x7fc00000l
    let neg_nan = 0xffc00000l
    let bare_nan = 0x7f800000l
    let to_hex_string = Printf.sprintf "%lx"
  end)
