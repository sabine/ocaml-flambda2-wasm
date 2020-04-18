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

module Types = Wasm_types 

open Types

(* Values and operators *)

type ('i32, 'i64, 'f32, 'f64) op =
  I32 of 'i32
  | I64 of 'i64
  | F32 of 'f32
  | F64 of 'f64

(* TODO: GC packed values I8 and I16 *)
type value = (I32.t, I64.t, F32.t, F64.t) op

(* Typing *)

let type_of = function
  | I32 _ -> I32Type
  | I64 _ -> I64Type
  | F32 _ -> F32Type
  | F64 _ -> F64Type

let default_value = function
  | I32Type -> I32 I32.zero
  | I64Type -> I64 I64.zero
  | F32Type -> F32 F32.zero
  | F64Type -> F64 F64.zero


(* Conversion *)

let value_of_bool b = I32 (if b then 1l else 0l)

(* Injection & projection *)

exception Value of value_type

module type ValueType =
sig
  type t
  val to_value : t -> value
  val of_value : value -> t (* raise Value *)
end

module I32Value =
struct
  type t = I32.t
  let to_value i = I32 i
  let of_value = function I32 i -> i | _ -> raise (Value (NumValueType I32Type))
end

module I64Value =
struct
  type t = I64.t
  let to_value i = I64 i
  let of_value = function I64 i -> i | _ -> raise (Value (NumValueType I64Type))
end

module F32Value =
struct
  type t = F32.t
  let to_value i = F32 i
  let of_value = function F32 z -> z | _ -> raise (Value (NumValueType F32Type))
end

module F64Value =
struct
  type t = F64.t
  let to_value i = F64 i
  let of_value = function F64 z -> z | _ -> raise (Value (NumValueType F64Type))
end
