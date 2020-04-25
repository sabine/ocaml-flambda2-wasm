(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   original code by Andreas Rossberg from WebAssembly/spec/interpreter  *)
(*   adapted for use in the OCaml compiler by Sander Spies                *)
(*   modified by Sabine Schmaltz, Tarides                                 *)
(*                                                                        *)
(*   Licensed under the Apache License, Version 2.0 (the "License");      *)
(*   you may not use this file except in compliance with the License.     *)
(*   You may obtain a copy of the License at                              *)
(*     https://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(**************************************************************************)

module Lib = Wasm_lib
module I32 = Wasm_i32
module I64 = Wasm_i64
module F32 = Wasm_f32
module F64 = Wasm_f64

(* everything marked with GC comes from https://docs.google.com/document/d/1DklC3qVuOdLHSXB5UXghM_syCh-4cMinQ50ICiXnK3Q/edit# *)

(* Types *)
type idx = { index: int32; name: string option}
type typeidx = idx
(*GC*)type fieldidx = idx
type funcidx = idx
type tableidx = idx
type memidx = idx
type globalidx = idx
type localidx = idx
type labelidx = idx

type mutability = Immutable | Mutable

type num_type = I32Type | I64Type | F32Type | F64Type
(*GC*)type ref_type = 
  | AnyRef
  | FuncRef
  | NullRef
  | OptRef of typeidx
  | I31Ref
  | EqRef
  | RttRef of typeidx
(*GC*)type packed_type = I8Type | I16Type
type value_type = NumValueType of num_type | (*GC*)RefValueType of ref_type
(*GC*)type storage_type = StorageTypeValue of value_type | StorageTypePacked of packed_type
(*GC*)type field_type = FieldType of mutability * storage_type

type elem_type = AnyFuncType
type stack_type = value_type list

type func_type = FuncType of { name: string option; t: stack_type * stack_type }
(*GC*)type struct_type = StructType of { name: string option; t: field_type list }
(*GC*)type array_type = ArrayType of { name: string option; t: field_type }

type deftype =
  | TypeFunc of func_type
  (*GC*)| TypeStruct of struct_type
  (*GC*)| TypeArray of array_type


type 'a limits = {min : 'a; max : 'a option}
type table_type = TableType of Int32.t limits * elem_type
type memory_type = MemoryType of Int32.t limits
type global_type = GlobalType of value_type * mutability
type extern_type =
  | ExternFuncType of func_type
  | ExternTableType of table_type
  | ExternMemoryType of memory_type
  | ExternGlobalType of global_type


(* Attributes *)

let size = function
  | I32Type | F32Type -> 4
  | I64Type | F64Type -> 8


(* Subtyping *)

let match_limits lim1 lim2 =
  I32.ge_u lim1.min lim2.min &&
  match lim1.max, lim2.max with
  | _, None -> true
  | None, Some _ -> false
  | Some i, Some j -> I32.le_u i j

let match_func_type ft1 ft2 =
  ft1 = ft2

let match_table_type (TableType (lim1, et1)) (TableType (lim2, et2)) =
  et1 = et2 && match_limits lim1 lim2

let match_memory_type (MemoryType lim1) (MemoryType lim2) =
  match_limits lim1 lim2

let match_global_type gt1 gt2 =
  gt1 = gt2

let match_extern_type et1 et2 =
  match et1, et2 with
  | ExternFuncType ft1, ExternFuncType ft2 -> match_func_type ft1 ft2
  | ExternTableType tt1, ExternTableType tt2 -> match_table_type tt1 tt2
  | ExternMemoryType mt1, ExternMemoryType mt2 -> match_memory_type mt1 mt2
  | ExternGlobalType gt1, ExternGlobalType gt2 -> match_global_type gt1 gt2
  | _, _ -> false


(* Filters *)

let funcs =
  Lib.List.map_filter (function ExternFuncType t -> Some t | _ -> None)
let tables =
  Lib.List.map_filter (function ExternTableType t -> Some t | _ -> None)
let memories =
  Lib.List.map_filter (function ExternMemoryType t -> Some t | _ -> None)
let globals =
  Lib.List.map_filter (function ExternGlobalType t -> Some t | _ -> None)
