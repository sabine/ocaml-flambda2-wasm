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
 * Throughout the implementation we use consistent naming conventions for
 * syntactic elements, associated with the types defined here and in a few
 * other places:
 *
 *   x : var
 *   v : value
 *   e : instrr
 *   f : func
 *   m : module_
 *
 *   t : value_type
 *   s : func_type
 *   c : context / config
 *
 * These conventions mostly follow standard practice in language semantics.
 *)

(* TODO: introduce symbol variable and use it instead! *)


module Types = Wasm_types
module Values = Wasm_values

open Types

(* Operators *)

module IntOp =
struct
  type unop = Clz | Ctz | Popcnt
  type binop = Add | Sub | Mul | DivS | DivU | RemS | RemU
             | And | Or | Xor | Shl | ShrS | ShrU | Rotl | Rotr
  type testop = Eqz
  type relop = Eq | Ne | LtS | LtU | GtS | GtU | LeS | LeU | GeS | GeU
  type cvtop = ExtendSI32 | ExtendUI32 | WrapI64
             | TruncSF32 | TruncUF32 | TruncSF64 | TruncUF64
             | ReinterpretFloat
end

module FloatOp =
struct
  type unop = Neg | Abs | Ceil | Floor | Trunc | Nearest | Sqrt
  type binop = Add | Sub | Mul | Div | Min | Max | CopySign
  type testop
  type relop = Eq | Ne | Lt | Gt | Le | Ge
  type cvtop = ConvertSI32 | ConvertUI32 | ConvertSI64 | ConvertUI64
             | PromoteF32 | DemoteF64
             | ReinterpretInt
end

module I32Op = IntOp
module I64Op = IntOp
module F32Op = FloatOp
module F64Op = FloatOp

type unop = (I32Op.unop, I64Op.unop, F32Op.unop, F64Op.unop) Values.op
type binop = (I32Op.binop, I64Op.binop, F32Op.binop, F64Op.binop) Values.op
type testop = (I32Op.testop, I64Op.testop, F32Op.testop, F64Op.testop) Values.op
type relop = (I32Op.relop, I64Op.relop, F32Op.relop, F64Op.relop) Values.op
type cvtop = (I32Op.cvtop, I64Op.cvtop, F32Op.cvtop, F64Op.cvtop) Values.op

type mem_size = Mem8 | Mem16 | Mem32
type extension = SX | ZX

type 'a memop =
  {ty : num_type; align : int; offset : int32; sz : 'a option}
type loadop = (mem_size * extension) memop
type storeop = mem_size memop


(* Expressions *)

type literal = Values.value
type name = string

type instr =
  | Unreachable                       (* trap unconditionally *)
  | Nop                               (* do nothing *)
  | Block of stack_type * instr list  (* execute in sequence *)
  | Loop of stack_type * instr list   (* loop header *)
  | If of stack_type * instr list * instr list  (* conditional *)
  | Br of labelidx                         (* break to n-th surrounding label *)
  | BrIf of labelidx                       (* conditional break *)
  | BrTable of labelidx list * labelidx         (* indexed break *)
  | Return                            (* break from function body *)
  | Call of funcidx                      (* call function *)
  | Link of string                    (* used to link later on *)
  | CallIndirect of typeidx               (* call function through table *)
  | Drop                              (* forget a value *)
  | Select                            (* branchless conditional *)
  | GetLocal of localidx                   (* read local variable *)
  | SetLocal of localidx                   (* write local variable *)
  | TeeLocal of localidx                   (* write local variable and keep value *)
  | GetGlobal of globalidx                  (* read global variable *)
  | SetGlobal of globalidx                  (* write global variable *)
  | Load of loadop                    (* read memory at address *)
  | Store of storeop                  (* write memory at address *)
  | CurrentMemory                     (* size of linear memory *)
  | GrowMemory                        (* grow linear memory *)
  | Const of literal                  (* constant *)
  | DelayedConst of string            (* a constant that is resolved at a later point *)
  | Test of testop                    (* numeric test *)
  | Compare of relop                  (* numeric comparison *)
  | Unary of unop                     (* unary numeric operator *)
  | Binary of binop                   (* binary numeric operator *)
  | Convert of cvtop                  (* conversion *)

  (*GC reference types proposal *)
  (*GC*)| RefNull
  (*GC*)| RefIsNull
  (*GC*)| RefFunc of funcidx
  (*GC*)| RefEq

  (*GC struct types *)
  (*GC*)| StructNew of typeidx
  (*GC*)| StructGet of typeidx * fieldidx
  (*GC*)| StructSet of typeidx * fieldidx
  (*GC*)| ArrayNew of typeidx
  (*GC*)| ArrayGet of typeidx
  (*GC*)| ArraySet of typeidx
  (*GC*)| ArrayLen of typeidx
  

(* Globals & Functions *)

type const = instr list

type global =
{
  name : string;
  gtype : global_type;
  value : const;
}

type func =
{
  name: string;
  ftype : typeidx;
  locals : value_type list;
  body : instr list;
}


(* Tables & Memories *)

type table =
{
  ttype : table_type;
}

type memory =
{
  mtype : memory_type;
}

type 'data segment =
{
  index : idx;
  offset : const;
  init : 'data;
}

type table_segment = funcidx list segment
type memory_segment = string segment


(* Modules *)

type type_ =
  | TypeFunc of func_type
  | TypeStruct of struct_type
  | TypeArray of array_type

type export_desc =
  | FuncExport of funcidx
  | TableExport of tableidx
  | MemoryExport of memidx
  | GlobalExport of globalidx

type export =
{
  name : name;
  edesc : export_desc;
}

type import_desc =
  | FuncImport of typeidx
  | TableImport of table_type
  | MemoryImport of memory_type
  | GlobalImport of global_type

type import =
{
  module_name : name;
  item_name : name;
  idesc : import_desc;
}

type data_part_detail =
| String of string
| Int32 of int32
| Nativeint of nativeint
| Int16 of int
| Int8 of int
| Float32 of F32.t

type data_part = {
  name: string;
  detail: data_part_detail list
}


type module_ =
{
  types : type_ list;
  globals : global list;
  tables : table list;
  memories : memory list;
  funcs : func list;
  start : funcidx option;
  elems : table_segment list;
  data : data_part segment list;
  imports : import list;
  exports : export list;
}


(* Auxiliary functions *)

let empty_module =
{
  types = [];
  globals = [];
  tables = [];
  memories = [];
  funcs = [];
  start = None;
  elems  = [];
  data = [];
  imports = [];
  exports = [];
}

let func_type_for (m : module_) (x : idx) : func_type =
  match (Lib.List32.nth m.types x.index) with
    | TypeFunc f -> f
    | TypeStruct _ -> failwith (I32.to_string_u x.index ^ " is a struct type, not a func type!")
    | TypeArray _ -> failwith (I32.to_string_u x.index ^ " is an array type, not a func type!")

let import_type (m : module_) (im : import) : extern_type =
  let {idesc; _} = im in
  match idesc with
  | FuncImport x -> ExternFuncType (func_type_for m x)
  | TableImport t -> ExternTableType t
  | MemoryImport t -> ExternMemoryType t
  | GlobalImport t -> ExternGlobalType t

let export_type (m : module_) (ex : export) : extern_type =
  let {edesc; _} = ex in
  let its = List.map (import_type m) m.imports in
  let open Lib.List32 in
  match edesc with
  | FuncExport x ->
    let fts =
      funcs its @ List.map (fun f -> func_type_for m f.ftype) m.funcs
    in ExternFuncType (nth fts x.index)
  | TableExport x ->
    let tts = tables its @ List.map (fun t -> t.ttype) m.tables in
    ExternTableType (nth tts x.index)
  | MemoryExport x ->
    let mts = memories its @ List.map (fun m -> m.mtype) m.memories in
    ExternMemoryType (nth mts x.index)
  | GlobalExport x ->
    let gts = globals its @ List.map (fun g -> g.gtype) m.globals in
    ExternGlobalType (nth gts x.index)

let string_of_name n =
  let b = Buffer.create 16 in
  let escape uc =
    if uc < 0x20 || uc >= 0x7f then
      Buffer.add_string b (Printf.sprintf "\\u{%02x}" uc)
    else begin
      let c = Char.chr uc in
      if c = '\"' || c = '\\' then Buffer.add_char b '\\';
      Buffer.add_char b c
    end
  in
  List.iter escape n;
  Buffer.contents b
