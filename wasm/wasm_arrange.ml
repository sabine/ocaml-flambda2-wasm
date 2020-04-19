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
[@@@ocaml.warning "-27"]

module Ast = Wasm_ast
open Ast
open Wasm_script
open Wasm_values
open Wasm_types
open Wasm_sexpr
module Utf8 = Wasm_utf8

(* Generic formatting *)

let nat n = I32.to_string_u (I32.of_int_u n)
let nat32 = I32.to_string_u

let add_hex_char buf c = Printf.bprintf buf "\\%02x" (Char.code c)
let add_char buf = function
  | '\n' -> Buffer.add_string buf "\\n"
  | '\t' -> Buffer.add_string buf "\\t"
  | '\"' -> Buffer.add_string buf "\\\""
  | '\\' -> Buffer.add_string buf "\\\\"
  | c when '\x20' <= c && c < '\x7f' -> Buffer.add_char buf c
  | c -> add_hex_char buf c
let add_unicode_char buf = function
  | (0x09 | 0x0a) as uc -> add_char buf (Char.chr uc)
  | uc when 0x20 <= uc && uc < 0x7f -> add_char buf (Char.chr uc)
  | uc -> Printf.bprintf buf "\\u{%02x}" uc

let string_with iter add_char s =
  let buf = Buffer.create 256 in
  Buffer.add_char buf '\"';
  iter (add_char buf) s;
  Buffer.add_char buf '\"';
  Buffer.contents buf

let bytes = string_with String.iter add_hex_char
let string = string_with String.iter add_char
let name = string

let list_of_opt = function None -> [] | Some x -> [x]

let list f xs = List.map f xs
let listi f xs = List.mapi f xs
let opt f xo = list f (list_of_opt xo)

let tab head f xs = if xs = [] then [] else [Node (head, list f xs)]
let atom f x = Atom (f x)

let break_bytes s =
  let ss = Lib.String.breakup s 16 in
  list (atom bytes) ss

let break_string s =
  let ss, s' = Lib.List.split_last (Lib.String.split s '\n') in
  list (atom string) (List.map (fun s -> s ^ "\n") ss @ [s'])


(* Types *)
let var (x : idx) = match x.name with
  | None -> nat32 x.index
  | Some n -> "$" ^ n

let num_value_type = function 
  | I32Type -> "i32"
  | I64Type -> "i64"
  | F32Type -> "f32"
  | F64Type -> "f64"

let cons_type = function
  | Func -> "func"
  | Any -> "any"
  | Null -> "null"
  | Opt (typeidx) -> "opt? " ^ var typeidx
  | I31 -> "i31"
  | Eq -> "eq"
  | Rtt (typeidx) -> "rtt " ^ var typeidx

let ref_value_type = function
  | Ref (ct) -> "ref " ^ cons_type ct
  | AnyRef -> "anyref"
  | NullRef -> "nullref"
  | OptRef (typeidx) -> "optref " ^ var typeidx

let value_type = function
  | NumValueType n -> num_value_type n
  | RefValueType r -> ref_value_type r

let value_types = function
  | [t] -> value_type t
  | ts -> "[" ^ String.concat " " (List.map value_type ts) ^ "]"

let elem_type = function
  | AnyFuncType -> "anyfunc"

let limits {min; max} =
  I32.to_string_u min ^
  (match max with None -> "" | Some n -> " " ^ I32.to_string_u n)

let memory_type = function
  | MemoryType lim -> limits lim

let table_type = function
  | TableType (lim, t) -> limits lim ^ " " ^ elem_type t

(* data type declarations *)
let packed_type = function
  | I8Type -> "i8"
  | I16Type -> "i16"

let storage_type = function
  | StorageTypeValue valt -> value_type valt
  | StorageTypePacked packt -> packed_type packt

let field_type = function
  | FieldType (Immutable, st) -> storage_type st
  | FieldType (Mutable, st) -> "(mut " ^ storage_type st ^ ")"

let field_decls kind ts = tab kind (atom field_type) ts

let struct_type (StructType st) =
  Node ("struct", field_decls "field" st.t)

let array_type (ArrayType ft) =
    Node ("array", [Atom (field_type ft.t)])

(* function type decls *)
let decls kind ts = tab kind (atom value_type) ts

let stack_type ts = decls "result" ts

let func_type (FuncType ft) =
  let (ins, out) = ft.t in
  Node ("func", decls "param" ins @ decls "result" out)

let limits nat {min; max} =
  String.concat " " (nat min :: opt nat max)

let global_type = function
  | GlobalType (t, Immutable) -> atom value_type t
  | GlobalType (t, Mutable) -> Node ("mut", [atom value_type t])



(* Operators *)

module IntOp =
struct
  open Ast.IntOp

  let testop xx = function
    | Eqz -> "eqz"

  let relop xx = function
    | Eq -> "eq"
    | Ne -> "ne"
    | LtS -> "lt_s"
    | LtU -> "lt_u"
    | GtS -> "gt_s"
    | GtU -> "gt_u"
    | LeS -> "le_s"
    | LeU -> "le_u"
    | GeS -> "ge_s"
    | GeU -> "ge_u"

  let unop xx = function
    | Clz -> "clz"
    | Ctz -> "ctz"
    | Popcnt -> "popcnt"

  let binop xx = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | DivS -> "div_s"
    | DivU -> "div_u"
    | RemS -> "rem_s"
    | RemU -> "rem_u"
    | And -> "and"
    | Or -> "or"
    | Xor -> "xor"
    | Shl -> "shl"
    | ShrS -> "shr_s"
    | ShrU -> "shr_u"
    | Rotl -> "rotl"
    | Rotr -> "rotr"

  let cvtop xx = function
    | ExtendSI32 -> "extend_s/i32"
    | ExtendUI32 -> "extend_u/i32"
    | WrapI64 -> "wrap/i64"
    | TruncSF32 -> "trunc_s/f32"
    | TruncUF32 -> "trunc_u/f32"
    | TruncSF64 -> "trunc_s/f64"
    | TruncUF64 -> "trunc_u/f64"
    | ReinterpretFloat -> "reinterpret/f" ^ xx
end

module FloatOp =
struct
  open Ast.FloatOp

  let testop xx = fun _ -> assert false

  let relop xx = function
    | Eq -> "eq"
    | Ne -> "ne"
    | Lt -> "lt"
    | Gt -> "gt"
    | Le -> "le"
    | Ge -> "ge"

  let unop xx = function
    | Neg -> "neg"
    | Abs -> "abs"
    | Ceil -> "ceil"
    | Floor -> "floor"
    | Trunc -> "trunc"
    | Nearest -> "nearest"
    | Sqrt -> "sqrt"

  let binop xx = function
    | Add -> "add"
    | Sub -> "sub"
    | Mul -> "mul"
    | Div -> "div"
    | Min -> "min"
    | Max -> "max"
    | CopySign -> "copysign"

  let cvtop xx = function
    | ConvertSI32 -> "convert_s/i32"
    | ConvertUI32 -> "convert_u/i32"
    | ConvertSI64 -> "convert_s/i64"
    | ConvertUI64 -> "convert_u/i64"
    | PromoteF32 -> "promote/f32"
    | DemoteF64 -> "demote/f64"
    | ReinterpretInt -> "reinterpret/i" ^ xx
end

let oper (intop, floatop) op =
  num_value_type (type_of op) ^ "." ^
  (match op with
  | I32 o -> intop "32" o
  | I64 o -> intop "64" o
  | F32 o -> floatop "32" o
  | F64 o -> floatop "64" o
  )

let unop = oper (IntOp.unop, FloatOp.unop)
let binop = oper (IntOp.binop, FloatOp.binop)
let testop = oper (IntOp.testop, FloatOp.testop)
let relop = oper (IntOp.relop, FloatOp.relop)
let cvtop = oper (IntOp.cvtop, FloatOp.cvtop)

let mem_size = function
  | Ast.Mem8 -> "8"
  | Ast.Mem16 -> "16"
  | Ast.Mem32 -> "32"

let extension = function
  | Ast.SX -> "_s"
  | Ast.ZX -> "_u"

let memop name {ty; align; offset; _} =
  num_value_type ty ^ "." ^ name ^
  (if offset = 0l then "" else " offset=" ^ nat32 offset) ^
  (if 1 lsl align = size ty then "" else " align=" ^ nat (1 lsl align))

let loadop op =
  match op.sz with
  | None -> memop "load" op
  | Some (sz, ext) -> memop ("load" ^ mem_size sz ^ extension ext) op

let storeop op =
  match op.sz with
  | None -> memop "store" op
  | Some sz -> memop ("store" ^ mem_size sz) op


(* Values *)

let value = function
  | I32 i -> I32.to_string_s i
  | I64 i -> I64.to_string_s i
  | F32 z -> F32.to_string z
  | F64 z -> F64.to_string z

let values = function
  | [v] -> value v
  | vs -> "[" ^ String.concat " " (List.map value vs) ^ "]"


(* Expressions *)

let constop v = num_value_type (type_of v) ^ ".const"

let rec instr e =
  let head, inner =
    match e with
    | Unreachable -> "unreachable", []
    | Nop -> "nop", []
    | Block (ts, es) -> "block", stack_type ts @ list instr es
    | Loop (ts, es) -> "loop", stack_type ts @ list instr es
    | If (ts, es1, es2) ->
      "if", stack_type ts @
        [Node ("then", list instr es1); Node ("else", list instr es2)]
    | Br x -> "br " ^ var x, []
    | BrIf x -> "br_if " ^ var x, []
    | BrTable (xs, x) ->
      "br_table " ^ String.concat " " (list var (xs @ [x])), []
    | Return -> "return", []
    | Call x -> "call " ^ var x, []
    | CallIndirect x -> "call_indirect " ^ var x, []
    | Drop -> "drop", []
    | Select -> "select", []
    | GetLocal x -> "get_local " ^ var x, []
    | SetLocal x -> "set_local " ^ var x, []
    | TeeLocal x -> "tee_local " ^ var x, []
    | GetGlobal x -> "get_global " ^ var x, []
    | SetGlobal x -> "set_global " ^ var x, []
    | Load op -> loadop op, []
    | Store op -> storeop op, []
    | CurrentMemory -> "current_memory", []
    | GrowMemory -> "grow_memory", []
    | Const lit -> constop lit ^ " " ^ value lit, []
    | DelayedConst s -> print_endline ("Did not resolve:" ^ s); assert false
    | Link s -> print_endline ("Did not link:" ^ s); assert false
    | Test op -> testop op, []
    | Compare op -> relop op, []
    | Unary op -> unop op, []
    | Binary op -> binop op, []
    | Convert op -> cvtop op, []

  (*GC*)| RefNull -> "ref.null", []
  (*GC*)| RefIsNull -> "ref.is_null", []
  (*GC*)| RefFunc funcidx -> "ref.func " ^ var funcidx, []
  (*GC*)| RefEq -> "ref.eq", []

  (*GC*)| StructNew typeidx -> "struct.new " ^ var typeidx, []
  (*GC*)| StructGet (typeidx, fieldidx) -> "struct.get " ^ var typeidx ^ " " ^ var fieldidx, []
  (*GC*)| StructSet (typeidx, fieldidx) -> "struct.set " ^ var typeidx ^ " " ^ var fieldidx, []
  (*GC*)| ArrayNew typeidx -> "array.new " ^ var typeidx, []
  (*GC*)| ArrayGet typeidx -> "array.get " ^ var typeidx, []
  (*GC*)| ArraySet typeidx -> "array.set " ^ var typeidx, []
  (*GC*)| ArrayLen typeidx -> "array.len " ^ var typeidx, []
  in Node (head, inner)

let const c =
  list instr c


(* Functions *)

let name_ n = string (Utf8.encode n);;

let func_with_name n f =
  let {ftype; locals; body; name} = f in
  Node ("func " ^ n,
    [Node ("type " ^ var ftype, [])] @
    decls "local" locals @
    list instr body
  )

let func_with_index off i (f : func) =
  let n = "$" ^ f.name in
  func_with_name n f

let func f =
  func_with_name "" f

let start x = Node ("start " ^ var x, [])


(* Tables & memories *)

let table off i tab =
  let {ttype = TableType (lim, t)} = tab in
  Node ("table $" ^ nat (off + i) ^ " " ^ limits nat32 lim,
    [atom elem_type t]
  )

let memory off i mem =
  let {mtype = MemoryType lim} = mem in
  Node ("memory $" ^ nat (off + i) ^ " " ^ limits nat32 lim, [])

let segment head dat seg =
  let {index; offset; init} = seg in
  Node (head, atom var index :: Node ("offset", const offset) :: dat init)

let elems seg =
  segment "elem" (list (atom var)) seg

let foo (f:data_part) =
  (* let r = List.fold_left (fun acc add -> acc ^ "hi" ) "" f in *)
  []


let data seg =
  segment "data" foo seg

(* Modules *)

let func_typedef i ty =
  let FuncType ft = ty in
  let n = match ft.name with
    | None -> "$" ^ nat i
    | Some name -> "$" ^ name
  in
  Node ("type " ^ n, [func_type ty])

let struct_typedef i ty =
  let StructType st = ty in
  let n = match st.name with
    | None -> "$" ^ nat i
    | Some name -> "$" ^ name
  in
  Node ("type " ^ n, [struct_type ty])

let array_typedef i ty =
  let ArrayType at = ty in
  let n = match at.name with
    | None -> "$" ^ nat i
    | Some name -> "$" ^ name
  in
  Node ("type " ^ n, [array_type ty])

let typedef i = function
  | TypeFunc f -> func_typedef i f
  | TypeStruct s -> struct_typedef i s
  | TypeArray a -> array_typedef i a

let import_desc i d =
  match d with
  | FuncImport x ->
    Node ("func $" ^ nat i, [Node ("type", [atom var x])])
  | TableImport t -> table 0 i {ttype = t}
  | MemoryImport t -> memory 0 i {mtype = t}
  | GlobalImport t -> Node ("global $" ^ nat i, [global_type t])

let import i im =
  let {module_name; item_name; idesc} = im in
  Node ("import",
    [atom name module_name; atom name item_name; import_desc i idesc]
  )

let export_desc d =
  match d with
  | FuncExport x -> Node ("func", [atom var x])
  | TableExport x -> Node ("table", [atom var x])
  | MemoryExport x -> Node ("memory", [atom var x])
  | GlobalExport x -> Node ("global", [atom var x])

let export ex =
  let {name = n; edesc} = ex in
  Node ("export", [atom name n; export_desc edesc])

let global off i g =
  let {gtype; value} = g in
  Node ("global $" ^ nat (off + i), global_type gtype :: const value)


(* Modules *)

let var_opt = function
  | None -> ""
  | Some x -> " " ^ x

let is_func_import im =
  match im.idesc with FuncImport _ -> true | _ -> false
let is_table_import im =
  match im.idesc with TableImport _ -> true | _ -> false
let is_memory_import im =
  match im.idesc with MemoryImport _ -> true | _ -> false
let is_global_import im =
  match im.idesc with GlobalImport _ -> true | _ -> false

let module_with_var_opt x_opt m =
  let func_imports = List.filter is_func_import m.imports in
  let table_imports = List.filter is_table_import m.imports in
  let memory_imports = List.filter is_memory_import m.imports in
  let global_imports = List.filter is_global_import m.imports in
  Node ("module" ^ var_opt x_opt,
    listi typedef m.types @
    listi import table_imports @
    listi import memory_imports @
    listi import global_imports @
    listi import func_imports @
    listi (table (List.length table_imports)) m.tables @
    listi (memory (List.length memory_imports)) m.memories @
    listi (global (List.length global_imports)) m.globals @
    listi (func_with_index (List.length func_imports)) m.funcs @
    list export m.exports @
    opt start m.start @
    list elems m.elems @
    list data m.data
  )

let binary_module_with_var_opt x_opt bs =
  Node ("module" ^ var_opt x_opt ^ " binary", break_bytes bs)

let quoted_module_with_var_opt x_opt s =
  Node ("module" ^ var_opt x_opt ^ " quote", break_string s)

let module_ = module_with_var_opt None


(* Scripts *)

let literal lit =
  match lit with
  | Values.I32 i -> Node ("i32.const " ^ I32.to_string_s i, [])
  | Values.I64 i -> Node ("i64.const " ^ I64.to_string_s i, [])
  | Values.F32 z -> Node ("f32.const " ^ F32.to_string z, [])
  | Values.F64 z -> Node ("f64.const " ^ F64.to_string z, [])
