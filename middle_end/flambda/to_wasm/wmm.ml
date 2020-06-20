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

module Wasm_types = Wasm.Wasm_types

type wasmtype_component =
  | Val
  | Anyref
  | I32
  | F64

type wasmtype = wasmtype_component array

let typ_void = ([||] : wasmtype)
let typ_val = [|Val|]
let typ_anyref = [|Anyref|]
let typ_i32 = [|I32|]
let typ_f64 = [|F64|]

type integer_comparison = Lambda.integer_comparison =
  | Ceq | Cne | Clt | Cgt | Cle | Cge

let negate_integer_comparison = Lambda.negate_integer_comparison

let swap_integer_comparison = Lambda.swap_integer_comparison

(* With floats [not (x < y)] is not the same as [x >= y] due to NaNs,
   so we provide additional comparisons to represent the negations.*)
type float_comparison = Lambda.float_comparison =
  | CFeq | CFneq | CFlt | CFnlt | CFgt | CFngt | CFle | CFnle | CFge | CFnge

let negate_float_comparison = Lambda.negate_float_comparison

let swap_float_comparison = Lambda.swap_float_comparison



(* TODO: try to keep a Flambda 2.0-style control flow where inlining has already been performed *)
type label = int

let label_counter = ref 99

type exit_label =
  | Return_lbl
  | Lbl of label

let new_label() = incr label_counter; !label_counter

type trywith_shared_label = int

type trap_action =
  | Push of trywith_shared_label
  | Pop

type trywith_kind =
  | Regular
  | Delayed of trywith_shared_label




type targetint = Wasm_types.I64.t

type rec_flag = Nonrecursive | Recursive

type phantom_defining_expr =
  | Wphantom_const_int of targetint
  | Wphantom_const_symbol of string
  | Wphantom_var of Backend_var.t
  | Wphantom_offset_var of { var : Backend_var.t; offset_in_targetints : int; }
  | Wphantom_read_field of { var : Backend_var.t; field : int; }
  | Wphantom_read_symbol_field of { sym : string; field : int; }
  | Wphantom_block of { tag : int; fields : Backend_var.t list; }

type memory_chunk =
    Byte_unsigned
  | Byte_signed
  | Sixteen_unsigned
  | Sixteen_signed
  | Thirtytwo_unsigned
  | Thirtytwo_signed
  | Word_int
  | Word_val
  | Single
  | Double
  | Double_u

and operation =
    Wapply of wasmtype
  | Wextcall of string * wasmtype * bool
  | Wload of memory_chunk * Wasm_types.mutability
  | Walloc
  | Wstore of memory_chunk * Lambda.initialization_or_assignment
  | Waddi | Wsubi | Wmuli | Wmulhi | Wdivi | Wmodi
  | Wand | Wor | Wxor | Wlsl | Wlsr | Wasr
  | Wcmpi of integer_comparison
  | Waddv | Wadda
  | Wcmpa of integer_comparison
  | Wnegf | Wabsf
  | Waddf | Wsubf | Wmulf | Wdivf
  | Wfloatofint | Wintoffloat
  | Wcmpf of float_comparison
  | Wraise of Lambda.raise_kind
  | Wcheckbound

type expression =
    Wconst_int of Wasm_types.I64.t * Debuginfo.t
  | Wconst_float of Wasm_types.F64.t * Debuginfo.t
  | Wconst_symbol of string * Debuginfo.t
  | Wblockheader of Wasm_types.I64.t * Debuginfo.t
  | Wvar of Backend_var.t
  | Wlet of Backend_var.With_provenance.t * expression * expression
  | Wlet_mut of Backend_var.With_provenance.t * wasmtype
                * expression * expression
  | Wphantom_let of Backend_var.With_provenance.t
      * phantom_defining_expr option * expression
  | Wassign of Backend_var.t * expression
  | Wtuple of expression list
  | Wop of operation * expression list * Debuginfo.t
  | Wsequence of expression * expression
  | Wifthenelse of expression * Debuginfo.t * expression
      * Debuginfo.t * expression * Debuginfo.t
  | Wswitch of expression * Wasm_types.I32.t array * (expression * Debuginfo.t) array
      * Debuginfo.t
  | Wcatch of
      rec_flag
        * (label * (Backend_var.With_provenance.t * wasmtype) list
          * expression * Debuginfo.t) list
        * expression
  | Wexit of exit_label * expression list * trap_action list
  | Wtrywith of expression * trywith_kind * Backend_var.With_provenance.t
      * expression * Debuginfo.t

type fundecl =
  { fun_name: string;
    fun_args: (Backend_var.With_provenance.t * wasmtype) list;
    fun_body: expression;
    fun_dbg : Debuginfo.t;
  }

type data_item_value =
  | Wnullref (* for uninitialized global variables of type anyref *)
  | Wint8 of Wasm_types.I32.t
  | Wint16 of Wasm_types.I32.t
  | Wint32 of Wasm_types.I32.t
  | Wint64 of Wasm_types.I64.t
  | Wsingle of Wasm_types.F32.t
  | Wdouble of Wasm_types.F64.t
  | Wsymbol_ref of string
  | Wstring of string

type data_item =
    Wdefine_local_symbol of string * data_item_value list
  | Wdefine_global_symbol of string * data_item_value list
  | Wunnamed_data_block of data_item_value list

type phrase =
    Wfunction of fundecl
  | Wdata of data_item list

let wcatch (i, ids, e1, e2, dbg) =
  Wcatch(Nonrecursive, [i, ids, e2, dbg], e1)
