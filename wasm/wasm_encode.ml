(* Version *)

[@@@ocaml.warning "-27"]

module Ast = Wasm_ast
module F32 = Wasm_f32
module F64 = Wasm_f64
module Utf8 = Wasm_utf8
module Lib = Wasm_lib

let version = 1l

open Wasm_values

(* Errors *)

(* module Code = Error.Make () *)
(* exception Code = Code.Error *)


(* Encoding stream *)

type stream =
{
  buf : Buffer.t;
  patches : (int * char) list ref
}

(* let table_relocs:func_reloc list = [] *)

(* let func_relocs:func_reloc list = []
let func_relocs:func_reloc list = [] *)

let stream () = {buf = Buffer.create 8192; patches = ref []}
let pos s = Buffer.length s.buf
let put s b = Buffer.add_char s.buf b
let put_string s bs = Buffer.add_string s.buf bs
let patch s pos b = s.patches := (pos, b) :: !(s.patches)

let to_string s =
  let bs = Buffer.to_bytes s.buf in
  List.iter (fun (pos, b) -> Bytes.set bs pos b) !(s.patches);
  Bytes.to_string bs


(* Encoding *)

type relocation =
  | Func_reloc of int32 * Ast.var
  | Memory_address_reloc of int32 * Ast.var
  | Static_address_reloc of int32
  | Type_reloc of int32 * Ast.var
  | Global_reloc of int32 * Ast.var

let encode m =
  let s = stream () in

  let relocations:relocation list ref = ref [] in

  let module E = struct
    (* Generic values *)

    let u8 i = put s (Char.chr (i land 0xff))
    let u16 i = u8 (i land 0xff); u8 (i lsr 8)
    let u32 i =
      Int32.(u16 (to_int (logand i 0xffffl));
             u16 (to_int (shift_right i 16)))
    let u64 i =
      Int64.(u32 (to_int32 (logand i 0xffffffffL));
             u32 (to_int32 (shift_right i 32)))

    let rec vu64 i =
      let b = Int64.(to_int (logand i 0x7fL)) in
      if 0L <= i && i < 128L then u8 b
      else (u8 (b lor 0x80); vu64 (Int64.shift_right_logical i 7))

    let rec vs64 i =
      let b = Int64.(to_int (logand i 0x7fL)) in
      if -64L <= i && i < 64L then u8 b
      else (u8 (b lor 0x80); vs64 (Int64.shift_right i 7))

    let vu1 i = vu64 Int64.(logand (of_int i) 1L)
    let vu32 i = vu64 Int64.(logand (of_int32 i) 0xffffffffL)
    let vu32p i p =
      let start = pos s in
      vu32 i;
      let end_ = pos s in
      let padding = p - (end_ - start) - 1 in
      for i = 0 to padding do
        ignore(i);
        put s (Char.chr 0x80)
      done;
      put s (Char.chr 0x00)

    let vs7 i = vs64 (Int64.of_int i)
    let vs32 i = vs64 (Int64.of_int32 i)
    let f32 x = u32 (F32.to_bits x)
    let f64 x = u64 (F64.to_bits x)

    let len i =
      if Int32.to_int (Int32.of_int i) <> i then
        failwith
          "cannot encode length with more than 32 bit";
      vu32 (Int32.of_int i)

    let bool b = vu1 (if b then 1 else 0)
    let string bs = len (String.length bs); put_string s bs
    let name n = string (Utf8.encode n)
    let list f xs = List.iter f xs
    let opt f xo = Lib.Option.app f xo
    let vec f xs = len (List.length xs); list f xs

    let gap32 () = let p = pos s in u32 0l; u8 0; p
    let patch_gap32 p n =
      assert (n <= 0x0fff_ffff); (* Strings cannot excess 2G anyway *)
      let lsb i = Char.chr (i land 0xff) in
      patch s p (lsb (n lor 0x80));
      patch s (p + 1) (lsb ((n lsr 7) lor 0x80));
      patch s (p + 2) (lsb ((n lsr 14) lor 0x80));
      patch s (p + 3) (lsb ((n lsr 21) lor 0x80));
      patch s (p + 4) (lsb (n lsr 28))

    (* Types *)

    open Types

    let value_type = function
      | I32Type -> vs7 (-0x01)
      | I64Type -> vs7 (-0x02)
      | F32Type -> vs7 (-0x03)
      | F64Type -> vs7 (-0x04)

    let elem_type = function
      | AnyFuncType -> vs7 (-0x10)

    let stack_type = function
      | [] -> vs7 (-0x40)
      | [t] -> value_type t
      | _ ->
        failwith
          "cannot encode stack type with arity > 1 (yet)"

    let func_type = function
      | FuncType (ins, out) -> vs7 (-0x20); vec value_type ins; vec value_type out

    let limits vu {min; max} =
      bool (max <> None); vu min; opt vu max

    let table_type = function
      | TableType (lim, t) -> elem_type t; limits vu32 lim

    let memory_type = function
      | MemoryType lim -> limits vu32 lim

    let mutability = function
      | Immutable -> u8 0
      | Mutable -> u8 1

    let global_type = function
      | GlobalType (t, mut) -> value_type t; mutability mut

    (* Expressions *)

    open Ast
    open Values

    let op n = u8 n
    let end_ () = op 0x0b

    let memop {align; offset; _} = vu32 (Int32.of_int align); vu32 offset

    let var x = vu32 x
    let temp = ref None
    let code_pos = ref (-1l)
    let rec instr e =
      (match e with
      | CallIndirect _ -> ()
      | _ -> temp := None);

      match e with
      | Unreachable -> op 0x00
      | Nop -> op 0x01

      | Block (ts, es) -> op 0x02; stack_type ts; list instr es; end_ ()
      | Loop (ts, es) -> op 0x03; stack_type ts; list instr es; end_ ()
      | If (ts, es1, es2) ->
        op 0x04; stack_type ts; list instr es1;
        if es2 <> [] then op 0x05;
        list instr es2; end_ ()

      | Br x -> op 0x0c; var x
      | BrIf x -> op 0x0d; var x
      | BrTable (xs, x) -> op 0x0e; vec var xs; var x
      | Return -> op 0x0f
      | Call x ->
          op 0x10;
          let p = pos s in
          relocations := !relocations @ [Func_reloc (Int32.of_int p, x.index)];
          var x.index
      | CallIndirect x ->
        op 0x11;
        (* (match !temp with
        | Some (offset, i) -> relocations := !relocations @ [Memory_address_reloc (Int32.of_int offset, i)]
        | None -> ());
        temp := None; *)
        let p2 = pos s in
        print_endline("CallIndirect: " ^ string_of_int (p2 - Int32.to_int (!code_pos)));
        relocations := !relocations @ [Type_reloc (Int32.of_int p2, x)];
        var x;
        u8 0x00
      | Drop -> op 0x1a
      | Select -> op 0x1b

      | GetLocal x -> op 0x20; var x
      | SetLocal x -> op 0x21; var x
      | TeeLocal x -> op 0x22; var x
      | GetGlobal x ->
        op 0x23;
        let p = pos s in
        relocations := !relocations @ [Global_reloc (Int32.of_int p, x)];
        var x
      | SetGlobal x ->
        op 0x24;
        var x
      | Load ({ty = I32Type; sz = None; _} as mo) -> op 0x28; memop mo
      | Load ({ty = I64Type; sz = None; _} as mo) -> op 0x29; memop mo
      | Load ({ty = F32Type; sz = None; _} as mo) -> op 0x2a; memop mo
      | Load ({ty = F64Type; sz = None; _} as mo) -> op 0x2b; memop mo
      | Load ({ty = I32Type; sz = Some (Mem8, SX); _} as mo) ->
        op 0x2c; memop mo
      | Load ({ty = I32Type; sz = Some (Mem8, ZX); _} as mo) ->
        op 0x2d; memop mo
      | Load ({ty = I32Type; sz = Some (Mem16, SX); _} as mo) ->
        op 0x2e; memop mo
      | Load ({ty = I32Type; sz = Some (Mem16, ZX); _} as mo) ->
        op 0x2f; memop mo
      | Load {ty = I32Type; sz = Some (Mem32, _); _} ->
        assert false
      | Load ({ty = I64Type; sz = Some (Mem8, SX); _} as mo) ->
        op 0x30; memop mo
      | Load ({ty = I64Type; sz = Some (Mem8, ZX); _} as mo) ->
        op 0x31; memop mo
      | Load ({ty = I64Type; sz = Some (Mem16, SX); _} as mo) ->
        op 0x32; memop mo
      | Load ({ty = I64Type; sz = Some (Mem16, ZX); _} as mo) ->
        op 0x33; memop mo
      | Load ({ty = I64Type; sz = Some (Mem32, SX); _} as mo) ->
        op 0x34; memop mo
      | Load ({ty = I64Type; sz = Some (Mem32, ZX); _} as mo) ->
        op 0x35; memop mo
      | Load {ty = F32Type | F64Type; sz = Some _; _} ->
        assert false
      | Store ({ty = I32Type; sz = None; _} as mo) -> op 0x36; memop mo
      | Store ({ty = I64Type; sz = None; _} as mo) -> op 0x37; memop mo
      | Store ({ty = F32Type; sz = None; _} as mo) -> op 0x38; memop mo
      | Store ({ty = F64Type; sz = None; _} as mo) -> op 0x39; memop mo
      | Store ({ty = I32Type; sz = Some Mem8; _} as mo) -> op 0x3a; memop mo
      | Store ({ty = I32Type; sz = Some Mem16; _} as mo) -> op 0x3b; memop mo
      | Store {ty = I32Type; sz = Some Mem32; _} -> assert false
      | Store ({ty = I64Type; sz = Some Mem8; _} as mo) -> op 0x3c; memop mo
      | Store ({ty = I64Type; sz = Some Mem16; _} as mo) -> op 0x3d; memop mo
      | Store ({ty = I64Type; sz = Some Mem32; _} as mo) -> op 0x3e; memop mo
      | Store {ty = F32Type | F64Type; sz = Some _; _} -> assert false
      | CurrentMemory -> op 0x3f; u8 0x00
      | GrowMemory -> op 0x40; u8 0x00
      | Const (I32 c) ->
        op 0x41;
        let p = pos s in
        temp := Some (p, c);
        vs32 c
      | Const (I64 c) -> op 0x42; vs64 c
      | Const (F32 c) -> op 0x43; f32 c
      | Const (F64 c) -> op 0x44; f64 c
      | Link _
      | DelayedConst _ -> assert false

      | Test (I32 I32Op.Eqz) -> op 0x45
      | Test (I64 I64Op.Eqz) -> op 0x50
      | Test (F32 _) -> assert false
      | Test (F64 _) -> assert false

      | Compare (I32 I32Op.Eq) -> op 0x46
      | Compare (I32 I32Op.Ne) -> op 0x47
      | Compare (I32 I32Op.LtS) -> op 0x48
      | Compare (I32 I32Op.LtU) -> op 0x49
      | Compare (I32 I32Op.GtS) -> op 0x4a
      | Compare (I32 I32Op.GtU) -> op 0x4b
      | Compare (I32 I32Op.LeS) -> op 0x4c
      | Compare (I32 I32Op.LeU) -> op 0x4d
      | Compare (I32 I32Op.GeS) -> op 0x4e
      | Compare (I32 I32Op.GeU) -> op 0x4f

      | Compare (I64 I64Op.Eq) -> op 0x51
      | Compare (I64 I64Op.Ne) -> op 0x52
      | Compare (I64 I64Op.LtS) -> op 0x53
      | Compare (I64 I64Op.LtU) -> op 0x54
      | Compare (I64 I64Op.GtS) -> op 0x55
      | Compare (I64 I64Op.GtU) -> op 0x56
      | Compare (I64 I64Op.LeS) -> op 0x57
      | Compare (I64 I64Op.LeU) -> op 0x58
      | Compare (I64 I64Op.GeS) -> op 0x59
      | Compare (I64 I64Op.GeU) -> op 0x5a

      | Compare (F32 F32Op.Eq) -> op 0x5b
      | Compare (F32 F32Op.Ne) -> op 0x5c
      | Compare (F32 F32Op.Lt) -> op 0x5d
      | Compare (F32 F32Op.Gt) -> op 0x5e
      | Compare (F32 F32Op.Le) -> op 0x5f
      | Compare (F32 F32Op.Ge) -> op 0x60

      | Compare (F64 F64Op.Eq) -> op 0x61
      | Compare (F64 F64Op.Ne) -> op 0x62
      | Compare (F64 F64Op.Lt) -> op 0x63
      | Compare (F64 F64Op.Gt) -> op 0x64
      | Compare (F64 F64Op.Le) -> op 0x65
      | Compare (F64 F64Op.Ge) -> op 0x66

      | Unary (I32 I32Op.Clz) -> op 0x67
      | Unary (I32 I32Op.Ctz) -> op 0x68
      | Unary (I32 I32Op.Popcnt) -> op 0x69

      | Unary (I64 I64Op.Clz) -> op 0x79
      | Unary (I64 I64Op.Ctz) -> op 0x7a
      | Unary (I64 I64Op.Popcnt) -> op 0x7b

      | Unary (F32 F32Op.Abs) -> op 0x8b
      | Unary (F32 F32Op.Neg) -> op 0x8c
      | Unary (F32 F32Op.Ceil) -> op 0x8d
      | Unary (F32 F32Op.Floor) -> op 0x8e
      | Unary (F32 F32Op.Trunc) -> op 0x8f
      | Unary (F32 F32Op.Nearest) -> op 0x90
      | Unary (F32 F32Op.Sqrt) -> op 0x91

      | Unary (F64 F64Op.Abs) -> op 0x99
      | Unary (F64 F64Op.Neg) -> op 0x9a
      | Unary (F64 F64Op.Ceil) -> op 0x9b
      | Unary (F64 F64Op.Floor) -> op 0x9c
      | Unary (F64 F64Op.Trunc) -> op 0x9d
      | Unary (F64 F64Op.Nearest) -> op 0x9e
      | Unary (F64 F64Op.Sqrt) -> op 0x9f

      | Binary (I32 I32Op.Add) -> op 0x6a
      | Binary (I32 I32Op.Sub) -> op 0x6b
      | Binary (I32 I32Op.Mul) -> op 0x6c
      | Binary (I32 I32Op.DivS) -> op 0x6d
      | Binary (I32 I32Op.DivU) -> op 0x6e
      | Binary (I32 I32Op.RemS) -> op 0x6f
      | Binary (I32 I32Op.RemU) -> op 0x70
      | Binary (I32 I32Op.And) -> op 0x71
      | Binary (I32 I32Op.Or) -> op 0x72
      | Binary (I32 I32Op.Xor) -> op 0x73
      | Binary (I32 I32Op.Shl) -> op 0x74
      | Binary (I32 I32Op.ShrS) -> op 0x75
      | Binary (I32 I32Op.ShrU) -> op 0x76
      | Binary (I32 I32Op.Rotl) -> op 0x77
      | Binary (I32 I32Op.Rotr) -> op 0x78

      | Binary (I64 I64Op.Add) -> op 0x7c
      | Binary (I64 I64Op.Sub) -> op 0x7d
      | Binary (I64 I64Op.Mul) -> op 0x7e
      | Binary (I64 I64Op.DivS) -> op 0x7f
      | Binary (I64 I64Op.DivU) -> op 0x80
      | Binary (I64 I64Op.RemS) -> op 0x81
      | Binary (I64 I64Op.RemU) -> op 0x82
      | Binary (I64 I64Op.And) -> op 0x83
      | Binary (I64 I64Op.Or) -> op 0x84
      | Binary (I64 I64Op.Xor) -> op 0x85
      | Binary (I64 I64Op.Shl) -> op 0x86
      | Binary (I64 I64Op.ShrS) -> op 0x87
      | Binary (I64 I64Op.ShrU) -> op 0x88
      | Binary (I64 I64Op.Rotl) -> op 0x89
      | Binary (I64 I64Op.Rotr) -> op 0x8a

      | Binary (F32 F32Op.Add) -> op 0x92
      | Binary (F32 F32Op.Sub) -> op 0x93
      | Binary (F32 F32Op.Mul) -> op 0x94
      | Binary (F32 F32Op.Div) -> op 0x95
      | Binary (F32 F32Op.Min) -> op 0x96
      | Binary (F32 F32Op.Max) -> op 0x97
      | Binary (F32 F32Op.CopySign) -> op 0x98

      | Binary (F64 F64Op.Add) -> op 0xa0
      | Binary (F64 F64Op.Sub) -> op 0xa1
      | Binary (F64 F64Op.Mul) -> op 0xa2
      | Binary (F64 F64Op.Div) -> op 0xa3
      | Binary (F64 F64Op.Min) -> op 0xa4
      | Binary (F64 F64Op.Max) -> op 0xa5
      | Binary (F64 F64Op.CopySign) -> op 0xa6

      | Convert (I32 I32Op.ExtendSI32) -> assert false
      | Convert (I32 I32Op.ExtendUI32) -> assert false
      | Convert (I32 I32Op.WrapI64) -> op 0xa7
      | Convert (I32 I32Op.TruncSF32) -> op 0xa8
      | Convert (I32 I32Op.TruncUF32) -> op 0xa9
      | Convert (I32 I32Op.TruncSF64) -> op 0xaa
      | Convert (I32 I32Op.TruncUF64) -> op 0xab
      | Convert (I32 I32Op.ReinterpretFloat) -> op 0xbc

      | Convert (I64 I64Op.ExtendSI32) -> op 0xac
      | Convert (I64 I64Op.ExtendUI32) -> op 0xad
      | Convert (I64 I64Op.WrapI64) -> assert false
      | Convert (I64 I64Op.TruncSF32) -> op 0xae
      | Convert (I64 I64Op.TruncUF32) -> op 0xaf
      | Convert (I64 I64Op.TruncSF64) -> op 0xb0
      | Convert (I64 I64Op.TruncUF64) -> op 0xb1
      | Convert (I64 I64Op.ReinterpretFloat) -> op 0xbd

      | Convert (F32 F32Op.ConvertSI32) -> op 0xb2
      | Convert (F32 F32Op.ConvertUI32) -> op 0xb3
      | Convert (F32 F32Op.ConvertSI64) -> op 0xb4
      | Convert (F32 F32Op.ConvertUI64) -> op 0xb5
      | Convert (F32 F32Op.PromoteF32) -> assert false
      | Convert (F32 F32Op.DemoteF64) -> op 0xb6
      | Convert (F32 F32Op.ReinterpretInt) -> op 0xbe

      | Convert (F64 F64Op.ConvertSI32) -> op 0xb7
      | Convert (F64 F64Op.ConvertUI32) -> op 0xb8
      | Convert (F64 F64Op.ConvertSI64) -> op 0xb9
      | Convert (F64 F64Op.ConvertUI64) -> op 0xba
      | Convert (F64 F64Op.PromoteF32) -> op 0xbb
      | Convert (F64 F64Op.DemoteF64) -> assert false
      | Convert (F64 F64Op.ReinterpretInt) -> op 0xbf

    let const c =
      list instr c; end_ ()

    (* Sections *)

    let section id f x needed =
      if needed then begin
        u8 id;
        let g = gap32 () in
        let p = pos s in
        f x;
        patch_gap32 g (pos s - p)
      end

    let custom_section id f x needed =
      if needed then begin
        u8 0;
        let g = gap32 () in
        let p = pos s in
        string id;
        f x;
        patch_gap32 g (pos s - p)
      end

    (* Type section *)
    let type_ t = func_type t

    let type_section ts =
      section 1 (vec type_) ts (ts <> [])

    (* Import section *)
    let import_desc d =
      match d with
      | FuncImport x -> u8 0x00; var x
      | TableImport t -> u8 0x01; table_type t
      | MemoryImport t -> u8 0x02; memory_type t
      | GlobalImport t -> u8 0x03; global_type t

    let import im =
      let {module_name; item_name; idesc} = im in
      name module_name; name item_name; import_desc idesc

    let import_section ims =
      section 2 (vec import) ims (ims <> [])

    (* Function section *)
    let func f = var f.ftype

    let func_section fs =
      section 3 (vec func) fs (fs <> [])

    (* Table section *)
    let table tab =
      let {ttype} = tab in
      table_type ttype

    let table_section tabs =
      section 4 (vec table) tabs (tabs <> [])

    (* Memory section *)
    let memory mem =
      let {mtype} = mem in
      memory_type mtype

    let memory_section mems =
      section 5 (vec memory) mems (mems <> [])

    (* Global section *)
    let global g =
      let {gtype; value} = g in
      global_type gtype; const value

    let global_section gs =
      section 6 (vec global) gs (gs <> [])

    (* Export section *)
    let export_desc d =
      match d with
      | FuncExport x -> u8 0; var x
      | TableExport x -> u8 1; var x
      | MemoryExport x -> u8 2; var x
      | GlobalExport x -> u8 3; var x

    let export ex =
      let {name = n; edesc} = ex in
      name n; export_desc edesc

    let export_section exs =
      section 7 (vec export) exs (exs <> [])

    (* Start section *)
    let start_section xo =
      section 8 (opt var) xo (xo <> None)

    (* Code section *)
    let compress ts =
      let combine t = function
        | (t', n) :: ts when t = t' -> (t, n + 1) :: ts
        | ts -> (t, 1) :: ts
      in List.fold_right combine ts []

    let local (t, n) = len n; value_type t

    let code f =
      let {locals; body; _} = f in
      print_endline ("Code pos:" ^ string_of_int (pos s - (Int32.to_int !code_pos)));
      let g = gap32 () in
      let p = pos s in
      vec local (compress locals);
      list instr body;
      end_ ();
      print_endline ("EndPos:" ^ string_of_int (pos s - (Int32.to_int !code_pos)));
      patch_gap32 g (pos s - p)

    let code_section fs =
      code_pos := Int32.of_int (pos s + 6);
      section 10 (vec code) fs (fs <> [])

    (* Element section *)
    let segment dat seg =
      let {index; offset; init} = seg in
      var index; const offset; dat init

    let table_segment seg =
      segment (vec var) seg

    let elem_section elems =
      section 9 (vec table_segment) elems (elems <> [])

    (* Data section *)
    (* let memory_segment seg =
      segment string seg *)

    let data_part_list (data_part_list:data_part) =
      let data_length = List.fold_left (fun cur add ->
          cur + match add with
          | String s -> String.length s
          | Nativeint _
          | Float32 _ -> 4
          | Int32 _ -> 4
          | Int16 _ -> 2
          | Int8 _ -> 1
      ) 0 data_part_list.detail in
      len data_length;
      List.iter (fun f ->
        match f with
        | String bs -> put_string s bs
        | Float32 f -> f32 f
        | Int32 i32 -> u32 i32
        | Nativeint ni -> u32 (Nativeint.to_int32 ni)
        | Int16 i -> u16 i
        | Int8 i -> u8 i
      ) data_part_list.detail

    let data_segment seg = (* https://github.com/WebAssembly/design/blob/master/BinaryEncoding.md#data-section *)
      segment data_part_list seg

    let data_section data =
      section 11 (vec data_segment) data (data <> [])

    let reloc_code symbols data =
      u8 10;
      vu32 (Int32.of_int (List.length !relocations));
      List.iter (fun r ->
        match r with
        | Type_reloc (offset, index) ->
          u8 6;
          vu32p (Int32.sub offset !code_pos) 5;
          vu32 index

        | Func_reloc (offset, index_) ->
          (let symbol_index = ref (-1) in
          List.iteri (fun i s -> match s.details with
            | Import index when index = index_ -> symbol_index := i
            | Function {index; _} when index = index_ -> symbol_index := i
            | _ -> ()) symbols;
          u8 0;
          vu32 (Int32.sub offset !code_pos);
          vu32 (Int32.of_int !symbol_index))
        | Memory_address_reloc (offset, index) ->
          (* print_endline "MEMORY ADDR RELOC!"; *)
          u8 1;
          vu32 (Int32.sub offset !code_pos);
          vu32 index
          (*
            2 and 5 are data related
          *)
          (*
            3 and 4 are code related
          *)

        | Global_reloc (offset, index) ->
          u8 7;
          vu32 (Int32.sub offset !code_pos);
          vu32 index
        | _ -> assert false
      ) !relocations

    let relocate_code_section symbols data =
      custom_section "reloc.CODE" (reloc_code symbols) data (data <> [])
(*
    let relocate_data_section data =
      custom_section "reloc.DATA" (vec data_segment) data (data <> []) *)

    let symbol sym =
      (match sym.details with
      | Import _
      | Function _ -> u8 0
      | Data _ ->  u8 1
      | Global _ -> u8 2
      );
      vu32 sym.flags;
      (match sym.details with
      | Global f
      | Function f ->
        vu32 f.index;
        string f.name;
      | Import i ->
        vu32 i;
      | Data d ->
        string d.name;
        print_endline ("Data: " ^ d.name);
        print_endline "-------";
        print_endline ("Index:" ^ Int32.to_string d.index);
        print_endline ("Offset:" ^ Int32.to_string d.offset);
        print_endline ("Size:" ^ Int32.to_string d.size);
        vu32 d.index;
        vu32 d.offset;
        vu32 d.size
      )


    let symbol_table (data: Ast.sym_info list) =
      (* let size = ref 0l in
      List.iter (fun f ->
        match f.details with
        | Data d -> size := Int32.add d.offset d.size
        | _ -> ()
      ) data;
      u8 2; (* WASM_DATA_SIZE *)
      let g = gap32 () in
      let p = pos s in
      vu32 !size;
      patch_gap32 g (pos s - p); *)
      (* u8 3; (* WASM_DATA_ALIGNMENT *)
      let g = gap32 () in
      let p = pos s in
      vu32 0l;
      patch_gap32 g (pos s - p); *)

      u8 5;
      let g = gap32 () in
      let p = pos s in
      let no_of_data = List.length (List.filter (fun f -> match f.details with Data _ -> true | _ -> false) data) in
      vu32 (Int32.of_int no_of_data);
      List.iter (fun f ->
        match f.details with
        | Data d -> (
          print_endline ("A SEGMENT HERE:" ^ d.name);
          string d.name;
          vu32 4l;
          vu32 0l;
          )
        | _ -> ()
      ) data;
      patch_gap32 g (pos s - p);
      print_endline "\nlet symbol_table\n========\n";
      u8 8; (* WASM_SYMBOL_TABLE *)
      let g = gap32 () in
      let p = pos s in

      vec symbol data;
      patch_gap32 g (pos s - p);


      List.iteri(fun i f ->
        match f.details with
        | Function {name = "_start"; _ } ->  (
          u8 6; (* WASM_INIT_FUNCS *)
          let g = gap32 () in
          let p = pos s in
          vu32 1l;
          vu32 0l;
          vu32 (Int32.of_int i);
          patch_gap32 g (pos s - p)
          )
        | _ -> ()
        ) data


    let linking_section (data:Ast.sym_info list) =
      custom_section "linking" symbol_table data (data <> [])

      (* Module *)
    let module_ m =
      u32 0x6d736100l;
      u32 version;
      type_section m.types;
      import_section m.imports;
      func_section m.funcs;
      table_section m.tables;
      memory_section m.memories;
      global_section m.globals;
      export_section m.exports;
      start_section m.start;
      elem_section m.elems;
      code_section m.funcs;
      data_section m.data;
      linking_section m.symbols;
      if List.length !relocations > 0 then
        relocate_code_section m.symbols m.funcs

      (* TODO: relocate_data_section m.data; *)

  end
  in E.module_ m; to_string s
