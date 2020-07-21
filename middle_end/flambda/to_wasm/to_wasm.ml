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

open Wmm
open Wasm.Wasm_ast
open Wasm.Wasm_types

module Env = To_wasm_env
module Ece = Effects_and_coeffects


let todo () = failwith "Not implemented yet!"

type static_data =
  DataItem of string * bytes

type res = {
  static_data: static_data list;
  fun_decls: Wasm.Wasm_ast.func list;
  instr_list: Wasm.Wasm_ast.instr list;
}

let static_const
      env
      (bound_symbols : Flambda.Let_symbol_expr.Bound_symbols.t)
      (static_const : Flambda.Static_const.t) =
  let _ = Format.printf "\nF: static_const" in
  let _ = Format.printf "\nbound_symbols: " in
  let _ = Flambda.Let_symbol_expr.Bound_symbols.print Format.std_formatter bound_symbols in
  let _ = Format.printf "\nstatic_const: " in
  let _ = Flambda.Static_const.print Format.std_formatter static_const in
  let _ = Format.printf "\n" in

  match bound_symbols, static_const with

  (* Flambda code:

  | Singleton s, Block (tag, _mut, fields) ->
      let name = symbol s in
      let tag = Tag.Scannable.to_int tag in
      let block_name = name, Cmmgen_state.Global in
      let header = C.block_header tag (List.length fields) in
      let env, static_fields =
        List.fold_right
          (fun v (env, static_fields) ->
             let env, static_field = static_value env v in
             env, static_field :: static_fields)
          fields (env, [])
      in
      let block = C.emit_block block_name header static_fields in
      let updates = static_block_updates (C.symbol name) env None 0 fields in
      env, R.set_data r block, updates
  *)
  | Singleton s, Block (tag, _mut, fields) ->
    let block = Bytes.create (4*(List.length fields + 1)) in
    let rec init_block i = function
      | [] -> ()
      | x::xs -> match x with
        | Flambda.Static_const.Field_of_block.Symbol (_s) -> failwith "cannot statically allocate symbol" (* TODO: this is not true, but we need to update the Env in order to statically allocate a symbol *)
        | Flambda.Static_const.Field_of_block.Tagged_immediate (target_imm) ->
          Bytes.set_int64_ne block (4*i) (Targetint.OCaml.to_int64 (Target_imm.to_targetint target_imm));
          init_block (i+1) xs;
          ()
        | Flambda.Static_const.Field_of_block.Dynamically_computed (_v) ->
          Bytes.set_int64_ne block (4*i) 1L;
          init_block (i+1) xs;
          ()
    in
    let tag = Int64.of_int (Tag.Scannable.to_int tag) in
    let size = (Int64.of_int (List.length fields)) in
    let gc_bits = (Int64.of_int 3) in
    let header = Int64.logor tag (Int64.logor (Int64.shift_left size 10) (Int64.shift_left gc_bits 8))in
    Bytes.set_int64_ne block 0 header;
    init_block 1 fields;
    (* TODO: Change all of this to use the GC MVP with anyref. GC bits are useless on WASM, probably.
     * However, GC bits will also not be a problem, so I add them here. This way, I can compare the block rep in the data segment to match 1:1 with
     * the regular OCaml block representation.
     * I assume 64-bit values here, and I do NOT tag integers currently, as the plan is to work with WASM GC. *)
    env, { static_data = [DataItem (Linkage_name.to_string(Symbol.linkage_name s), block)]; fun_decls = []; instr_list = [];}
  | _ ->
    failwith "static_const not implemented for this"

let cont_has_one_occurrence k num =
  match (num : Name_occurrences.Num_occurrences.t) with
  | One -> true
  | More_than_one -> false
  | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation %a, this should not happen"
        Continuation.print k


type inlining_decision =
  | Skip (* no use, the bound variable can be skipped/ignored *)
  | Inline (* the variable is used once, we can try and inline its use *)
  | Regular (* the variable is used multiple times, do not try and inline it. *)


let decide_inline_let effs v body =
  let free_names = Expr.free_names body in
  match Name_occurrences.count_variable free_names v with
  | Zero ->
    begin match Env.classify effs with
    | Coeffect | Pure -> Skip
    | Effect -> Regular (* Could be Inline technically, but it doesn't matter
                           since it can only be flushed by the env. *)
    end
  | One ->
    begin match Env.classify effs with
    | Effect when not (Flambda_features.Expert.inline_effects_in_cmm ()) -> Regular
    | _ -> Inline
    end
  | More_than_one -> Regular

let rec expr env e =
  match (Flambda.Expr.descr e : Flambda.Expr.descr) with
  | Let e' -> lexpr env e'
  | Let_symbol e' -> lsymbol env e'
  | Let_cont e' -> lcont env e'
  | Apply e' -> apply_expr env e'
  | Apply_cont e' -> apply_cont env e'
  | Switch e' -> switch env e'
  | Invalid e' -> invalid env e'

and named env n =
  match (n : Flambda.Named.t) with
  | Simple s -> todo ()
    (*let t, env, effs = simple env s in
    t, None, env, effs*)
  | Set_of_closures s -> todo ()
    (*let t, env, effs = set_of_closures env s in
    t, None, env, effs*)
  | Prim (p, dbg) -> 
      match p with
      | Unary (Duplicate_array {kind; source_mutability; destination_mutability;}, s) ->
      (* emit a call to the primitive implementation in WebAssembly *)
        todo ()
      | _ -> todo ()
    (*let prim_eff = Flambda_primitive.effects_and_coeffects p in
    let t, extra, env, effs = prim env dbg p in
    t, extra, env, Ece.join effs prim_eff*)

and decide_inline_cont h k num_free_occurrences =
  not (Flambda.Continuation_handler.is_exn_handler h)
  && (Flambda.Continuation_handler.stub h
      || cont_has_one_occurrence k num_free_occurrences)

and let_expr_bind ?extra body env v wmm_expr effs =
  match decide_inline_let effs v body with
  | Skip -> env
  | Inline -> Env.bind_variable env v ?extra effs true wmm_expr
  | Regular -> Env.bind_variable env v ?extra effs false wmm_expr

and lexpr env t = 
  let _ = Format.printf "\nTrying to compile let_expr:" in
  let _ = Flambda.Let_expr.print Format.std_formatter t in
  let _ = Format.printf "\n" in

  Flambda.Let_expr.pattern_match t ~f:(fun ~bound_vars ~body ->
    let mode = Bindable_let_bound.name_mode bound_vars in
    match Name_mode.descr mode with
    | In_types ->
      Misc.fatal_errorf
        "Binding in terms a variable of mode In_types is forbidden"
    | Phantom ->
      let _ = Format.printf "\nPhantom let ok\n" in
      expr env body
    | Normal ->
      let e = Flambda.Let.defining_expr t in
      begin match bound_vars, e with
      | Singleton v, _ ->
        let v = Var_in_binding_pos.var v in
        let wmm_expr, extra, env, effs = named env e in
        let env = let_expr_bind ?extra body env v wmm_expr effs in
        expr env body
      | Set_of_closures { closure_vars; _ }, Set_of_closures soc ->
        todo ()
        (*let_set_of_closures env body closure_vars soc*)
      | Set_of_closures _, (Simple _ | Prim _) ->
        Misc.fatal_errorf
          "Set_of_closures binding a non-Set_of_closures:@ %a"
          Flambda.Let.print t
      end
  )
  (*{ static_data = []; fun_decls = []; instr_list = [];}*)

and lsymbol env let_sym =
  let _ = Format.printf "\nTrying to compile let_symbol_expr:" in
  let _ = Flambda.Let_symbol_expr.print Format.std_formatter let_sym in
  let _ = Format.printf "\n" in

  let body = Flambda.Let_symbol_expr.body let_sym in
  let bound_symbols = Flambda.Let_symbol_expr.bound_symbols let_sym in
  let env' = { env with 
    names_in_scope = Code_id_or_symbol.Set.union
      env.names_in_scope
      (Flambda.Let_symbol_expr.Bound_symbols.everything_being_defined bound_symbols)}
      (* All bound symbols are allowed to appear in each other's definition,
       so they're added to the environment first *)
  in
  let env'', {static_data; fun_decls; instr_list} =
    static_const
      env'
      (Flambda.Let_symbol_expr.bound_symbols let_sym)
      (Flambda.Let_symbol_expr.defining_expr let_sym)
  in
  let r2 = expr env'' body in
  { static_data = static_data @ r2.static_data; fun_decls = fun_decls @ r2.fun_decls; instr_list = instr_list @ r2.instr_list }

and lcont _env t = 
  let _ = Format.printf "\nTrying to compile let_cont:" in
  let _ = Flambda.Let_cont_expr.print Format.std_formatter t in
  let _ = Format.printf "\n" in
  todo ()

and apply_expr _env t = 
  let _ = Format.printf "\nTrying to compile apply_expr:" in
  let _ = Apply_expr.print Format.std_formatter t in
  let _ = Format.printf "\n" in
  todo ()

and apply_cont env expr = 
  let _ = Format.printf "\nTrying to compile apply_cont:" in
  let _ = Apply_cont_expr.print Format.std_formatter expr in
  let _ = Format.printf "\n" in

  match Continuation.sort (Apply_cont_expr.continuation expr), Apply_cont_expr.trap_action expr, Apply_cont_expr.args expr with
  (*
      | Normal, None, [] -> "goto", None
      | Normal, None, _::_ -> "apply_cont", None
      | Normal, Some trap_action, [] -> "goto", Some trap_action
      | Normal, Some trap_action, _::_ -> "apply_cont", Some trap_action
      | Return, None, [] -> "return", None
      | Return, None, _::_ -> "return", None
      | Return, Some trap_action, [] -> "return", Some trap_action
      | Return, Some trap_action, _::_ -> "return", Some trap_action
      | Define_root_symbol, None, [] ->
        "apply_cont", None
      | Define_root_symbol, None, _::_ ->
        "apply_cont", None
      | Define_root_symbol, Some trap_action, [] ->
        "apply_cont", Some trap_action
      | Define_root_symbol, Some trap_action, _::_ ->
        "apply_cont", Some trap_action *)
      | Toplevel_return, None, [] ->
        { static_data = []; fun_decls = []; instr_list = []}
      | Toplevel_return, None, _::_ ->
        let _ = Format.printf "\nthis is a top-level return with arguments" in
        { static_data = []; fun_decls = []; instr_list = [LocalGet {index = 0l; name = Some "in"}]}
          (* Const (I64 (I64.of_int_u 0)) *)
          (** Sabine: for now, this is the same address that the module initialization function got passed in.
              With the reftypes proposal, the initialization function could take as parameter the index in a shared function table
              where we have to place the module's global data.
              With a GC on linear memory, we return the address of the start of the data region, which I think is the same as
              the traditional OCaml compiler does.
              *)
      (*
      | Toplevel_return, Some trap_action, [] ->
        "module_init_end", Some trap_action
      | Toplevel_return, Some trap_action, _::_ ->
        "module_init_end", Some trap_action
      (* CR mshinwell: See CR on [create], below. *)
      | Exn, (None | Some (Push _)), []
      | Exn, (None | Some (Push _)), _::_ ->
        "apply_cont", trap_action (*assert false*)
      | Exn, Some (Pop _), [] -> "raise", None
      | Exn, Some (Pop _), _::_ -> "raise", None
      *)
      
      | _ -> todo ()

and switch _env t = 
  let _ = Format.printf "\nTrying to compile switch_expr:" in
  let _ = Switch_expr.print Format.std_formatter t in
  let _ = Format.printf "\n" in
  todo ()

and invalid _env _t =
  failwith "Invalid expression from Flambda encountered"
  

let rec static_data_to_wasm index = function
  | [] -> [], ""
  | DataItem (x, data)::xs -> 
    let remainder_globals, remainder_data_parts = static_data_to_wasm (index + Bytes.length data) xs in
    let global = {
      name = Some(x);
      gtype = GlobalType (NumValueType I64Type, Mutable);
      value = [Const (I64 (I64.of_int_u index))];
    } in
    global :: remainder_globals, Bytes.to_string data ^ remainder_data_parts


let unit (middle_end_result : Flambda_middle_end.middle_end_result) = 
  let unit = middle_end_result.unit in
  let _ = Flambda.Expr.print Format.std_formatter (Flambda_unit.body unit) in

  let unit = middle_end_result.unit in
  let offsets =
    match middle_end_result.cmx with
    | None -> Exported_offsets.imported_offsets ()
    | Some cmx -> Flambda_cmx_format.exported_offsets cmx
  in

  Profile.record_call "flambda2_to_wasm" (fun () ->
    let offsets = To_wasm_closure.compute_offsets offsets unit in
    let used_closure_vars = Flambda_unit.used_closure_vars unit in
    let dummy_k = Continuation.create () in
    let env =
      Env.mk offsets dummy_k
        (Flambda_unit.exn_continuation unit)
        used_closure_vars
    in

    let {static_data; fun_decls; instr_list} = expr env (Flambda_unit.body unit) in
(*
    let global_variables = [] in
    let functions = [] in
    let module_init_function_instructions = [] in
*)

    let module_init_function_type_name = "_t__module_init" in
    let module_init_function_type = TypeFunc (FuncType {
      name = Some module_init_function_type_name;
      t = ([
        NamedValueType {
          name = Some "in";
          t = NumValueType I64Type;
        }
        ],[NumValueType I64Type]);
    }) in
    let module_init_function_type_index = {index = 0l; name = Some "module_init_t"} in
    let module_init_function_name = "__module_init" in
    let module_init_function_index = {index = 0l; name = Some module_init_function_name} in
    let module_init_function = {
      name = module_init_function_name;
      ftype = module_init_function_type_index;
      locals = [];
      body = instr_list;
    } in
    let module_init_function_export = {
      name = module_init_function_name;
      edesc = FuncExport module_init_function_index
    } in

    let globals, data_parts = static_data_to_wasm 0 static_data in
    let data_segment = {
      index = {name = None; index = 0l};
      offset = [Const (I64 (I64.of_int_u 0))];
      init = data_parts;
    } in

    { empty_module with
      imports = [];
      memories = [{mtype = MemoryType ({min = 100l; max = None})}];
      globals = globals;
      data = [data_segment];
      types = [module_init_function_type];
      funcs = fun_decls @ [module_init_function];
      start = None;
      exports = [module_init_function_export]
    }
  )