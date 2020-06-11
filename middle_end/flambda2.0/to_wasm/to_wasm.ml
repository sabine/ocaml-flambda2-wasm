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

module Ece = Effects_and_coeffects


let todo () = failwith "Not implemented yet!"

type cont =
  | Jump of { t: func_type; cont: funcidx; }
  | Inline of { handler_params: Kinded_parameter.t list;
                handler_body: Flambda.Expr.t; }

type env = {
  k_return : Continuation.t; (* int *)
  k_exn : Continuation.t; (* int *)
  used_closure_vars : Var_within_closure.Set.t;

  function_needs_closure: bool Code_id.Map.t;

  vars  : instr Variable.Map.t;
  conts : cont Continuation.Map.t;
  (* Map from continuations to handlers (i.e variables bound by the
     continuation and expression of the continuation handler). *)

  names_in_scope : Code_id_or_symbol.Set.t;
  (* Code ids and symbols bound in this scope, for invariant checking *)
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
  | Singleton s, Block (tag, _mut, fields) ->
    env, [], []
  | _ ->
    failwith "static_const not implemented for this"

let rec expr env e =
  match (Flambda.Expr.descr e : Flambda.Expr.descr) with
  | Let e' -> lexpr env e'
  | Let_symbol e' -> lsymbol env e'
  | Let_cont e' -> lcont env e'
  | Apply e' -> apply_expr env e'
  | Apply_cont e' -> apply_cont env e'
  | Switch e' -> switch env e'
  | Invalid e' -> invalid env e'

and lexpr _env t = 
  let _ = Format.printf "\nTrying to compile let_expr:" in
  let _ = Flambda.Let_expr.print Format.std_formatter t in
  let _ = Format.printf "\n" in
  todo ()

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
  let env'', globals, instructions =
    static_const
      env'
      (Flambda.Let_symbol_expr.bound_symbols let_sym)
      (Flambda.Let_symbol_expr.defining_expr let_sym)
  in
  let globals', functions, instructions' = expr env'' body in
  globals @ globals', functions, instructions @ instructions'

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
        [], [], []
      | Toplevel_return, None, _::_ ->
        let _ = Format.printf "\nthis is a top-level return with arguments" in
        [], [], [GetLocal {index = 0l; name = Some "in"}]
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
  failwith "Invalid expression from Flambda2.0 encountered"
  

let unit (unit : Flambda_unit.t) = 
  let _ = Flambda.Expr.print Format.std_formatter (Flambda_unit.body unit) in

  Profile.record_call "flambda2_to_wasm" (fun () ->
    let used_closure_vars = Flambda_unit.used_closure_vars unit in
    let env = {
      k_return = Flambda_unit.return_continuation unit;
      k_exn = Flambda_unit.exn_continuation unit;
      used_closure_vars = used_closure_vars;
      function_needs_closure = Code_id.Map.empty;
      vars = Variable.Map.empty;
      conts = Continuation.Map.empty;
      names_in_scope = Code_id_or_symbol.Set.empty;
    } in

    let (global_variables, functions, module_init_function_instructions) = expr env (Flambda_unit.body unit) in

(*
    let global_variables = [] in
    let functions = [] in
    let module_init_function_instructions = [] in
*)

    let module_init_function_type_name = "_t__module_init" in
    let module_init_function_type = FuncType {
      name = Some module_init_function_type_name;
      t = ([
        NamedValueType {
          name = Some "in";
          t = NumValueType I64Type;
        }
        ],[NumValueType I64Type]);
    } in
    let module_init_function_name = "__module_init" in
    let module_init_function_index = {index = 0l; name = Some module_init_function_name} in
    let module_init_function = {
      name = module_init_function_name;
      ftype = module_init_function_type;
      locals = [];
      body = module_init_function_instructions;
    } in
    let module_init_function_export = {
      name = module_init_function_name;
      edesc = FuncExport module_init_function_index
    } in

    { empty_module with
      imports = [];
      globals = global_variables;
      types = [];
      funcs = functions @ [module_init_function];
      start = None;
      exports = [module_init_function_export]
    }
  )