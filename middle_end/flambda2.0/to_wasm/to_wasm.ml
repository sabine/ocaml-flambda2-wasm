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

open Wasm.Wasm_ast
open Wasm.Wasm_types

module Ece = Effects_and_coeffects

let todo () = failwith "Not yet implemented"



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

and lexpr _env _t = todo ()

and lsymbol env let_sym =
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

and lcont _env _t = todo ()
and apply_expr _env _t = 
  Apply_expr.print Format.std_formatter _t;
  todo ()
and apply_cont _env _t = 
  Apply_cont_expr.print Format.std_formatter _t;
  todo ()
and switch _env _t = todo ()
and invalid _env _t = todo ()
  

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

(*
    let (global_variables, functions, module_init_function_instructions) = expr env (Flambda_unit.body unit) in
*)
    let global_variables = [] in
    let functions = [] in
    let module_init_function_instructions = [] in

    let module_init_function_type_name = "_t__module_init" in
    let module_init_function_type = TypeFunc (FuncType {
      name = Some module_init_function_type_name;
      t = ([NumValueType I32Type],[])
    }) in
    let module_init_function_type_index = {index = 0l; name = Some module_init_function_type_name} in
    let module_init_function_name = "__module_init" in
    let module_init_function_index = {index = 0l; name = Some module_init_function_name} in
    let module_init_function = {
      name = module_init_function_name;
      ftype = module_init_function_type_index;
      locals = [];
      body = module_init_function_instructions;
    } in
    let module_init_function_export = {
      name = module_init_function_name;
      edesc = FuncExport module_init_function_index
    } in

    let test_array_type = TypeArray (ArrayType {
          name = Some "testarray";
          t = FieldType (Mutable, StorageTypeValue (NumValueType I32Type))
        })
    in

    { empty_module with
      imports = [];
      globals = global_variables;
      types = [module_init_function_type; test_array_type];
      funcs = [module_init_function] @ functions;
      start = None;
      exports = [module_init_function_export]
    }
  )