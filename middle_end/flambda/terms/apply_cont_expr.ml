(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

type t = {
  k : Continuation.t;
  args : Simple.t list;
  trap_action : Trap_action.Option.t;
  dbg : Debuginfo.t;
}

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf { k; args; trap_action; dbg; } =
    let name, trap_action =
      match Continuation.sort k, trap_action, args with
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
        "apply_cont", Some trap_action
      | Toplevel_return, None, [] ->
        "module_init_end", None
      | Toplevel_return, None, _::_ ->
        "module_init_end", None
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
    in
    Format.fprintf ppf "@[<hov 1>%a@<0>%s%s@<0>%s %a"
      Trap_action.Option.print trap_action
      (Flambda_colours.expr_keyword ())
      name
      (Flambda_colours.normal ())
      Continuation.print k;
    begin match args with
    | [] -> ()
    | args ->
      Format.fprintf ppf " %a" Simple.List.print args
    end;
    Format.fprintf ppf "@<0>%s%a@<0>%s@]"
      (Flambda_colours.elide ())
      Debuginfo.print_or_elide dbg
      (Flambda_colours.normal ())

  let output _ _ = Misc.fatal_error "Not yet implemented"

  let hash _ = Misc.fatal_error "Not yet implemented"

  (* CR mshinwell: I wonder if the Debuginfo should be excluded from this.
     For example at the moment we could get a Switch with two arms that go
     to the same place but differ only on Debuginfo. *)
  let compare
        { k = k1; args = args1; trap_action = trap_action1; dbg = dbg1; }
        { k = k2; args = args2; trap_action = trap_action2; dbg = dbg2; } =
    let c = Continuation.compare k1 k2 in
    if c <> 0 then c
    else
      let c = Misc.Stdlib.List.compare Simple.compare args1 args2 in
      if c <> 0 then c
      else
        let c =
          Option.compare Trap_action.compare trap_action1 trap_action2
        in
        if c <> 0 then c
        else Debuginfo.compare dbg1 dbg2

  let equal t1 t2 = (compare t1 t2 = 0)
end)

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant env ({ k; args; trap_action; dbg=_; } as t) =
  let module E = Invariant_env in
  let unbound_continuation cont reason =
    Misc.fatal_errorf "Unbound continuation %a in %s: %a"
      Continuation.print cont
      reason
      print t
  in
  let args_arity = List.map (fun arg -> E.kind_of_simple env arg) args in
  let arity, kind (*, cont_stack *) =
    match E.find_continuation_opt env k with
    | Some result -> result
    | None -> unbound_continuation k "[Apply_cont] term"
  in
(*
  let stack = E.current_continuation_stack env in
  E.Continuation_stack.unify cont stack cont_stack;
*)
  if not (Flambda_arity.equal args_arity arity) then begin
    Misc.fatal_errorf "Continuation %a called with wrong arity in \
        this [Apply_cont] term: expected %a but found %a:@ %a"
      Continuation.print k
      Flambda_arity.print arity
      Flambda_arity.print args_arity
      print t
  end;
  begin match kind with
  | Normal -> ()
  | Exn_handler ->
    Misc.fatal_errorf "Continuation %a is an exception handler \
        but is used in this [Apply_cont] term as a normal continuation:@ \
        %a"
      Continuation.print k
      print t
  end;
  let check_exn_handler exn_handler =
    match E.find_continuation_opt env exn_handler with
    | None ->
      unbound_continuation exn_handler "[Apply] trap handler"
    | Some (arity, kind (*, cont_stack *)) ->
      begin match kind with
      | Exn_handler -> ()
      | Normal ->
        Misc.fatal_errorf "Continuation %a is a normal continuation  \
            but is used in the trap action of this [Apply] term as an \
            exception handler:@ %a"
          Continuation.print exn_handler
          print t
      end;
      assert (not (Continuation.equal k exn_handler));
      let expected_arity = [K.value] in
      if not (Flambda_arity.equal arity expected_arity) then begin
        Misc.fatal_errorf "Exception handler continuation %a has \
            the wrong arity for the trap handler action of this \
            [Apply] term: expected %a but found %a:@ %a"
          Continuation.print k
          Flambda_arity.print expected_arity
          Flambda_arity.print arity
          print t
      end;
      ()
(*
      cont_stack
*)
  in
(*
  let current_stack = E.current_continuation_stack env in
*)
  (* CR mshinwell for pchambart: We need to fix this.  I've removed the
     trap IDs since we don't need them for compilation, and they would be
     another kind of name that needs freshening (which is weird since they
     don't have any binding site). *)
(*
  let stack, cont_stack =
*)
    match trap_action with
    | None -> () (*current_stack, cont_stack *)
    | Some (Push { exn_handler }) ->
      check_exn_handler exn_handler
(*
      let cont_stack = check_exn_handler exn_handler in
      E.Continuation_stack.push id exn_handler current_stack, cont_stack
*)
    | Some (Pop { exn_handler; raise_kind = _; }) ->
      check_exn_handler exn_handler

(*
      let cont_stack = check_exn_handler exn_handler in
      current_stack, E.Continuation_stack.push id exn_handler cont_stack
*)
(*
  in
  E.Continuation_stack.unify cont stack cont_stack
  current_stack
  *)

(* CR mshinwell: Check the sort of [k]. *)
let create ?trap_action k ~args ~dbg = { k; args; trap_action; dbg; }

let goto k =
  { k;
    args = [];
    trap_action = None;
    dbg = Debuginfo.none;
  }

let continuation t = t.k
let args t = t.args
let trap_action t = t.trap_action
let debuginfo t = t.dbg

let free_names { k; args; trap_action; dbg=_; } =
  let default =
    Name_occurrences.add_continuation (Simple.List.free_names args) k
  in
  match trap_action with
  | None -> default
  | Some trap_action ->
    (* A second occurrence of [k] is added to ensure that the continuation [k]
       is never inlined out (which cannot be done, or the trap action
       would be lost). *)
    Name_occurrences.add_continuation
      (Name_occurrences.union default (Trap_action.free_names trap_action))
      k

let apply_name_permutation ({ k; args; trap_action; dbg; } as t) perm =
  let k' = Name_permutation.apply_continuation perm k in
  let args' = Simple.List.apply_name_permutation args perm in
  let trap_action' =
    match trap_action with
    | None -> None
    | Some trap_action' ->
      let new_trap_action' =
        Trap_action.apply_name_permutation trap_action' perm
      in
      if new_trap_action' == trap_action' then trap_action
      else Some new_trap_action'
  in
  if k == k' && args == args' && trap_action == trap_action' then t
  else { k = k'; args = args'; trap_action = trap_action'; dbg; }

let all_ids_for_export { k = _; args; trap_action = _; dbg = _; } =
  List.fold_left (fun ids arg -> Ids_for_export.add_simple ids arg)
    Ids_for_export.empty
    args

let import import_map { k; args; trap_action; dbg; } =
  let args = List.map (Ids_for_export.Import_map.simple import_map) args in
  { k; args; trap_action; dbg; }

let update_continuation t continuation =
  { t with k = continuation; }

let update_continuation_and_args t cont ~args =
  if Continuation.equal t.k cont && args == t.args then t
  else
    { t with
      k = cont;
      args;
    }

let update_args t ~args =
  if args == t.args then t
  else { t with args; }

let no_args t =
  match args t with
  | [] -> true
  | _::_ -> false

let is_goto t =
  no_args t && Option.is_none (trap_action t)

let is_goto_to t k =
  Continuation.equal (continuation t) k && is_goto t

let to_goto t =
  if no_args t && Option.is_none (trap_action t) then Some (continuation t)
  else None

let clear_trap_action t =
  { t with trap_action = None; }

let to_one_arg_without_trap_action t =
  match t.trap_action with
  | Some _ -> None
  | None ->
    match t.args with
    | [arg] -> Some arg 
    | [] | _::_::_ -> None
