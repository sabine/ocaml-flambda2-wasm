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

module Bound_symbols = struct
  module Code_and_set_of_closures = struct
    type t = {
      code_ids : Code_id.Set.t;
      closure_symbols : Symbol.t Closure_id.Map.t;
    }

    (* CR mshinwell: Share with [Bindable_let_bound] and below *)
    let print_closure_binding ppf (closure_id, sym) =
      Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
        Symbol.print sym
        (Flambda_colours.elide ())
        (Flambda_colours.elide ())
        Closure_id.print closure_id

    let print ppf { code_ids; closure_symbols; } =
      match
        Code_id.Set.elements code_ids, Closure_id.Map.bindings closure_symbols
      with
      | [code_id], [] ->
        Format.fprintf ppf "%a" Code_id.print code_id
      | [], [closure_binding] ->
        Format.fprintf ppf "@<0>%s%a@<0>%s"
          (Flambda_colours.symbol ())
          print_closure_binding closure_binding
          (Flambda_colours.normal ())
      | _, _ ->
        Format.fprintf ppf "@[<hov 1>\
            @[<hov 1>(code_ids@ (%a))@]@ \
            @[<hov 1>(closure_symbols@ {%a})@]\
            @]"
          Code_id.Set.print code_ids
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
            print_closure_binding)
          (Closure_id.Map.bindings closure_symbols)

    let being_defined { code_ids = _; closure_symbols; } =
      Symbol.Set.of_list (Closure_id.Map.data closure_symbols)

    let closure_symbols_being_defined { code_ids = _; closure_symbols; } =
      Symbol.Set.of_list (Closure_id.Map.data closure_symbols)

    let code_being_defined { code_ids; closure_symbols = _; } = code_ids

    let free_names { code_ids; closure_symbols; } =
      let from_code_ids =
        Code_id.Set.fold (fun code_id from_code_ids ->
            Name_occurrences.add_code_id from_code_ids code_id Name_mode.normal)
          code_ids
          Name_occurrences.empty
      in
      Closure_id.Map.fold (fun _closure_id closure_sym bound_names ->
          Name_occurrences.add_symbol bound_names closure_sym Name_mode.normal)
        closure_symbols
        from_code_ids
  end

  type t =
    | Singleton of Symbol.t
    | Sets_of_closures of Code_and_set_of_closures.t list

  let print ppf t =
    match t with
    | Singleton sym ->
      Format.fprintf ppf "@[%a@ \u{2237}@ %a@]"
        Symbol.print sym
        K.print K.value
    | Sets_of_closures [set] ->
      Code_and_set_of_closures.print ppf set
    | Sets_of_closures sets ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Code_and_set_of_closures.print)
        sets

  let print_with_cache ~cache:_ ppf t = print ppf t

  (* CR mshinwell: This should have an [invariant] function.  One thing to
     check is that the [closure_symbols] are all distinct. *)

  let invariant _ _ = ()

  let being_defined t =
    match t with
    | Singleton sym -> Symbol.Set.singleton sym
    | Sets_of_closures sets ->
      Symbol.Set.union_list
        (List.map Code_and_set_of_closures.being_defined sets)

  let closure_symbols_being_defined t =
    match t with
    | Singleton _sym -> Symbol.Set.empty
    | Sets_of_closures sets ->
      Symbol.Set.union_list
        (List.map Code_and_set_of_closures.closure_symbols_being_defined sets)

  let code_being_defined t =
    match t with
    | Singleton _ -> Code_id.Set.empty
    | Sets_of_closures sets ->
      Code_id.Set.union_list
        (List.map Code_and_set_of_closures.code_being_defined sets)

  let everything_being_defined t =
    let code =
      Code_id.Set.fold (fun code_id code ->
          Code_id_or_symbol.Set.add (Code_id code_id) code)
        (code_being_defined t)
        Code_id_or_symbol.Set.empty
    in
    let closure_symbols =
      Symbol.Set.fold (fun symbol closure_symbols ->
          Code_id_or_symbol.Set.add (Symbol symbol) closure_symbols)
        (being_defined t)
        Code_id_or_symbol.Set.empty
    in
    Code_id_or_symbol.Set.union code closure_symbols

  let apply_name_permutation t _perm = t

  let free_names t =
    match t with
    | Singleton sym -> Name_occurrences.singleton_symbol sym Name_mode.normal
    | Sets_of_closures sets ->
      Name_occurrences.union_list
        (List.map Code_and_set_of_closures.free_names sets)
end

type t = {
  bound_symbols : Bound_symbols.t;
  defining_expr : Static_const.t;
  body : Expr.t;
}

let create bound_symbols defining_expr body =
  { bound_symbols;
    defining_expr;
    body;
  }

let bound_symbols t = t.bound_symbols
let defining_expr t = t.defining_expr
let body t = t.body

type flattened_for_printing_descr =
  | Code of Code_id.t * Static_const.code
  | Set_of_closures of Symbol.t Closure_id.Map.t * Set_of_closures.t
  | Other of Symbol.t * Static_const.t

type flattened_for_printing = {
  second_or_later_binding_within_rec : bool;
  second_or_later_set_of_closures : bool;
  descr : flattened_for_printing_descr;
}

let flatten_for_printing { bound_symbols; defining_expr; _ } =
  match bound_symbols with
  | Singleton symbol ->
    [{ second_or_later_binding_within_rec = false;
       second_or_later_set_of_closures = false;
       descr = Other (symbol, defining_expr);
    }]
  | Sets_of_closures bound_symbol_components ->
    let code_and_sets_of_closures =
      match defining_expr with
      | Sets_of_closures code_and_sets_of_closures ->
        code_and_sets_of_closures
      | _ ->
        Misc.fatal_errorf "Bad form of static constant:@ %a"
          Static_const.print defining_expr
    in
    let flattened, _ =
      List.fold_left2
        (fun (flattened_acc, second_or_later_set_of_closures)
             ({ code_ids = _; closure_symbols; }
                : Bound_symbols.Code_and_set_of_closures.t)
             ({ code; set_of_closures; }
                : Static_const.code_and_set_of_closures) ->
          let flattened,_ =
            Code_id.Map.fold (fun code_id code (flattened', first) ->
                let flattened =
                  { second_or_later_binding_within_rec = not first;
                    second_or_later_set_of_closures;
                    descr = Code (code_id, code);
                  }
                in
                flattened :: flattened', false)
              code
              ([], true)
          in
          let flattened' =
            match set_of_closures with
            | None -> []
            | Some set_of_closures ->
              let second_or_later_binding_within_rec =
                not (Code_id.Map.is_empty code)
              in
              let second_or_later_set_of_closures =
                second_or_later_set_of_closures
                  && not (Code_id.Map.is_empty code)
              in
              [{ second_or_later_binding_within_rec;
                 second_or_later_set_of_closures;
                 descr = Set_of_closures (closure_symbols, set_of_closures);
              }]
          in
          let flattened_acc =
            (List.rev flattened) @ (List.rev flattened') @ flattened_acc
          in
          flattened_acc, true)
        ([], false)
        bound_symbol_components code_and_sets_of_closures
    in
    flattened

let print_closure_binding ppf (closure_id, sym) =
  Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
    Symbol.print sym
    (Flambda_colours.elide ())
    (Flambda_colours.elide ())
    Closure_id.print closure_id

let print_flattened_descr_lhs ppf descr =
  match descr with
  | Code (code_id, _) -> Code_id.print ppf code_id
  | Set_of_closures (closure_symbols, _) ->
    Format.fprintf ppf "@[<hov 0>%a@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () ->
          Format.fprintf ppf "@<0>%s,@ @<0>%s"
            (Flambda_colours.elide ())
            (Flambda_colours.normal ()))
        print_closure_binding)
      (Closure_id.Map.bindings closure_symbols)
  | Other (symbol, _) -> Symbol.print ppf symbol

(* CR mshinwell: Use [print_with_cache]? *)
let print_flattened_descr_rhs ppf descr =
  match descr with
  | Code (_, code) -> Static_const.print_code ppf code
  | Set_of_closures (_, set) -> Set_of_closures.print ppf set
  | Other (_, static_const) -> Static_const.print ppf static_const

let print_flattened ppf
      { second_or_later_binding_within_rec;
        second_or_later_set_of_closures;
        descr;
      } =
  fprintf ppf "@[<hov 1>";
  if second_or_later_binding_within_rec then begin
    fprintf ppf "@<0>%sand%s @<0>%s"
      (Flambda_colours.elide ())
      (if second_or_later_set_of_closures then " [another set]" else "")
      (Flambda_colours.normal ())
  end;
  fprintf ppf
    "%a@<0>%s =@<0>%s@ %a@]"
    print_flattened_descr_lhs descr
    (Flambda_colours.elide ())
    (Flambda_colours.normal ())
    print_flattened_descr_rhs descr

let flatten t : _ * Expr.t =
  let rec flatten (expr : Expr.t) : _ * Expr.t =
    match Expr.descr expr with
    | Let_symbol t ->
      let flattened = flatten_for_printing t in
      let flattened', body = flatten t.body in
      flattened @ flattened', body
    | _ -> [], expr
  in
  let flattened = flatten_for_printing t in
  let flattened', body = flatten t.body in
  flattened @ flattened', body

let print_with_cache ~cache ppf t =
  let rec print_more flattened =
    match flattened with
    | [] -> ()
    | flat::flattened ->
      fprintf ppf "@ ";
      print_flattened ppf flat;
      print_more flattened
  in
  let flattened, body = flatten t in
  match flattened with
  | [] -> assert false
  | flat::flattened ->
    fprintf ppf "@[<v 1>(@<0>%slet_symbol@<0>%s@ (@[<v 0>%a"
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      print_flattened flat;
    print_more flattened;
    fprintf ppf "@])@ %a)@]" (Expr.print_with_cache ~cache) body

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant env { bound_symbols = _; defining_expr = _; body; } =
  (* Static_const.invariant env defining_expr; *) (* CR mshinwell: FIXME *)
  Expr.invariant env body

let free_names { bound_symbols; defining_expr; body; } =
  let from_bound_symbols = Bound_symbols.free_names bound_symbols in
  let from_defining_expr =
    match bound_symbols with
    | Singleton _ -> Static_const.free_names defining_expr
    | Sets_of_closures _ ->
      Name_occurrences.diff (Static_const.free_names defining_expr)
        from_bound_symbols
  in
  Name_occurrences.union from_defining_expr
    (Name_occurrences.diff (Expr.free_names body) from_bound_symbols)

let apply_name_permutation ({ bound_symbols; defining_expr; body; } as t) perm =
  let defining_expr' = Static_const.apply_name_permutation defining_expr perm in
  let body' = Expr.apply_name_permutation body perm in
  if defining_expr == defining_expr' && body == body' then t
  else
    { bound_symbols;
      defining_expr = defining_expr';
      body = body';
    }

(* CR mshinwell: Add a type to just encapsulate bound_symbols and
   defining_expr. *)
let pieces_of_code ?newer_versions_of ?set_of_closures code =
  let newer_versions_of =
    Option.value newer_versions_of ~default:Code_id.Map.empty
  in
  let code =
    Code_id.Map.mapi (fun id params_and_body : Static_const.code ->
        let newer_version_of =
          Code_id.Map.find_opt id newer_versions_of
        in
        { params_and_body = Present params_and_body;
          newer_version_of;
        })
      code
  in
  let static_const : Static_const.t =
    let set_of_closures = Option.map snd set_of_closures in
    Sets_of_closures [{
      code;
      set_of_closures;
    }]
  in
  let bound_symbols : Bound_symbols.t =
    let closure_symbols =
      Option.value (Option.map fst set_of_closures)
        ~default:Closure_id.Map.empty
    in
    Sets_of_closures [{
      code_ids = Code_id.Map.keys code;
      closure_symbols;
    }]
  in
  bound_symbols, static_const
