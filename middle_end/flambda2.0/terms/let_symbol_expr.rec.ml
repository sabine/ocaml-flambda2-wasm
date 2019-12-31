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

module Bound_symbols = struct
  type t =
    | Singleton : Symbol.t -> t
    | Code_and_set_of_closures of {
        code_ids : Code_id.Set.t;
        closure_symbols : Symbol.t Closure_id.Map.t;
      }

  (* CR mshinwell: Share with [Bindable_let_bound] *)
  let print_closure_binding ppf (closure_id, sym) =
    Format.fprintf ppf "@[(%a \u{21a6} %a)@]"
      Closure_id.print closure_id
      Symbol.print sym

  let print ppf t =
    match t with
    | Singleton sym ->
      Format.fprintf ppf "@[%a@ \u{2237}@ %a@]"
        Symbol.print sym
        K.print K.value
    | Code_and_set_of_closures { code_ids; closure_symbols; } ->
      Format.fprintf ppf "@[<hov 1>\
          @[<hov 1>(code_ids@ %a)@]@ \
          @[<hov 1>(closure_symbols@ {%a})@]\
          @]"
        Code_id.Set.print code_ids
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          print_closure_binding)
        (Closure_id.Map.bindings closure_symbols)

  (* CR mshinwell: This should have an [invariant] function.  One thing to
     check is that the [closure_symbols] are all distinct. *)

  let being_defined t =
    match t with
    | Singleton sym -> Symbol.Set.singleton sym
    | Code_and_set_of_closures { code_ids = _; closure_symbols; } ->
      Symbol.Set.of_list (Closure_id.Map.data closure_symbols)

  let closure_symbols_being_defined t =
    match t with
    | Singleton _sym -> Symbol.Set.empty
    | Code_and_set_of_closures { code_ids = _; closure_symbols; } ->
      Symbol.Set.of_list (Closure_id.Map.data closure_symbols)

  let code_being_defined t =
    match t with
    | Singleton _ -> Code_id.Set.empty
    | Code_and_set_of_closures { code_ids; closure_symbols = _; } -> code_ids

  let apply_name_permutation t _perm = t

  let free_names t =
    match t with
    | Singleton sym -> Name_occurrences.singleton_symbol sym
    | Code_and_set_of_closures { code_ids; closure_symbols; } ->
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

let print_with_cache ~cache ppf ({ bound_symbols; defining_expr; body; } as t) =
  let rec let_body (expr : Expr.t) =
    match Expr.descr expr with
    | Let ({ bound_symbols; defining_expr; body; } as t) ->
      fprintf ppf
        "@ @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
        (Flambda_colours.let_bound_symbol ())
        Bound_symbols.print bound_symbols
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        (Named.print_with_cache ~cache) defining_expr;
      let_body body
    | _ -> expr
  in
  fprintf ppf "@[<v 1>(@<0>%slet_symbol@<0>%s@ (@[<v 0>\
      @[<hov 1>@<0>%s%a@<0>%s =@<0>%s@ %a@]"
    (Flambda_colours.expr_keyword ())
    (Flambda_colours.normal ())
    (Flambda_colours.let_bound_symbol ())
    Bound_symbols.print bound_symbols
    (Flambda_colours.elide ())
    (Flambda_colours.normal ())
    (Static_const.print_with_cache ~cache) defining_expr;
  let expr = let_body body in
  fprintf ppf "@])@ %a)@]" (Expr.print_with_cache ~cache) body

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let invariant env { bound_symbols = _; defining_expr; body; } =
  Named.invariant env defining_expr;
  Expr.invariant env body

let free_names { bound_symbols; defining_expr; body; } =
  let from_bound_symbols = Bound_symbols.free_names bound_symbols in
  let from_defining_expr =
    match defining_expr with
    | Singleton sym -> Name_occurrences.singleton_symbol sym
    | Code_and_set_of_closures _ ->
      Name_occurrences.diff (Static_const.free_names defining_expr)
        from_bound_symbols
  in
  Name_occurrences.union from_defining_expr
    (Name_occurrences.diff (Expr.free_names body) from_bound_symbols

let apply_name_permutation ({ bound_symbols; defining_expr; body; } as t) perm =
  let defining_expr' = Static_const.apply_name_permutation defining_expr perm in
  let body' = Expr.apply_name_permutation body perm in
  if defining_expr == defining_expr' && body == body' then t
  else
    { bound_symbols;
      defining_expr;
      body;
    }
