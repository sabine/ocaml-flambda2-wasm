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

[@@@ocaml.warning "+a-30-40-41-42"]

type t = {
  imported_symbols : Flambda_kind.t Symbol.Map.t;
  root_symbol : Symbol.t;
  return_continuation : Continuation.t;
  exn_continuation : Continuation.t;
  body : Flambda.Expr.t;
}

let create ~imported_symbols ~root_symbol ~return_continuation
      ~exn_continuation ~body =
  { imported_symbols;
    root_symbol;
    return_continuation;
    exn_continuation;
    body;
  }

let imported_symbols t = t.imported_symbols
let root_symbol t = t.root_symbol
let return_continuation t = t.return_continuation
let exn_continuation t = t.exn_continuation
let body t = t.body

let print ppf
      { imported_symbols; root_symbol; return_continuation; exn_continuation;
        body;
      } =
  Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(imported_symbols@ %a)@]@ \
        @[<hov 1>(root_symbol@ %a)@]@ \
        @[<hov 1>(return_continuation@ %a)@]@ \
        @[<hov 1>(exn_continuation@ %a)@]@ \
        @[<hov 1>(body@ %a)@]\
      )@]"
    (Symbol.Map.print Flambda_kind.print) imported_symbols
    Symbol.print root_symbol
    Continuation.print return_continuation
    Continuation.print exn_continuation
    Flambda.Expr.print t.body

let invariant _t = ()

let used_closure_vars t =
  Name_occurrences.closure_vars (Flambda.Expr.free_names t.body)
