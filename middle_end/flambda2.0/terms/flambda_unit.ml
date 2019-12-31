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
  exn_continuation : Continuation.t;
  body : Expr.t;
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

let print ppf t =
  Format.fprintf ppf "@[(@[(imported_symbols %a)@]@ @[<1>(body@ %a)@])@]"
    (Symbol.Map.print K.print) t.imported_symbols
    Program_body.print t.body

let invariant _t = ()

let used_closure_vars t = Program_body.used_closure_vars t.body
