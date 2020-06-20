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

(** The representation of the application of an OCaml function, OCaml method
    or external call to a list of arguments. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Create an application expression. *)
val create
   : callee:Simple.t
  -> continuation:Continuation.t
  -> Exn_continuation.t
  -> args:Simple.t list
  -> call_kind:Call_kind.t
  -> Debuginfo.t
  -> inline:Inline_attribute.t
  -> inlining_depth:int
  -> t

(** The function or method being applied. *)
val callee : t -> Simple.t

(** The arguments of the function or method being applied. *)
val args : t -> Simple.t list

(** Information about what kind of call is involved (direct function call,
    method call, etc). *)
val call_kind : t -> Call_kind.t

(** Where to send the result of the application. *)
val continuation : t -> Continuation.t

(** Where to jump to upon the application raising an exception. *)
val exn_continuation : t -> Exn_continuation.t

(** Debugging information attached to the application. *)
val dbg : t -> Debuginfo.t

(** Instructions from the source code as to whether the callee should be
    inlined. *)
val inline : t -> Inline_attribute.t

(** Change the return continuation of an application. *)
val with_continuation : t -> Continuation.t -> t

val with_continuations : t -> Continuation.t -> Exn_continuation.t -> t

val with_exn_continuation : t -> Exn_continuation.t -> t

(** Change the call kind of an application. *)
val with_call_kind : t -> Call_kind.t -> t

(** Change the continuation, callee and arguments of an application. *)
val with_continuation_callee_and_args
   : t
  -> Continuation.t
  -> callee:Simple.t
  -> args:Simple.t list
  -> t

val inlining_depth : t -> int
