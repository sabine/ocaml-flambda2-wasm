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

(** Values of type [t] represent alpha-equivalence classes of the definitions
    of continuations:
      let_cont [name] [args] = [handler] in [body]
    or using an alternative notation:
      [body]
      where [name] [args] = [handler]

    - Continuations are second-class.
    - Continuations do not capture variables.
    - Continuations may be (mutually-)recursive.

    It is an error to mark a continuation that might be recursive as
    non-recursive. The converse is safe.

    Note: any continuation used as an exception handler must be non-recursive
    by the point it reaches [Flambda_to_cmm]. (This means that it is
    permissible to introduce mutual recursion through stubs associated with
    such continuations, so long as [Simplify] is run afterwards to inline them
    out and turn the resulting single [Recursive] handler into a
    [Non_recursive] one. *)
(* CR mshinwell: ensure the statement about [Flambda_to_cmm] still holds. *)
type t = private
  | Non_recursive of {
      handler : Non_recursive_let_cont_handler.t;
      num_free_occurrences : Name_occurrences.Num_occurrences.t;
      (** [num_free_occurrences] can be used, for example, to decide whether
          to inline out a linearly-used continuation. *)
    }
  | Recursive of Recursive_let_cont_handlers.t

(** Printing, invariant checks, name manipulation, etc. *)
include Expr_std.S with type t := t

include Contains_ids.S with type t := t

(** Create a definition of a non-recursive continuation.  If the continuation
    does not occur free in the [body], then just the [body] is returned,
    without any enclosing [Let_cont]. *)
val create_non_recursive
   : Continuation.t
  -> Continuation_handler.t
  -> body:Expr.t
  -> Expr.t

(** Create a definition of a set of possibly-recursive continuations. *)
val create_recursive
   : Continuation_handler.t Continuation.Map.t
  -> body:Expr.t
  -> Expr.t
