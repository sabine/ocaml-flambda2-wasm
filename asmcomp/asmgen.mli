(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** From Lambda to assembly code *)

(** The type of converters from Lambda to Clambda. *)
type middle_end =
     backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> Clambda.with_constants

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation
   : ?toplevel:(string -> bool)
  -> backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> middle_end:middle_end
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> unit

(** The type of converters from Lambda to Flambda2 programs *)
type middle_end2 =
     ppf_dump:Format.formatter
  -> prefixname:string
  -> backend:(module Flambda2_backend_intf.S)
  -> filename:string
  -> module_ident:Ident.t
  -> module_block_size_in_words:int
  -> module_initializer:Lambda.lambda
  -> Flambda_unit.t

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation2
   : ?toplevel:(string -> bool)
  -> backend:(module Flambda2_backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> size:int
  -> module_ident:Ident.t
  -> module_initializer:Lambda.lambda
  -> middle_end:middle_end2
  -> ppf_dump:Format.formatter
  -> Ident.Set.t (* CR mshinwell: label this *)
  -> unit

(** Flambda2 backend *)
module Flambda2_backend : Flambda2_backend_intf.S

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation_flambda
   : ?toplevel:(string -> bool)
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> required_globals:Ident.Set.t
  -> Flambda_unit.t
  -> unit

val compile_phrase :
    ppf_dump:Format.formatter -> Cmm.phrase -> unit

type error = Assembler_error of string
exception Error of error
val report_error: Format.formatter -> error -> unit


val compile_unit:
  string(*asm file*) -> bool(*keep asm*) ->
  string(*obj file*) -> (unit -> unit) -> unit
