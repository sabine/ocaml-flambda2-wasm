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

(** Language terms that represent statically-allocated values. *)

module Program_body : sig


  module Static_structure : sig
    (** The bindings in a [Static_structure] are ordered: symbols bound later
        in the list cannot be referred to by [Static_part]s earlier in the
        list.
        Allowing multiple bindings here enables several static parts to use
        the same results from a given [computation]. *)
    type t0 =
      | S : 'k Bound_symbols.t * 'k Static_part.t -> t0

    type t = t0 list

    (* CR mshinwell: Add a creation function *)
    (* CR mshinwell: make [t] abstract *)

    val print : Format.formatter -> t -> unit

    val is_empty : t -> bool

    val being_defined : t -> Symbol.Set.t

    val delete_bindings
       : t
      -> free_names_after:Name_occurrences.t
      -> Code_age_relation.t
      -> t

    val free_names : t -> Name_occurrences.t

    (** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
        version of [id2]. *)
    val pieces_of_code
       : ?newer_versions_of:Code_id.t Code_id.Map.t
      -> ?set_of_closures:
           (Symbol.t Closure_id.Map.t * Flambda.Set_of_closures.t)
      -> Flambda.Function_params_and_body.t Code_id.Map.t
      -> t0
  end

  module Definition : sig


    val print : Format.formatter -> t -> unit

    val singleton_symbol : Symbol.t -> Flambda_kind.value Static_part.t -> t

    (** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
        version of [id2]. *)
    val pieces_of_code
       : ?newer_versions_of:Code_id.t Code_id.Map.t
      -> Flambda.Function_params_and_body.t Code_id.Map.t
      -> t

    val get_pieces_of_code
       : t
      -> (Flambda.Function_params_and_body.t * (Code_id.t option)) Code_id.Map.t

    val being_defined : t -> Symbol.Set.t

    val code_being_defined : t -> Code_id.Set.t

    val iter_computation : t -> f:(Computation.t -> unit) -> unit

    val map_computation : t -> f:(Computation.t -> Computation.t) -> t

    val iter_static_parts : t -> static_part_iterator -> unit

    val map_static_parts : t -> static_part_mapper -> t
  end

  type t

  (** Print a list of symbol definitions to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** Define the given symbol(s).  No symbol defined by the
      [definition] may be referenced by the same definition, only by
      subsequent [define_symbol] constructs. *)
  val define_symbol : Definition.t -> body:t -> Code_age_relation.t -> t

  (** The module block symbol for the compilation unit. *)
  val root : Symbol.t -> t

  val free_names : t -> Name_occurrences.t

  val iter_definitions : t -> f:(Definition.t -> unit) -> unit

  type descr = private
    (* CR mshinwell: Rename [Definition] to [Definition].  It doesn't
       always define a symbol now. *)
    | Definition of Definition.t * t
    | Root of Symbol.t

  val descr : t -> descr
end

(** A "program" is the contents of one compilation unit.  It describes the
    various values that are assigned to symbols in the object file. *)
module Program : sig
  type t = {
    imported_symbols : Flambda_kind.t Symbol.Map.t;
    exn_continuation : Continuation.t;
    body : Expr.t;
  }

  (** Perform well-formedness checks on the program. *)
  val invariant : t -> unit

  (** Print a program to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** All free names in the given program.  Imported symbols are not treated
      as free. *)
  val free_names : t -> Name_occurrences.t

  val used_closure_vars : t -> Var_within_closure.Set.t

  (** All symbols imported from other compilation units by the given program. *)
  val imported_symbols : t -> Flambda_kind.t Symbol.Map.t

  (** The module block symbol for the given program (the only symbol that
      can never be eliminated). *)
  val root_symbol : t -> Symbol.t

  val iter_body : t -> f:(Program_body.t -> unit) -> unit

  val map_body : t -> f:(Program_body.t -> Program_body.t) -> t
end
