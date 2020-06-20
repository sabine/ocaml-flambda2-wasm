(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2020 OCamlPro SAS                                    *)
(*   Copyright 2017--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Numbers.Int

let initial = 0

let next t =
  t + 1

let prev t =
  if t <= initial then begin
    Misc.fatal_error "Cannot decrement continuation level past the \
      initial level"
  end;
  t - 1

let (<=) (t1 : t) t2 = t1 <= t2
let (<) (t1 : t) t2 = t1 < t2
let (>) (t1 : t) t2 = t1 > t2
let (>=) (t1 : t) t2 = t1 >= t2

let to_int t = t

let max t1 t2 = max t1 t2

module Set = Patricia_tree.Make_set (struct let print = print end)
module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)
