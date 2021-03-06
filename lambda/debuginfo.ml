(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Int_replace_polymorphic_compare
open Lexing
open Location

type item = {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Lambda.lambda_scopes;
}

type t = item list

type alloc_dbginfo_item =
  { alloc_words : int;
    alloc_dbg : t }
type alloc_dbginfo = alloc_dbginfo_item list

let none = []

let is_none = function
  | [] -> true
  | _ :: _ -> false

let to_string dbg =
  match dbg with
  | [] -> ""
  | ds ->
    let items =
      List.map
        (fun d ->
           Printf.sprintf "%s:%d,%d-%d"
             d.dinfo_file d.dinfo_line d.dinfo_char_start d.dinfo_char_end)
        ds
    in
    "{" ^ String.concat ";" items ^ "}"

let item_from_location ~scopes loc =
  let valid_endpos =
    String.equal loc.loc_end.pos_fname loc.loc_start.pos_fname in
  { dinfo_file = loc.loc_start.pos_fname;
    dinfo_line = loc.loc_start.pos_lnum;
    dinfo_char_start = loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_char_end =
      if valid_endpos
      then loc.loc_end.pos_cnum - loc.loc_start.pos_bol
      else loc.loc_start.pos_cnum - loc.loc_start.pos_bol;
    dinfo_start_bol = loc.loc_start.pos_bol;
    dinfo_end_bol =
      if valid_endpos then loc.loc_end.pos_bol
      else loc.loc_start.pos_bol;
    dinfo_end_line =
      if valid_endpos then loc.loc_end.pos_lnum
      else loc.loc_start.pos_lnum;
    dinfo_scopes = scopes
  }

let from_location = function
  | Lambda.Loc_unknown -> []
  | Lambda.Loc_known {scopes; loc} ->
    assert (loc != Location.none);
    [item_from_location ~scopes loc]

let to_location = function
  | [] -> Location.none
  | d :: _ ->
    let loc_start =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_line;
        pos_bol = d.dinfo_start_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_start;
      } in
    let loc_end =
      { pos_fname = d.dinfo_file;
        pos_lnum = d.dinfo_end_line;
        pos_bol = d.dinfo_end_bol;
        pos_cnum = d.dinfo_start_bol + d.dinfo_char_end;
      } in
    { loc_ghost = false; loc_start; loc_end; }

let concat dbg1 dbg2 =
  dbg1 @ dbg2

(* CR-someday afrisch: FWIW, the current compare function does not seem very
   good, since it reverses the two lists. I don't know how long the lists are,
   nor if the specific currently implemented ordering is useful in other
   contexts, but if one wants to use Map, a more efficient comparison should
   be considered. *)
let compare dbg1 dbg2 =
  let rec loop ds1 ds2 =
    match ds1, ds2 with
    | [], [] -> 0
    | _ :: _, [] -> 1
    | [], _ :: _ -> -1
    | d1 :: ds1, d2 :: ds2 ->
      let c = String.compare d1.dinfo_file d2.dinfo_file in
      if c <> 0 then c else
      let c = compare d1.dinfo_line d2.dinfo_line in
      if c <> 0 then c else
      let c = compare d1.dinfo_char_end d2.dinfo_char_end in
      if c <> 0 then c else
      let c = compare d1.dinfo_char_start d2.dinfo_char_start in
      if c <> 0 then c else
      let c = compare d1.dinfo_start_bol d2.dinfo_start_bol in
      if c <> 0 then c else
      let c = compare d1.dinfo_end_bol d2.dinfo_end_bol in
      if c <> 0 then c else
      let c = compare d1.dinfo_end_line d2.dinfo_end_line in
      if c <> 0 then c else
      loop ds1 ds2
  in
  loop (List.rev dbg1) (List.rev dbg2)

let hash t =
  List.fold_left (fun hash item -> Hashtbl.hash (hash, item)) 0 t

let rec print_compact ppf t =
  let print_item item =
    Format.fprintf ppf "%a:%i"
      Location.print_filename item.dinfo_file
      item.dinfo_line;
    if item.dinfo_char_start >= 0 then begin
      Format.fprintf ppf ",%i--%i" item.dinfo_char_start item.dinfo_char_end
    end
  in
  match t with
  | [] -> ()
  | [item] -> print_item item
  | item::t ->
    print_item item;
    Format.fprintf ppf ";";
    print_compact ppf t

(* CR mshinwell: read the formatter margin? *)
let print_compact ppf t =
  let str = Format.asprintf "%a" print_compact t in
  if String.length str < 300000000 then print_compact ppf t
  else begin
    let t =
      match t with
      | [] | [_] -> t
      | item::_ -> [item]
    in
    print_compact ppf t;
    Format.fprintf ppf "; ..."
  end

let print_or_elide ppf t =
  if not (is_none t) then begin
    Format.fprintf ppf "@ @[<h><%a>@]" print_compact t
  end

(* CR mshinwell: provide an sexp printer here *)
let print ppf t =
  if is_none t then Format.pp_print_string ppf "None"
  else print_compact ppf t
