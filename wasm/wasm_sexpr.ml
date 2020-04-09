(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   original code by Andreas Rossberg from WebAssembly/spec/interpreter  *)
(*   adapted for use in the OCaml compiler by Sander Spies                *)
(*   modified slightly by Sabine Schmaltz, Tarides                        *)
(*                                                                        *)
(*   Licensed under the Apache License, Version 2.0 (the "License");      *)
(*   you may not use this file except in compliance with the License.     *)
(*   You may obtain a copy of the License at                              *)
(*     https://www.apache.org/licenses/LICENSE-2.0                        *)
(*                                                                        *)
(**************************************************************************)

type sexpr = Atom of string | Node of string * sexpr list

type rope = Leaf of string | Concat of rope list
let (^+) s r = Concat [Leaf s; r]
let (+^) r s = Concat [r; Leaf s]

let rec iter f = function
  | Leaf s -> f s
  | Concat rs -> List.iter (iter f) rs

let rec concat = function
  | Leaf s -> s
  | Concat rs -> String.concat "" (List.map concat rs)

let rec pp off width = function
  | Atom s -> String.length s, Leaf s
  | Node (s, xs) ->
    let lens, rs = List.split (List.map (pp (off + 2) width) xs) in
    let len = String.length s + List.length rs + List.fold_left (+) 2 lens in
    let sep, fin =
      if off + len <= width then " ", ""
      else let indent = String.make off ' ' in "\n  " ^ indent, "\n" ^ indent
    in len, "(" ^+ s ^+ Concat (List.map (fun r -> sep ^+ r) rs) +^ fin +^ ")"

let output oc width x =
  iter (output_string oc) (snd (pp 0 width x));
  output_string oc "\n";
  flush oc

let print = output stdout

let to_string width x = concat (snd (pp 0 width x)) ^ "\n"
