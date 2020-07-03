type ('a, 'b) t =
  | Int : (int, string) t
  | Foo : (int list -> 'b) -> (int list, 'b) t

(* some function which itself (or simply whose call) can trigger a gc *)
let bar () =
  Format.printf "some logging mechanism ...@."


let rec foo (type a) (type b) (w: (a, b) t) (x: a) (y: b) =
  let () = bar () in (* here, x is in the frame of foo,
                        but we do not yet know whether it is an int or a pointer *)
  match w with
  | Int -> 
    let () = print_int x in
    y
  | Foo (f) ->
    f x

let m =
  foo Int 42

let n =
  foo (Foo (fun x -> x)) [1;2;3]
