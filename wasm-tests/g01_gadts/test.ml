type 'a t =
  | Int : int t
  | Foo : int list t


(* some function which itself (or simply whose call) can trigger a gc *)
let bar () =
  Format.printf "some logging mechanism ...@."


let foo (type a) (w: a t) (x: a) =
  let () = bar () in (* if we compile this as one function, here, x is in the frame of foo,
                        but we do not yet know whether it is an int or a pointer *)
  match w with
  | Int -> print_int x
  | Foo -> ()

let () =
  foo Int 42
