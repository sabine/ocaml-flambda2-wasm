type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let iter f seq =
  let rec aux seq = match seq () with
    | Nil -> ()
    | Cons (x, next) ->
        f x;
        aux next
  in
  aux seq
