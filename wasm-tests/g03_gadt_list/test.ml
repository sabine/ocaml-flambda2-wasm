type 'a printer = Format.formatter -> 'a -> unit

type _ hlist =
  | Nil: 'a hlist
  | Cons: (('a * 'a printer) * 'b hlist) -> 'a hlist

let rec len : type a. a hlist -> int = function
  | Nil -> 0
  | Cons (h, t) -> 1 + len t


let xs = Cons ((4, function ppf -> Format.fprintf ppf "%d"), Cons (("abc",  function ppf -> Format.fprintf ppf "%s"), Nil))
let l = len (xs)


let rec print : type a. a hlist printer = fun ppf hs -> match hs with
  | Nil -> ()
  | Cons ((h, printer), t) -> Format.fprintf ppf "%a@ %a" printer h print t