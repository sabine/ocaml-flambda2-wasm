type _ heterogeneous_list =
  | Nil: 'a heterogeneous_list
  | Cons: (('a * ('a -> string)) * 'b heterogeneous_list) -> 'a heterogeneous_list

let rec len : type a. a heterogeneous_list -> int = function
  | Nil -> 0
  | Cons (h, t) -> 1 + len t

let xs = Cons ((4, string_of_int), Cons (("abc", fun x -> "\"" ^ x ^ "\""), Nil))

let l = len (xs)

let rec print : type a. a heterogeneous_list -> string = fun hs -> match hs with
  | Nil -> "[]"
  | Cons ((h, printer), t) -> printer h ^ "::" ^ print t
