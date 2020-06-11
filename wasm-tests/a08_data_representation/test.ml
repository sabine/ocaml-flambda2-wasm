(* https://xavierleroy.org/publi/data-representation-plilp.pdf *)

let map_pair f z = (f (fst z), f (snd z))

let int_of_float2 (x : float) : int = int_of_float x

let z = map_pair int_of_float2 (3.14, 2.718)
