module Make () =
struct
  exception Error of Wasm_source.region * string

  let warn at m = prerr_endline (Wasm_source.string_of_region at ^ ": warning: " ^ m)
  let error at m = raise (Error (at, m))
end