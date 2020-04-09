(* WebAssembly-compatible i64 implementation *)

include Wasm_int.Make
  (struct
    include Int64
    let bitwidth = 64
  end)
