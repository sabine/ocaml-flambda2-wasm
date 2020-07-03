type 'a tid = .. ;;

module type S = sig type t type 'a tid += T : t tid end ;;
let create (type v) () =
  let module V = struct
    type t = v
    type 'a tid += T : t tid
  end in (module V : S with type t = v) ;;

