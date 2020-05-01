let a = ref 10

let _ = while !a > 0 do
  a := !a - 1
done

let b = !a
