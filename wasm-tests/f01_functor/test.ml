module type Test = sig
  type t
  val x : t
  val hello : t -> t
end

module Hello (S: Test) = struct
  let f y = S.hello y
end

module A = struct
  type t = int
  let x = 40
  let hello a = a + x
end

module AHello = Hello (A)

let a = AHello.f 2

module B = struct
  type t = int32
  let x = 40l
  let hello a = Int32.add a x
end

module BHello = Hello (B)

let b = BHello.f 2l
