module Ast = Wasm_ast

type var = string

type definition =
  | Textual of Ast.module_
  | Encoded of string * string
  | Quoted of string * string

type action =
  | Invoke of var option * Ast.name * Ast.literal list
  | Get of var option * Ast.name

type assertion =
  | AssertMalformed of definition * string
  | AssertInvalid of definition * string
  | AssertUnlinkable of definition * string
  | AssertUninstantiable of definition * string
  | AssertReturn of action * Ast.literal list
  | AssertReturnCanonicalNaN of action
  | AssertReturnArithmeticNaN of action
  | AssertTrap of action * string
  | AssertExhaustion of action * string

type command =
  | Module of var option * definition
  | Register of Ast.name * var option
  | Action of action
  | Assertion of assertion
  | Meta of meta

and meta =
  | Input of var option * string
  | Output of var option * string option
  | Script of var option * script

and script = command list

exception Syntax of string
