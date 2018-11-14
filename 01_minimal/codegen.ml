open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "minimal"
let builder = builder context
let double_type = double_type context

let codegen_expr = function
  | `Number n -> const_float double_type n
