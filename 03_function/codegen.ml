open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "minimal"
let builder = builder context
let double_type = double_type context

exception InvalidOperator of char
let rec codegen_expr = function
  | `Number n -> const_float double_type n
  | `BinOp (op, e1, e2) ->
        let lhs = codegen_expr e1 in
        let rhs = codegen_expr e2 in
        (match op with
           | '+' -> build_add lhs rhs "addtmp" builder
           | '-' -> build_sub lhs rhs "subtmp" builder
           | '*' -> build_mul lhs rhs "multmp" builder
           | _ -> raise (InvalidOperator op))
