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

let codegen_proto = function
  | `Prototype (name, args) ->
      let ft = function_type double_type [| |] in
      declare_function name ft the_module

let codegen_func = function
  | `Function (proto, body) ->
      let lproto = codegen_proto proto in
      let bb = append_block context "entry" lproto in
      position_at_end bb builder;
      let _ = build_ret (codegen_expr body) builder in
      lproto
