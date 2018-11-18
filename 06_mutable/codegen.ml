open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "minimal"
let builder = builder context
let var_env:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

exception InvalidOperator of char
let rec codegen_expr = function
  | `Variable var ->
        let v = (try Hashtbl.find var_env var
                 with Not_found -> raise (Error "unknown variable name"))
        in
        build_load v var builder
  | `Number n -> const_float double_type n
  | `BinOp (op, e1, e2) ->
        let lhs = codegen_expr e1 in
        let rhs = codegen_expr e2 in
        (match op with
           | '+' -> build_add lhs rhs "addtmp" builder
           | '-' -> build_sub lhs rhs "subtmp" builder
           | '*' -> build_mul lhs rhs "multmp" builder
           | '<' -> 
                   let cmp = build_fcmp Fcmp.Ult lhs rhs "cmptmp" builder in
                   build_uitofp cmp double_type "booltmp" builder
           | _ -> raise (InvalidOperator op))
  | `Call (fname, args) ->
        let f =
            (match lookup_function fname the_module with
               | Some x -> x
               | None ->  raise (Error "Unknown function name"))
        in
        let a = Array.of_list (List.map codegen_expr args) in
        build_call f a "calltmp" builder
  | `If (cond_, then_, else_) ->
          let cond = codegen_expr cond_ in
          let zero = const_float double_type 0.0 in
          let cond_val = build_fcmp Fcmp.One cond zero "ifcond" builder in

          let start_bb = insertion_block builder in
          let the_function = block_parent start_bb in

          let then_bb = append_block context "then" the_function in
          position_at_end then_bb builder;
          let then_val = codegen_expr then_ in
          let new_then_bb = insertion_block builder in

          let else_bb = append_block context "else" the_function in
          position_at_end else_bb builder;
          let else_val  = codegen_expr else_ in
          let new_else_bb = insertion_block builder in

          let merge_bb = append_block context "ifcont" the_function in
          position_at_end merge_bb builder;
          let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
          let phi = build_phi incoming "iftmp" builder in

          position_at_end start_bb builder;
          ignore (build_cond_br cond_val then_bb else_bb builder);

          position_at_end new_then_bb builder;
          ignore (build_br merge_bb builder);

          position_at_end new_else_bb builder;
          ignore (build_br merge_bb builder);

          position_at_end merge_bb builder;

          phi

  | _ -> raise (Error "Encountered unknown expr type")

let codegen_proto = function
  | `Prototype (name, args) ->
      let arg_types = Array.make (List.length args) double_type in
      let ft = function_type double_type arg_types in
      let f = declare_function name ft the_module in
      let create_var n a = (set_value_name n a; Hashtbl.add var_env n a) in
      (Array.iter2 create_var (Array.of_list args) (params f); f)

let create_entry_block_alloca f var =
    let builder = builder_at context (instr_begin (entry_block f)) in
    build_alloca double_type var builder

let create_argument_allocas f p =
    let args = match p with
                 | `Prototype (name, args) -> args
    in
    let g var v =
        let alloca = create_entry_block_alloca f var in
        ignore (build_store v alloca builder);
        Hashtbl.add var_env var alloca
    in
    Array.iter2 g (Array.of_list args) (params f)

let codegen_func = function
  | `Function (proto, body) ->
      let f = codegen_proto proto in
      let bb = append_block context "entry" f in
      position_at_end bb builder;
      create_argument_allocas f proto;
      let _ = build_ret (codegen_expr body) builder in
      f
