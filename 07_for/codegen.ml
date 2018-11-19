open Llvm

exception Error of string

let context = global_context ()
let the_module = create_module context "minimal"
let builder = builder context
let var_env:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context

let create_entry_block_alloca f var =
    let builder = builder_at context (instr_begin (entry_block f)) in
    build_alloca double_type var builder

exception InvalidOperator of char
let rec codegen_expr = function
  | `Variable var ->
        let v = (try Hashtbl.find var_env var
                 with Not_found -> raise (Error "unknown variable name"))
        in
        build_load v var builder
  | `Number n -> const_float double_type n
  | `BinOp ('=', e1, e2) ->
          let var = match e1 with
                      | `Variable var -> var
                      | _ -> raise (Error "Assignment to non-variable")
          in
          let val_ = codegen_expr e2 in
          let v = (try Hashtbl.find var_env var
                   with Not_found -> raise (Error "unknown variable name"))
          in
          ignore (build_store val_ v builder);
          val_
  | `BinOp (':', e1, e2) ->
        let _ = codegen_expr e1 in
        codegen_expr e2
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
  | `Var (var, o, e) ->
          let v = match o with
                    | None -> const_float double_type 0.0
                    | Some x -> codegen_expr x
          in
          let old_v = Hashtbl.find_opt var_env var in
          let f = block_parent (insertion_block builder) in
          let alloca = create_entry_block_alloca f var in
          ignore (build_store v alloca builder);
          Hashtbl.add var_env var alloca;
          let body = codegen_expr e in
          (match old_v with
            | Some ov -> Hashtbl.add var_env var ov
            | None -> ());
          body
  | `For (var, start_, end_, update_, body_) ->
          let f = block_parent (insertion_block builder) in
          let alloca = create_entry_block_alloca f var in
          let start_val = codegen_expr start_ in
          ignore (build_store start_val alloca builder);

          let loop_bb = append_block context "loop" f in
          ignore (build_br loop_bb builder);
          position_at_end loop_bb builder;

          let old_val = Hashtbl.find_opt var_env var in
          Hashtbl.add var_env var alloca;

          ignore (codegen_expr body_);

          let update_val = match update_ with
            | Some u -> codegen_expr u
            | None -> const_float double_type 1.0
          in

          let end_val = codegen_expr end_ in

          let cur_val = build_load alloca var builder in
          let next_val = build_add cur_val update_val "nextval" builder in
          ignore (build_store next_val alloca builder);

          let zero = const_float double_type 0.0 in
          let end_cond = build_fcmp Fcmp.One end_val zero "loopcond" builder in
          let after_bb = append_block context "afterloop" f in
          ignore (build_cond_br end_cond loop_bb after_bb builder);
          position_at_end after_bb builder;

          (match old_val with
             | Some v -> Hashtbl.add var_env var v
             | None -> ());

          const_null double_type

  | _ -> raise (Error "Encountered unknown expr type")

let codegen_proto = function
  | `Prototype (name, args) ->
      let arg_types = Array.make (List.length args) double_type in
      let ft = function_type double_type arg_types in
      let f = declare_function name ft the_module in
      let create_var n a = (set_value_name n a; Hashtbl.add var_env n a) in
      (Array.iter2 create_var (Array.of_list args) (params f); f)

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
