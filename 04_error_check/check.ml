exception Error of string

let get_pcount = function `Prototype (fname, params) -> List.length params

let rec check_expr fenv venv = function
  | `Number _ -> ()
  | `BinOp (op, e1, e2) -> 
          begin
              check_expr fenv venv e1; 
              check_expr fenv venv e2
          end
  | `Variable x -> 
          if in_venv venv x 
          then () 
          else raise (Error "Variable not defined: " ^ x)
  | `Call (fname, es) ->
          if not in_fenv fenv fname
          then raise (Error "Function not defined: " ^ fname)
          else 
              let pcount = get_pcount (apply_fenv fenv fname) in
              let acount = List.length es in
              if not pcount = acount
              then raise (Error "Function " ^ fname ^ " called with incorrect number of args")
              else List.iter (check_expr fenv venv) es

let check_func_name fenv = function
  | `Prototype (name, _) ->
          if not (name = "") && in_fenv fenv name
          then raise (Error "Function name conflict: " ^ name)
          else ()

let check_func_body fenv pt body = match pt with
  | `Prototype (_, params) ->
          let venv = make_venv params in
          check_expr fenv venv body

let check_func fenv = function
  | `Function (p, b) ->
          begin
              check_func_name fenv p;
              check_func_body fenv p b;
              extend_fenv fenv p
          end

let check_toplevel fenv = function
  | `TLMain func | `TLFunction func -> check_func fenv func

