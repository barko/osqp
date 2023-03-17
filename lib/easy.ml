include Raw

type nonzero = CSC.nonzero

type t = {
  n : int;  (** number of variables *)
  m : int;  (** number of constraints *)
  q : fv;  (** n-length vector; linear term *)
  p : nonzero list;
      (** n x n postive symmetric semidefinite matrix; sparse quadratic term *)
  a : nonzero list;  (** m x n linear constraint matrix *)
  l : fv;  (** m-length vector; lower bound on linear constraints *)
  u : fv;  (** m-length vector; upper bound on linear constraints *)
}

let fi_ba_of_list num_nz elements =
  let float_a = Bigarray.(Array1.create float64 c_layout num_nz) in
  let int_a = Bigarray.(Array1.create int c_layout num_nz) in
  List.iteri
    (fun j (x, i) ->
      float_a.{j} <- x;
      int_a.{j} <- i)
    elements;
  (float_a, int_a)

let i_ba_of_list list =
  let n = List.length list in
  let a = Bigarray.(Array1.create int c_layout n) in
  List.iteri (fun i x -> a.{i} <- x) list;
  a

let set_adaptive_rho settings = function
  | true -> set_adaptive_rho settings 1
  | false -> set_adaptive_rho settings 0

let get_adaptive_rho settings =
  match get_adaptive_rho settings with 0 -> false | _ -> true

let set_polish settings = function
  | true -> set_polish settings 1
  | false -> set_polish settings 0

let get_polish settings =
  match get_polish settings with 0 -> false | _ -> true

let set_verbose settings = function
  | true -> set_verbose settings 1
  | false -> set_verbose settings 0

let get_verbose settings =
  match get_verbose settings with 0 -> false | _ -> true

let set_scaled_termination settings = function
  | true -> set_scaled_termination settings 1
  | false -> set_scaled_termination settings 0

let get_scaled_termination settings =
  match get_scaled_termination settings with 0 -> false | _ -> true

let set_check_termination settings = function
  | None -> set_check_termination settings 0
  | Some interval -> set_check_termination settings interval

let get_check_termination settings =
  match get_check_termination settings with
  | 0 -> None
  | interval -> Some interval

let set_warm_start settings = function
  | true -> set_warm_start settings 1
  | false -> set_warm_start settings 0

let get_warm_start settings =
  match get_warm_start settings with 0 -> false | _ -> true

let set_time_limit settings = function
  | None -> set_time_limit settings 0
  | Some interval -> set_time_limit settings interval

let get_time_limit settings =
  match get_time_limit settings with 0 -> None | interval -> Some interval

type solve_error =
  | DATA_VALIDATION
  | SETTINGS_VALIDATION
  | LINSYS_SOLVER_LOAD
  | LINSYS_SOLVER_INIT
  | NONCVX
  | MEM_ALLOC
  | WORKSPACE_NOT_INIT

let string_of_solve_error = function
  | DATA_VALIDATION -> "DATA_VALIDATION"
  | SETTINGS_VALIDATION -> "SETTINGS_VALIDATION"
  | LINSYS_SOLVER_LOAD -> "LINSYS_SOLVER_LOAD"
  | LINSYS_SOLVER_INIT -> "LINSYS_SOLVER_INIT"
  | NONCVX -> "NONCVX"
  | MEM_ALLOC -> "MEM_ALLOC"
  | WORKSPACE_NOT_INIT -> "WORKSPACE_NOT_INIT"

let solve_h t =
  match solve t with
  | 0 -> Ok (solution t)
  | 1 -> Error DATA_VALIDATION
  | 2 -> Error SETTINGS_VALIDATION
  | 3 -> Error LINSYS_SOLVER_LOAD
  | 4 -> Error LINSYS_SOLVER_INIT
  | 5 -> Error NONCVX
  | 6 -> Error MEM_ALLOC
  | 7 -> Error WORKSPACE_NOT_INIT
  | _ -> assert false

let solve ?config t =
  if t.n < 1 then failwith "number of variables must be positive";
  if t.m < 0 then failwith "number of constraints must be non-negative";
  if Bigarray.Array1.dim t.l <> t.m then failwith "length mismtach: lower bound";
  if Bigarray.Array1.dim t.u <> t.m then failwith "length mismtach: upper bound";
  let { CSC.xis = p_xis; ps = p_ps; num_nz = p_num_nz } =
    CSC.create ~m:t.m ~n:t.n t.p Upper
  in
  let { CSC.xis = a_xis; ps = a_ps; num_nz = a_num_nz } =
    CSC.create ~m:t.m ~n:t.n t.a Plain
  in

  let settings =
    match config with Some c -> c | None -> default_settings ()
  in
  let p_x, p_i = fi_ba_of_list p_num_nz p_xis in
  let a_x, a_i = fi_ba_of_list a_num_nz a_xis in
  let p_p = i_ba_of_list p_ps in
  let a_p = i_ba_of_list a_ps in

  match create settings p_x p_i p_p t.q a_x a_i a_p t.l t.u t.n t.m with
  | None -> failwith "create failed"
  | Some qp -> solve_h qp
