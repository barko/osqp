include Raw

type nonzero = {
  i : int;
  j : int;
  x : float
}

type t = {
  n : int;
  (* number of variables *)

  m : int;
  (* number of constraints *)

  q : fv;
  (* n-length vector; linear term *)

  p : nonzero list;
  (* n x n postive symmetric semidefinite matrix; sparse quadratic term *)

  a : nonzero list;
  (* m x n linear constraint matrix *)

  l : fv;
  (* m-length vector; lower bound on linear constraints *)

  u : fv;
  (* m-length vector; upper bound on linear constraints *)

}

let cmp_nz {i = i1; j = j1; _} {i = i2; j = j2; _} =
  match Int.compare j1 j2 with
  | 0 -> Int.compare i1 i2
  | c -> c

let rec prepend ~value ~count list =
  if count = 0 then
    list
  else
    prepend ~value ~count:(count - 1) (value :: list)

let check_bounds_exn ~m ~n {i; j; _} =
  if i < 0 || i >= m then
    failwith "row index out of bounds";
  if j < 0 || j >= n then
    failwith "column index out of bounds"


let csc ~m ~n =
  let rec loop check_structure_exn xs is ps prev_nz k = function
    | ({i; j; x} as nz) :: rest ->
      (* because nonzeros are sorted *)
      assert (prev_nz.j <= j);

      check_bounds_exn ~m ~n nz;

      if prev_nz.j = j && prev_nz.i = i then
        failwith "duplicate nonzeros";

      check_structure_exn nz;

      let xs = x :: xs in
      let is = i :: is in
      let ps = prepend ~value:k ~count:(j - prev_nz.j) ps in
      let k = k + 1 in
      loop check_structure_exn xs is ps nz k rest


    | [] ->
      let xs = List.rev xs in
      let is = List.rev is in
      let ps = List.rev (k :: ps) in
      assert (List.length xs = List.length is);
      assert (List.length ps = n + 1);
      xs, is, ps
  in
  fun nonzeros structure ->
    let nonzeros = List.sort cmp_nz nonzeros in
    let check_structure_exn =
      match structure with
      | `Upper ->
        fun {i; j; _} ->
          if i > j then
            failwith "not upper-triangular"
      | `Lower ->
        fun {i; j; _} ->
          if i < j then
            failwith "not lower-triangular"
      | `None ->
        fun _ -> ()
    in
    match nonzeros with
    | [] -> [], [], [0]
    | nz :: rest ->
      check_bounds_exn ~m ~n nz;
      let xs = [nz.x] in
      let is = [nz.i] in
      let ps = [0] in
      loop check_structure_exn xs is ps nz 1 rest

(*
let test1 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=1; x=2.}; {i=1; j=0; x=3.}; {i=2; j=1; x=4.}] in
  let xs, is, ps = csc ~m:3 ~n:2 nz `None in
  assert ( xs = [1.; 3.; 2.; 4.    ] );
  assert ( is = [0 ; 1 ; 0 ; 2 ] );
  assert ( ps = [0 ; 2 ; 4     ] )

let test2 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=3; x=2.}; {i=1; j=0; x=3.}; {i=2; j=3; x=4.}] in
  let xs, is, ps = csc ~m:3 ~n:4 nz `None in
  assert ( xs = [1.; 3.; 2.; 4.    ] );
  assert ( is = [0 ; 1 ; 0 ; 2     ] );
  assert ( ps = [0 ; 2 ; 2 ; 2 ; 4 ] )

let test3 () =
  let nz = [{i=0; j=0; x=1.}; {i=0; j=3; x=2.}; {i=1; j=1; x=3.}] in
  let xs, is, ps = csc ~m:3 ~n:4 nz `Upper in
  assert ( xs = [1.; 3.; 2.        ] );
  assert ( is = [0 ; 1 ; 0         ] );
  assert ( ps = [0 ; 1 ; 2 ; 2 ; 3 ] )
*)

let f_ba_of_list list =
  let n = List.length list in
  let a = Bigarray.(Array1.create float64 c_layout n) in
  List.iteri (
    fun i x ->
      a.{i} <- x
  ) list;
  a

let i_ba_of_list list =
  let n = List.length list in
  let a = Bigarray.(Array1.create int c_layout n) in
  List.iteri (
    fun i x ->
      a.{i} <- x
  ) list;
  a

let set_adaptive_rho settings = function
  | true  -> set_adaptive_rho settings 1
  | false -> set_adaptive_rho settings 0

let get_adaptive_rho settings =
  match get_adaptive_rho settings with
  | 0 -> false
  | _ -> true

let set_polish settings = function
  | true  -> set_polish settings 1
  | false -> set_polish settings 0

let get_polish settings =
  match get_polish settings with
  | 0 -> false
  | _ -> true

let set_verbose settings = function
  | true  -> set_verbose settings 1
  | false -> set_verbose settings 0

let get_verbose settings =
  match get_verbose settings with
  | 0 -> false
  | _ -> true

let set_scaled_termination settings = function
  | true  -> set_scaled_termination settings 1
  | false -> set_scaled_termination settings 0

let get_scaled_termination settings =
  match get_scaled_termination settings with
  | 0 -> false
  | _ -> true

let set_check_termination settings = function
  | None  -> set_check_termination settings 0
  | Some interval -> set_check_termination settings interval

let get_check_termination settings =
  match get_check_termination settings with
  | 0 -> None
  | interval -> Some interval

let set_warm_start settings = function
  | true  -> set_warm_start settings 1
  | false -> set_warm_start settings 0

let get_warm_start settings =
  match get_warm_start settings with
  | 0 -> false
  | _ -> true

let set_time_limit settings = function
  | None  -> set_time_limit settings 0
  | Some interval -> set_time_limit settings interval

let get_time_limit settings =
  match get_time_limit settings with
  | 0 -> None
  | interval -> Some interval


type solve_error =
  | DATA_VALIDATION
  | SETTINGS_VALIDATION
  | LINSYS_SOLVER_LOAD
  | LINSYS_SOLVER_INIT
  | NONCVX
  | MEM_ALLOC
  | WORKSPACE_NOT_INIT

let string_of_solve_error = function
  | DATA_VALIDATION      -> "DATA_VALIDATION"
  | SETTINGS_VALIDATION  -> "SETTINGS_VALIDATION"
  | LINSYS_SOLVER_LOAD   -> "LINSYS_SOLVER_LOAD"
  | LINSYS_SOLVER_INIT   -> "LINSYS_SOLVER_INIT"
  | NONCVX               -> "NONCVX"
  | MEM_ALLOC            -> "MEM_ALLOC"
  | WORKSPACE_NOT_INIT   -> "WORKSPACE_NOT_INIT"


let solve t =
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

let solve t =
  if t.n < 1 then
    failwith "number of variables must be positive";
  if t.m < 0 then
    failwith "number of constraints must be non-negative";
  if Bigarray.Array1.dim t.l <> t.m then
    failwith "length mismtach: lower bound";
  if Bigarray.Array1.dim t.u <> t.m then
    failwith "length mismtach: upper bound";
  let p_x, p_i, p_p = csc ~m:t.m ~n:t.n t.p `Upper in
  let a_x, a_i, a_p = csc ~m:t.m ~n:t.n t.a `None in

  let settings = default_settings () in
  let p_x = f_ba_of_list p_x in
  let p_i = i_ba_of_list p_i in
  let p_p = i_ba_of_list p_p in
  let a_x = f_ba_of_list a_x in
  let a_i = i_ba_of_list a_i in
  let a_p = i_ba_of_list a_p in

  match create settings p_x p_i p_p t.q a_x a_i a_p t.l t.u t.n t.m with
  | None -> failwith "create failed"
  | Some qp ->
    solve qp

