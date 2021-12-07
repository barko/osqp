open Bigarray
let fa = Array1.of_array float64 c_layout
let ia = Array1.of_array int c_layout

let pr_fa a =
  let n = Array1.dim a in
  for i = 0 to n-1 do
    Printf.printf "%d %f\n" i a.{i}
  done

let pr_ia a =
  let n = Array1.dim a in
  for i = 0 to n-1 do
    Printf.printf "%d %d\n" i a.{i}
  done

let pr_result = function
  | Ok (x, y) ->
    Printf.printf "ok\nx=\n";
    pr_fa x;
    Printf.printf "y=\n";
    pr_fa y

  | Error e ->
    Printf.printf "result=%s\n" (Osqp.Easy.string_of_solve_error e)

let raw_test () =
  let p_x = fa [| 4.0; 1.0; 2.0 |] in
  let p_i = ia [| 0; 0; 1 |] in
  let p_p = ia [| 0; 1; 3 |] in
  let q   = fa [| 1.; 1. |] in
  let a_x = fa [| 1.; 1.; 1.; 1. |] in
  let a_i = ia [| 0; 1; 0; 2 |] in
  let a_p = ia [| 0; 2; 4; |] in
  let l   = fa [| 1.; 0.; 0. |] in
  let u   = fa [| 1.; 0.7; 0.7 |] in
  let n   = 2 in
  let m   = 3 in

  let open Osqp.Raw in
  let settings = default_settings () in
  Printf.printf "rho=%e\n%!" (get_rho settings);
  Printf.printf "sigma=%e\n%!" (get_sigma settings);
  Printf.printf "scaling=%d\n%!" (get_scaling settings);
  Printf.printf "adaptive_rho=%d\n%!" (get_adaptive_rho settings);
  Printf.printf "adaptive_rho_interval=%d\n%!" (get_adaptive_rho_interval settings);
  Printf.printf "adaptive_rho_tolerance=%e\n%!" (get_adaptive_rho_tolerance settings);
  Printf.printf "adaptive_rho_fraction=%e\n%!" (get_adaptive_rho_fraction settings);
  Printf.printf "max_iter=%d\n%!" (get_max_iter settings);
  Printf.printf "eps_abs=%e\n%!" (get_eps_abs settings);
  Printf.printf "eps_rel=%e\n%!" (get_eps_rel settings);
  Printf.printf "eps_prim_inf=%e\n%!" (get_eps_prim_inf settings);
  Printf.printf "eps_dual_inf=%e\n%!" (get_eps_dual_inf settings);
  Printf.printf "alpha=%e\n%!" (get_alpha settings);
  Printf.printf "delta=%e\n%!" (get_delta settings);
  Printf.printf "polish=%d\n%!" (get_polish settings);
  Printf.printf "polish_refine_iter=%d\n%!" (get_polish_refine_iter settings);
  Printf.printf "verbose=%d\n%!" (get_verbose settings);
  Printf.printf "scaled_termination=%d\n%!" (get_scaled_termination settings);
  Printf.printf "check_termination=%d\n%!" (get_check_termination settings);
  Printf.printf "warm_start=%d\n%!" (get_warm_start settings);
  Printf.printf "time_limit=%d\n%!" (get_time_limit settings);
  set_rho settings (get_rho settings);
  match create settings p_x p_i p_p q a_x a_i a_p l u n m with
  | None -> print_endline "failed to create"
  | Some t ->
    let res = solve t in
    Printf.printf "result=%d\n" res

let easy_test () =
  let open Osqp.Easy in
  let open Osqp.CSC in
  let p = [{ i=0; j=0; x=4.0}; {i=0; j=1; x=1.0}; {i=1; j=1; x=2.0}] in
  let q   = fa [| 1.; 1. |] in
  let a = [{i=0; j=0; x=1.}; {i=0; j=1; x=1.}; {i=1; j=0; x=1.}; {i=2; j=1; x=1.}] in
  let l   = fa [| 1.; 0.; 0. |] in
  let u   = fa [| 1.; 0.7; 0.7 |] in
  let n   = 2 in
  let m   = 3 in
  let t = { m; n; p; q; a; l; u; } in
  let res = solve t in
  (* pr_result res *)
  ignore res


let _ =
  easy_test ()
