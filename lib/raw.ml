open Bigarray

type fv = (float, float64_elt, c_layout) Array1.t
type iv = (int, int_elt, c_layout) Array1.t
type t
type settings

external default_settings : unit -> settings = "stub_default_settings"
external set_rho : settings -> float -> unit = "stub_set_rho"
external get_rho : settings -> float = "stub_get_rho"
external set_sigma : settings -> float -> unit = "stub_set_sigma"
external get_sigma : settings -> float = "stub_get_sigma"
external set_scaling : settings -> int -> unit = "stub_set_scaling"
external get_scaling : settings -> int = "stub_get_scaling"
external set_adaptive_rho : settings -> int -> unit = "stub_set_adaptive_rho"
external get_adaptive_rho : settings -> int = "stub_get_adaptive_rho"

external set_adaptive_rho_interval : settings -> int -> unit
  = "stub_set_adaptive_rho_interval"

external get_adaptive_rho_interval : settings -> int
  = "stub_get_adaptive_rho_interval"

external set_adaptive_rho_tolerance : settings -> float -> unit
  = "stub_set_adaptive_rho_tolerance"

external get_adaptive_rho_tolerance : settings -> float
  = "stub_get_adaptive_rho_tolerance"

external set_adaptive_rho_fraction : settings -> float -> unit
  = "stub_set_adaptive_rho_fraction"

external get_adaptive_rho_fraction : settings -> float
  = "stub_get_adaptive_rho_fraction"

external set_max_iter : settings -> int -> unit = "stub_set_max_iter"
external get_max_iter : settings -> int = "stub_get_max_iter"
external set_eps_abs : settings -> float -> unit = "stub_set_eps_abs"
external get_eps_abs : settings -> float = "stub_get_eps_abs"
external set_eps_rel : settings -> float -> unit = "stub_set_eps_rel"
external get_eps_rel : settings -> float = "stub_get_eps_rel"
external set_eps_prim_inf : settings -> float -> unit = "stub_set_eps_prim_inf"
external get_eps_prim_inf : settings -> float = "stub_get_eps_prim_inf"
external set_eps_dual_inf : settings -> float -> unit = "stub_set_eps_dual_inf"
external get_eps_dual_inf : settings -> float = "stub_get_eps_dual_inf"
external set_alpha : settings -> float -> unit = "stub_set_alpha"
external get_alpha : settings -> float = "stub_get_alpha"
external set_delta : settings -> float -> unit = "stub_set_delta"
external get_delta : settings -> float = "stub_get_delta"
external set_polish : settings -> int -> unit = "stub_set_polish"
external get_polish : settings -> int = "stub_get_polish"

external set_polish_refine_iter : settings -> int -> unit
  = "stub_set_polish_refine_iter"

external get_polish_refine_iter : settings -> int
  = "stub_get_polish_refine_iter"

external set_verbose : settings -> int -> unit = "stub_set_verbose"
external get_verbose : settings -> int = "stub_get_verbose"

external set_scaled_termination : settings -> int -> unit
  = "stub_set_scaled_termination"

external get_scaled_termination : settings -> int
  = "stub_get_scaled_termination"

external set_check_termination : settings -> int -> unit
  = "stub_set_check_termination"

external get_check_termination : settings -> int = "stub_get_check_termination"
external set_warm_start : settings -> int -> unit = "stub_set_warm_start"
external get_warm_start : settings -> int = "stub_get_warm_start"
external set_time_limit : settings -> int -> unit = "stub_set_time_limit"
external get_time_limit : settings -> int = "stub_get_time_limit"

external create :
  settings ->
  fv ->
  (* P_x *)
  iv ->
  (* P_i *)
  iv ->
  (* P_p *)
  fv ->
  (* q *)
  fv ->
  (* A_x *)
  iv ->
  (* A_i *)
  iv ->
  (* A_p *)
  fv ->
  (* l *)
  fv ->
  (* u *)
  int ->
  (* n *)
  int ->
  (* m *)
  t option = "bc_stub_create" "nat_stub_create"

external solve : t -> int = "stub_solve"
external solution : t -> fv * fv = "stub_solution"
