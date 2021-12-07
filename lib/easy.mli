type t = {
  n : int;
  m : int;
  q : Raw.fv;
  p : CSC.nonzero list;
  a : CSC.nonzero list;
  l : Raw.fv;
  u : Raw.fv;
}

type settings

val default_settings : unit -> settings
val get_adaptive_rho : settings -> bool
val get_adaptive_rho_fraction : settings -> float
val get_adaptive_rho_interval : settings -> int
val get_adaptive_rho_tolerance : settings -> float
val get_alpha : settings -> float
val get_check_termination : settings -> int option
val get_delta : settings -> float
val get_eps_abs : settings -> float
val get_eps_dual_inf : settings -> float
val get_eps_prim_inf : settings -> float
val get_eps_rel : settings -> float
val get_max_iter : settings -> int
val get_polish : settings -> bool
val get_polish_refine_iter : settings -> int
val get_rho : settings -> float
val get_scaled_termination : settings -> bool
val get_scaling : settings -> int
val get_sigma : settings -> float
val get_time_limit : settings -> int option
val get_verbose : settings -> bool
val get_warm_start : settings -> bool
val set_adaptive_rho : settings -> bool -> unit
val set_adaptive_rho_fraction : settings -> float -> unit
val set_adaptive_rho_interval : settings -> int -> unit
val set_adaptive_rho_tolerance : settings -> float -> unit
val set_alpha : settings -> float -> unit
val set_check_termination : settings -> int option -> unit
val set_delta : settings -> float -> unit
val set_eps_abs : settings -> float -> unit
val set_eps_dual_inf : settings -> float -> unit
val set_eps_prim_inf : settings -> float -> unit
val set_eps_rel : settings -> float -> unit
val set_max_iter : settings -> int -> unit
val set_polish : settings -> bool -> unit
val set_polish_refine_iter : settings -> int -> unit
val set_rho : settings -> float -> unit
val set_scaled_termination : settings -> bool -> unit
val set_scaling : settings -> int -> unit
val set_sigma : settings -> float -> unit
val set_time_limit : settings -> int option -> unit
val set_verbose : settings -> bool -> unit
val set_warm_start : settings -> bool -> unit

type solve_error =
  | DATA_VALIDATION
  | SETTINGS_VALIDATION
  | LINSYS_SOLVER_LOAD
  | LINSYS_SOLVER_INIT
  | NONCVX
  | MEM_ALLOC
  | WORKSPACE_NOT_INIT

val string_of_solve_error : solve_error -> string

val solve : ?config:settings -> t -> (Raw.fv * Raw.fv, solve_error) result
