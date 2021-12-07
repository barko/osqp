#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/bigarray.h>
#include <caml/fail.h>

#include <assert.h>
#include "osqp.h"

/*
void settings_delete(value settings_)
{
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
}
*/

static struct custom_operations settings_ops = {
  "osqp.settings",             // not default
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value stub_default_settings(value u)
{
  CAMLparam1(u);
  CAMLlocal1(settings_);
  settings_ = caml_alloc_custom( &settings_ops, sizeof(OSQPSettings), 0, 1 );
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  osqp_set_default_settings(settings);
  CAMLreturn(settings_);
}

// rho : float
CAMLprim value stub_set_rho(value settings_, value rho_)
{
  CAMLparam2(settings_, rho_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float rho = Double_val(rho_);
  settings->rho = rho;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_rho(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->rho));
}

// sigma : float
CAMLprim value stub_set_sigma(value settings_, value sigma_)
{
  CAMLparam2(settings_, sigma_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float sigma = Double_val(sigma_);
  settings->sigma = sigma;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_sigma(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->sigma));
}

// scaling : int
CAMLprim value stub_set_scaling(value settings_, value scaling_)
{
  CAMLparam2(settings_, scaling_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int scaling = Long_val(scaling_);
  settings->scaling = scaling;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_scaling(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->scaling));
}

// adaptive_rho : int
CAMLprim value stub_set_adaptive_rho(value settings_, value adaptive_rho_)
{
  CAMLparam2(settings_, adaptive_rho_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int adaptive_rho = Long_val(adaptive_rho_);
  settings->adaptive_rho = adaptive_rho;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_adaptive_rho(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->adaptive_rho));
}

// adaptive_rho_interval : int
CAMLprim value stub_set_adaptive_rho_interval(value settings_, value adaptive_rho_interval_)
{
  CAMLparam2(settings_, adaptive_rho_interval_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int adaptive_rho_interval = Long_val(adaptive_rho_interval_);
  settings->adaptive_rho_interval = adaptive_rho_interval;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_adaptive_rho_interval(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->adaptive_rho_interval));
}

// adaptive_rho_tolerance : float
CAMLprim value stub_set_adaptive_rho_tolerance(value settings_, value adaptive_rho_tolerance_)
{
  CAMLparam2(settings_, adaptive_rho_tolerance_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float adaptive_rho_tolerance = Double_val(adaptive_rho_tolerance_);
  settings->adaptive_rho_tolerance = adaptive_rho_tolerance;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_adaptive_rho_tolerance(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->adaptive_rho_tolerance));
}

// adaptive_rho_fraction : float
CAMLprim value stub_set_adaptive_rho_fraction(value settings_, value adaptive_rho_fraction_)
{
  CAMLparam2(settings_, adaptive_rho_fraction_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float adaptive_rho_fraction = Double_val(adaptive_rho_fraction_);
  settings->adaptive_rho_fraction = adaptive_rho_fraction;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_adaptive_rho_fraction(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->adaptive_rho_fraction));
}

// max_iter : int
CAMLprim value stub_set_max_iter(value settings_, value max_iter_)
{
  CAMLparam2(settings_, max_iter_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int max_iter = Long_val(max_iter_);
  settings->max_iter = max_iter;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_max_iter(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->max_iter));
}

// eps_abs : float
CAMLprim value stub_set_eps_abs(value settings_, value eps_abs_)
{
  CAMLparam2(settings_, eps_abs_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float eps_abs = Double_val(eps_abs_);
  settings->eps_abs = eps_abs;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_eps_abs(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->eps_abs));
}

// eps_rel : float
CAMLprim value stub_set_eps_rel(value settings_, value eps_rel_)
{
  CAMLparam2(settings_, eps_rel_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float eps_rel = Double_val(eps_rel_);
  settings->eps_rel = eps_rel;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_eps_rel(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->eps_rel));
}

// eps_prim_inf : float
CAMLprim value stub_set_eps_prim_inf(value settings_, value eps_prim_inf_)
{
  CAMLparam2(settings_, eps_prim_inf_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float eps_prim_inf = Double_val(eps_prim_inf_);
  settings->eps_prim_inf = eps_prim_inf;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_eps_prim_inf(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->eps_prim_inf));
}

// eps_dual_inf : float
CAMLprim value stub_set_eps_dual_inf(value settings_, value eps_dual_inf_)
{
  CAMLparam2(settings_, eps_dual_inf_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float eps_dual_inf = Double_val(eps_dual_inf_);
  settings->eps_dual_inf = eps_dual_inf;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_eps_dual_inf(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->eps_dual_inf));
}

// alpha : float
CAMLprim value stub_set_alpha(value settings_, value alpha_)
{
  CAMLparam2(settings_, alpha_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float alpha = Double_val(alpha_);
  settings->alpha = alpha;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_alpha(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->alpha));
}

// delta : float
CAMLprim value stub_set_delta(value settings_, value delta_)
{
  CAMLparam2(settings_, delta_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_float delta = Double_val(delta_);
  settings->delta = delta;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_delta(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(caml_copy_double(settings->delta));
}

// polish : int
CAMLprim value stub_set_polish(value settings_, value polish_)
{
  CAMLparam2(settings_, polish_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int polish = Long_val(polish_);
  settings->polish = polish;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_polish(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->polish));
}

// polish_refine_iter : int
CAMLprim value stub_set_polish_refine_iter(value settings_, value polish_refine_iter_)
{
  CAMLparam2(settings_, polish_refine_iter_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int polish_refine_iter = Long_val(polish_refine_iter_);
  settings->polish_refine_iter = polish_refine_iter;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_polish_refine_iter(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->polish_refine_iter));
}

// verbose : int
CAMLprim value stub_set_verbose(value settings_, value verbose_)
{
  CAMLparam2(settings_, verbose_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int verbose = Long_val(verbose_);
  settings->verbose = verbose;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_verbose(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->verbose));
}

// scaled_termination : int
CAMLprim value stub_set_scaled_termination(value settings_, value scaled_termination_)
{
  CAMLparam2(settings_, scaled_termination_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int scaled_termination = Long_val(scaled_termination_);
  settings->scaled_termination = scaled_termination;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_scaled_termination(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->scaled_termination));
}

// check_termination : int
CAMLprim value stub_set_check_termination(value settings_, value check_termination_)
{
  CAMLparam2(settings_, check_termination_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int check_termination = Long_val(check_termination_);
  settings->check_termination = check_termination;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_check_termination(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->check_termination));
}

// warm_start : int
CAMLprim value stub_set_warm_start(value settings_, value warm_start_)
{
  CAMLparam2(settings_, warm_start_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int warm_start = Long_val(warm_start_);
  settings->warm_start = warm_start;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_warm_start(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->warm_start));
}

// time_limit : int
CAMLprim value stub_set_time_limit(value settings_, value time_limit_)
{
  CAMLparam2(settings_, time_limit_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  c_int time_limit = Long_val(time_limit_);
  settings->time_limit = time_limit;
  CAMLreturn(Val_unit);
}

CAMLprim value stub_get_time_limit(value settings_)
{
  CAMLparam1(settings_);
  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  CAMLreturn(Val_int(settings->time_limit));
}


typedef struct {
  OSQPData* data;
  OSQPWorkspace* work;
} caml_osqp;

void stub_delete(value t_)
{
  caml_osqp* t = (caml_osqp*)Data_custom_val(t_);
  OSQPData* data = t->data;
  osqp_cleanup(t->work);
  if (data->A) {
    c_free(data->A);
  }
  if (data->P) {
    c_free(data->P);
  }
  c_free(data);
}

static struct custom_operations osqp_ops = {
  "osqp.t",                  // not default
  stub_delete,               // not default
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value nat_stub_create(
 value settings_,
 value P_x_, value P_i_, value P_p_,
 value q_,
 value A_x_, value A_i_, value A_p_,
 value l_, value u_,
 value n_, value m_
) {
  CAMLparam5(settings_, P_x_, P_i_, P_p_, q_);
  CAMLxparam5(A_x_, A_i_, A_p_, l_, u_);
  CAMLxparam2(n_, m_);

  // number of variables
  assert( Is_long(n_) );
  c_int n = Int_val(n_);
  assert( n > 0 );

  // number of constraints
  assert( Is_long(m_) );
  c_int m = Int_val(m_);
  assert( m >= 0 );

  // all bigarrays have one dimension
  assert( Caml_ba_array_val(P_x_)->num_dims == 1 );
  assert( Caml_ba_array_val(P_i_)->num_dims == 1 );
  assert( Caml_ba_array_val(P_p_)->num_dims == 1 );
  assert( Caml_ba_array_val(q_  )->num_dims == 1 );
  assert( Caml_ba_array_val(A_x_)->num_dims == 1 );
  assert( Caml_ba_array_val(A_i_)->num_dims == 1 );
  assert( Caml_ba_array_val(A_p_)->num_dims == 1 );
  assert( Caml_ba_array_val(l_  )->num_dims == 1 );
  assert( Caml_ba_array_val(u_  )->num_dims == 1 );

  // some bigarrays are float64's, others are int64's
  assert( (Caml_ba_array_val(P_x_)->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64  );
  assert( (Caml_ba_array_val(P_i_)->flags & CAML_BA_KIND_MASK) == CAML_BA_CAML_INT );
  assert( (Caml_ba_array_val(P_p_)->flags & CAML_BA_KIND_MASK) == CAML_BA_CAML_INT );
  assert( (Caml_ba_array_val(q_  )->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64  );
  assert( (Caml_ba_array_val(A_x_)->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64  );
  assert( (Caml_ba_array_val(A_i_)->flags & CAML_BA_KIND_MASK) == CAML_BA_CAML_INT );
  assert( (Caml_ba_array_val(A_p_)->flags & CAML_BA_KIND_MASK) == CAML_BA_CAML_INT );
  assert( (Caml_ba_array_val(u_  )->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64  );
  assert( (Caml_ba_array_val(l_  )->flags & CAML_BA_KIND_MASK) == CAML_BA_FLOAT64  );

  c_int P_nnz = Caml_ba_array_val(P_x_)->dim[0];
  c_int A_nnz = Caml_ba_array_val(A_x_)->dim[0];

  assert( P_nnz >= 0 );
  assert( A_nnz >= 0 );

  assert( Caml_ba_array_val(P_i_)->dim[0] == P_nnz );
  assert( Caml_ba_array_val(P_p_)->dim[0] == n + 1 );

  assert( Caml_ba_array_val(A_i_)->dim[0] == A_nnz );
  assert( Caml_ba_array_val(A_p_)->dim[0] == n + 1 );

  assert( Caml_ba_array_val(q_)->dim[0] == n );

  assert( Caml_ba_array_val(l_)->dim[0] == m );
  assert( Caml_ba_array_val(u_)->dim[0] == m );

  c_float* P_x = Caml_ba_data_val(P_x_);
  c_int*   P_i = Caml_ba_data_val(P_i_);
  c_int*   P_p = Caml_ba_data_val(P_p_);
  c_float* q   = Caml_ba_data_val(q_  );
  c_float* A_x = Caml_ba_data_val(A_x_);
  c_int*   A_i = Caml_ba_data_val(A_i_);
  c_int*   A_p = Caml_ba_data_val(A_p_);
  c_float* l   = Caml_ba_data_val(l_  );
  c_float* u   = Caml_ba_data_val(u_  );

  OSQPData* data = (OSQPData*)c_malloc(sizeof(OSQPData));
  data->n = n;
  data->m = m;
  data->P = csc_matrix(n, n, P_nnz, P_x, P_i, P_p);
  data->q = q;
  data->A = csc_matrix(m, n, A_nnz, A_x, A_i, A_p);
  data->l = l;
  data->u = u;

  OSQPSettings* settings = (OSQPSettings*)Data_custom_val(settings_);
  OSQPWorkspace* work = NULL;
  c_int exitflag = osqp_setup(&work, data, settings);
  if ( exitflag == 0 ) {
    assert( work != NULL );
    CAMLlocal2(t_, some_);
    t_ = caml_alloc_custom( &osqp_ops, sizeof(caml_osqp), 0, 1 );
    caml_osqp* t = (caml_osqp*)Data_custom_val(t_);
    t->work = work;
    t->data = data;
    some_ = caml_alloc_small(1,0);
    Field(some_,0) = t_;
    CAMLreturn(some_);
  }
  else {
    // None
    CAMLreturn(Val_int(0));
  }

}

CAMLprim value bc_stub_create(value* argv, int argc)
{
  assert( argc == 12 );
  return nat_stub_create(
   argv[0],
   argv[1],
   argv[2],
   argv[3],
   argv[4],
   argv[5],
   argv[6],
   argv[7],
   argv[8],
   argv[9],
   argv[10],
   argv[11]
  );
}

CAMLprim value stub_solve( value t_ )
{
  CAMLparam1(t_);
  caml_osqp* t = (caml_osqp*)Data_custom_val(t_);
  c_int err = osqp_solve(t->work);
  CAMLreturn(Val_int(err));
}

CAMLprim value stub_solution( value t_ )
{
  CAMLparam1(t_);
  CAMLlocal3(x, y, xy);

  caml_osqp* t = (caml_osqp*)Data_custom_val(t_);
  c_int n = t->data->n;
  c_int m = t->data->m;
  long dims[1];

  // x
  dims[0] = n;
  c_float* xx = (c_float*)c_malloc(n * sizeof(c_float));
  memcpy( xx, t->work->solution->x, n * sizeof(c_float) );
  x = caml_ba_alloc_dims( CAML_BA_FLOAT64 | CAML_BA_C_LAYOUT, 1, xx, n );

  // y
  dims[0] = m;
  c_float* yy = (c_float*)c_malloc(m * sizeof(c_float));
  memcpy( yy, t->work->solution->y, m * sizeof(c_float) );
  y = caml_ba_alloc_dims( CAML_BA_FLOAT64 | CAML_BA_C_LAYOUT, 1, yy, m );

  xy = caml_alloc_tuple(2);
  Store_field(xy, 0, x);
  Store_field(xy, 1, y);

  CAMLreturn(xy);
}
