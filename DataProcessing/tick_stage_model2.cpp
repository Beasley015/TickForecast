
#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {

  DATA_INTEGER(n);
  DATA_INTEGER(n_sy);
  DATA_IVECTOR(sy_id);
  DATA_VECTOR(EVI);
  DATA_VECTOR(rel_greenup);

  DATA_IVECTOR(obs_index);
  DATA_INTEGER(n_obs);
  DATA_VECTOR(nymph_obs);
  DATA_VECTOR(adult_obs);

  PARAMETER(b0_g);
  PARAMETER(b1_g);
  PARAMETER(b2_g);
  PARAMETER(b3_g);

  PARAMETER(b0_m);
  PARAMETER(b1_m);
  PARAMETER(b2_m);

  PARAMETER(log_phi_nymph);
  PARAMETER(log_phi_adult);

  PARAMETER_VECTOR(log_N0);
  PARAMETER_VECTOR(log_A0);

  Type phi_nymph = exp(log_phi_nymph) + Type(1e-6);
  Type phi_adult = exp(log_phi_adult) + Type(1e-6);

  vector<Type> N(n);
  vector<Type> A(n);
  vector<Type> g(n);
  vector<Type> m(n);

  Type nll = 0.0;

  for(int i = 0; i < n; i++) {

    int sy = sy_id(i);

    if(i == 0 || sy_id(i) != sy_id(i - 1)) {
      N(i) = exp(log_N0(sy)) + Type(1e-3);
      A(i) = exp(log_A0(sy)) + Type(1e-3);
    } else {
      Type N_prev = N(i - 1);
      Type A_prev = A(i - 1);

      Type g_prev = g(i - 1);
      Type m_prev = m(i - 1);

      N(i) = N_prev * (Type(1.0) - g_prev) + Type(1e-3);
      A(i) = A_prev * (Type(1.0) - m_prev) + N_prev * g_prev + Type(1e-3);
    }

    Type rg = rel_greenup(i);

    Type eta_g =
      b0_g +
      b1_g * EVI(i) +
      b2_g * rg +
      b3_g * rg * rg;

    Type eta_m =
      b0_m +
      b1_m * EVI(i) +
      b2_m * sqrt(rg * rg + Type(1e-6));

    g(i) = Type(0.20) * invlogit(eta_g);
    m(i) = Type(0.20) * invlogit(eta_m);
  }

  for(int j = 0; j < n_obs; j++) {

    int idx = obs_index(j);

    Type muN = N(idx) + Type(1e-3);
    Type muA = A(idx) + Type(1e-3);

    if(nymph_obs(j) >= 0) {
      nll -= dnbinom2(nymph_obs(j), muN, phi_nymph, true);
    }

    if(adult_obs(j) >= 0) {
      nll -= dnbinom2(adult_obs(j), muA, phi_adult, true);
    }
  }

  ADREPORT(N);
  ADREPORT(A);
  ADREPORT(g);
  ADREPORT(m);

  return nll;
}

