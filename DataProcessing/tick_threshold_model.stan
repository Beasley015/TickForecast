functions {
  real crossing_doy(
    real theta,
    vector cgdd,
    vector doy,
    int start_idx,
    int n_days
  ) {
    int first = start_idx;
    int last = start_idx + n_days - 1;

    if (n_days < 2)
      return doy[first];

    if (theta <= cgdd[first])
      return doy[first];

    for (k in (first + 1):last) {
      if (cgdd[k] >= theta) {

        real frac;

        if (cgdd[k] == cgdd[k - 1])
          return doy[k];

        frac =
          (theta - cgdd[k - 1]) /
          (cgdd[k] - cgdd[k - 1]);

        return doy[k - 1] +
               frac * (doy[k] - doy[k - 1]);
      }
    }

    return doy[last];
  }
}

data {
  int<lower=1> N;
  int<lower=1> S;

  vector[N] tick_doy;
  vector[N] lat_c;

  array[N] int<lower=1, upper=S> site;

  int<lower=1> N_daily;

  vector[N_daily] doy;
  vector[N_daily] cgdd;

  array[N] int<lower=1, upper=N_daily> start_idx;
  array[N] int<lower=1> n_days;
}

parameters {
  real beta_0;
  real beta_lat;
  real beta_between;
  real beta_within;

  // Hierarchical GDD threshold parameters
  real theta_mu;
  real<lower=0> theta_sd;
  vector[S] theta_raw;

  real<lower=0> sigma_tick;
}

transformed parameters {
  vector[S] theta;
  vector[N] crossing;
  vector[S] crossing_mean;
  vector[N] crossing_anom;
  vector[N] eta;

  // Non-centered hierarchical thresholds
  theta = theta_mu + theta_sd * theta_raw;

  {
    vector[S] crossing_sum = rep_vector(0.0, S);
    vector[S] crossing_n = rep_vector(0.0, S);

    // Calculate threshold-crossing DOY
    for (i in 1:N) {
      crossing[i] = crossing_doy(
        theta[site[i]],
        cgdd,
        doy,
        start_idx[i],
        n_days[i]
      );

      crossing_sum[site[i]] += crossing[i];
      crossing_n[site[i]] += 1.0;
    }

    // Mean crossing date for each site
    for (s in 1:S) {
      crossing_mean[s] =
        crossing_sum[s] / crossing_n[s];
    }
  }

  // Decompose crossing date into
  // between-site mean + within-site annual anomaly
  for (i in 1:N) {
    crossing_anom[i] =
      crossing[i] - crossing_mean[site[i]];

    eta[i] =
      beta_0 +
      beta_lat * lat_c[i] +
      beta_between * (crossing_mean[site[i]] - 120) +
      beta_within * crossing_anom[i];
  }
}

model {
  // Regression priors
  beta_0 ~ normal(120, 30);
  beta_lat ~ normal(0, 5);
  beta_between ~ normal(0, 1);
  beta_within ~ normal(0, 1);

  // Hierarchical threshold priors
  theta_mu ~ normal(415, 75);
  theta_sd ~ normal(0, 50);
  theta_raw ~ std_normal();

  // Residual error
  sigma_tick ~ exponential(0.2);

  // Likelihood
  tick_doy ~ normal(eta, sigma_tick);
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;

  for (i in 1:N) {
    y_rep[i] =
      normal_rng(eta[i], sigma_tick);

    log_lik[i] =
      normal_lpdf(
        tick_doy[i] |
        eta[i],
        sigma_tick
      );
  }
}

