functions {
  vector dengue_age_ode(real t,
                        vector y,
                        real[] theta,
                        real[] x_r,
                        int[] x_i) {
    int A = x_i[1];  // number of age groups
    real lambda1 = theta[1];
    real lambda2 = theta[2];
    real sigma12 = theta[3];
    real sigma21 = theta[4];
    
    vector[4 * A] dydt;
    
    for (a in 1:A) {
      int offset = (a - 1) * 4;
      real S     = y[offset + 1];
      real X10   = y[offset + 2];
      real X20   = y[offset + 3];
      real X12   = y[offset + 4];
      
      dydt[offset + 1] = -(lambda1 + lambda2) * S;
      dydt[offset + 2] = lambda1 * S - lambda2 * sigma12 * X10;
      dydt[offset + 3] = lambda2 * S - lambda1 * sigma21 * X20;
      dydt[offset + 4] = lambda2 * sigma12 * X10 - lambda1 * sigma21 * X20;
    }
    return dydt;
  }
}

data {
  int<lower=1> A;        // age groups
  int<lower=1> N;        // time points
  real ts[N];            // time points
  real y_obs[N, A];      // observed secondary infections by age group
}

transformed data {
  real t0 = 0;
  real x_r[0];
  int x_i[1] = { A };
  vector[4 * A] y0;
  
  for (a in 1:A) {
    int offset = (a - 1) * 4;
    y0[offset + 1] = 1000; // S
    y0[offset + 2] = 0;    // X10
    y0[offset + 3] = 0;    // X20
    y0[offset + 4] = 0;    // X12
  }
}

parameters {
  real<lower=0> lambda1;
  real<lower=0> lambda2;
  real<lower=0> sigma12;
  real<lower=0> sigma21;
  real<lower=0> phi;
}

model {
  real theta[4] = { lambda1, lambda2, sigma12, sigma21 };
  vector[4 * A] y_hat[N] = integrate_ode_rk45(dengue_age_ode, y0, t0, ts, theta, x_r, x_i);

  // Priors
  lambda1 ~ normal(0.05, 0.02);
  lambda2 ~ normal(0.04, 0.02);
  sigma12 ~ lognormal(log(1), 0.3);
  sigma21 ~ lognormal(log(1), 0.3);
  phi ~ exponential(1);

  // Likelihood
  for (n in 1:N)
    for (a in 1:A) {
      int offset = (a - 1) * 4;
      y_obs[n, a] ~ neg_binomial_2(y_hat[n, offset + 4], phi); // X12
    }
}
