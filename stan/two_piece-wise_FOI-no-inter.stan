### STAN MODEL 4: PIECE-WISE CONSTANT FOI WITH NO INTERACTION

functions {
  real s_step(real t, real lambda1, real lambda2, real s_init) {
    return exp(-t * (lambda1 + lambda2)) * s_init;
  }

  real x1_step(real t, real lambda1, real lambda2, real sigma12,
               real s_init, real x1_init) {
    real num = (1 - exp(-t * (lambda1 + lambda2 - lambda2 * sigma12))) * s_init * lambda1;
    real denom = lambda1 + lambda2 - lambda2 * sigma12;
    return exp(-t * lambda2 * sigma12) * (x1_init + num / denom);
  }

  real x2_step(real t, real lambda1, real lambda2, real sigma21,
               real s_init, real x2_init) {
    real num = (-1 + exp(-t * (lambda1 + lambda2 - lambda1 * sigma21))) * s_init * lambda2
               - x2_init * (lambda1 + lambda2) + x2_init * lambda1 * sigma21;
    real denom = -lambda2 + lambda1 * (sigma21 - 1);
    return exp(-t * lambda1 * sigma21) * num / denom;
  }

  real x12_step(real t, real lambda1, real lambda2, real sigma12, real sigma21,
                real s_init, real x1_init, real x2_init, real x12_init) {
    real A = exp(-t * (lambda2 * sigma12 + lambda1 * sigma21));

    real B = -exp(t * lambda1 * sigma21)
             * (s_init * lambda1 + x1_init * (lambda1 + lambda2 - lambda2 * sigma12))
             * (-lambda2 + lambda1 * (sigma21 - 1));

    real C = exp(t * (lambda2 * (sigma12 - 1) + lambda1 * (sigma21 - 1)))
             * s_init * lambda1 * lambda2 * (sigma12 * (sigma21 - 1) - sigma21);

    real D = exp(t * (lambda2 * sigma12 + lambda1 * sigma21))
             * (s_init + x1_init + x2_init + x12_init)
             * (-lambda1 + lambda2 * (sigma12 - 1))
             * (lambda1 + lambda2 - lambda1 * sigma21);

    real E = -exp(t * lambda2 * sigma12)
             * (-lambda1 + lambda2 * (sigma12 - 1))
             * (s_init * lambda2 + x2_init * (lambda1 + lambda2 - lambda1 * sigma21));

    real denom = (lambda1 + lambda2 - lambda2 * sigma12)
                 * (lambda1 + lambda2 - lambda1 * sigma21);

    return -A * (B + C + D + E) / denom;
  }
}

data {
  int<lower=1> N;
  vector[N] age;
  int<lower=1> n_age_fine;
  vector[n_age_fine] age_fine;
  int<lower=0> n_tested[N];
  int<lower=0> y[N, 4];
  real<lower=0> cutoff_age;
}

transformed data {
  real sigma12 = 1.0; // no interaction
  real sigma21 = 1.0; // no interaction
}

parameters {
  real<lower=0> lambda1_a;
  real<lower=0> lambda1_b;
  real<lower=0> lambda2_a;
  real<lower=0> lambda2_b;
}

model {
  lambda1_a ~ uniform(0.0, 2.0);
  lambda1_b ~ uniform(0.0, 2.0);
  lambda2_a ~ uniform(0.0, 2.0);
  lambda2_b ~ uniform(0.0, 2.0);

  for (i in 1:N) {
    real t = age[i];
    real s_init = 1;
    real x1_init = 0;
    real x2_init = 0;
    real x12_init = 0;

    real s;
    real x1;
    real x2;
    real x12;

    if (t <= cutoff_age) {
      s = s_step(t, lambda1_b, lambda2_b, s_init);
      x1 = x1_step(t, lambda1_b, lambda2_b, sigma12, s_init, x1_init);
      x2 = x2_step(t, lambda1_b, lambda2_b, sigma21, s_init, x2_init);
      x12 = x12_step(t, lambda1_b, lambda2_b, sigma12, sigma21, s_init, x1_init, x2_init, x12_init);
    } else {
      real t1 = t - cutoff_age;
      real t2 = cutoff_age;

      real S1    = s_step(t1, lambda1_a, lambda2_a, s_init);
      real X1_1  = x1_step(t1, lambda1_a, lambda2_a, sigma12, s_init, x1_init);
      real X2_1  = x2_step(t1, lambda1_a, lambda2_a, sigma21, s_init, x2_init);
      real X12_1 = x12_step(t1, lambda1_a, lambda2_a, sigma12, sigma21, s_init, x1_init, x2_init, x12_init);

      s = s_step(t2, lambda1_b, lambda2_b, S1);
      x1 = x1_step(t2, lambda1_b, lambda2_b, sigma12, S1, X1_1);
      x2 = x2_step(t2, lambda1_b, lambda2_b, sigma21, S1, X2_1);
      x12 = x12_step(t2, lambda1_b, lambda2_b, sigma12, sigma21, S1, X1_1, X2_1, X12_1);
    }

    vector[4] probs = [s, x1, x2, x12]';
    y[i] ~ multinomial(probs);
  }
}

generated quantities {
  int y_rep[N, 4];
  vector[n_age_fine] prob_s;
  vector[n_age_fine] prob_x1;
  vector[n_age_fine] prob_x2;
  vector[n_age_fine] prob_x12;
  vector[n_age_fine] seroprev;
  vector[N] log_lik;

  for (i in 1:N) {
    real t = age[i];
    real s_init = 1;
    real x1_init = 0;
    real x2_init = 0;
    real x12_init = 0;

    real s;
    real x1;
    real x2;
    real x12;

    if (t <= cutoff_age) {
      s = s_step(t, lambda1_b, lambda2_b, s_init);
      x1 = x1_step(t, lambda1_b, lambda2_b, sigma12, s_init, x1_init);
      x2 = x2_step(t, lambda1_b, lambda2_b, sigma21, s_init, x2_init);
      x12 = x12_step(t, lambda1_b, lambda2_b, sigma12, sigma21, s_init, x1_init, x2_init, x12_init);
    } else {
      real t1 = t - cutoff_age;
      real t2 = cutoff_age;

      real S1    = s_step(t1, lambda1_a, lambda2_a, s_init);
      real X1_1  = x1_step(t1, lambda1_a, lambda2_a, sigma12, s_init, x1_init);
      real X2_1  = x2_step(t1, lambda1_a, lambda2_a, sigma21, s_init, x2_init);
      real X12_1 = x12_step(t1, lambda1_a, lambda2_a, sigma12, sigma21, s_init, x1_init, x2_init, x12_init);

      s = s_step(t2, lambda1_b, lambda2_b, S1);
      x1 = x1_step(t2, lambda1_b, lambda2_b, sigma12, S1, X1_1);
      x2 = x2_step(t2, lambda1_b, lambda2_b, sigma21, S1, X2_1);
      x12 = x12_step(t2, lambda1_b, lambda2_b, sigma12, sigma21, S1, X1_1, X2_1, X12_1);
    }

    vector[4] probs = [s, x1, x2, x12]';
    y_rep[i] = multinomial_rng(probs, n_tested[i]);
    log_lik[i] = multinomial_lpmf(y[i] | probs);
  }

  for (i in 1:n_age_fine) {
    real t = age_fine[i];
    real s_init = 1;
    real x1_init = 0;
    real x2_init = 0;
    real x12_init = 0;

    real s;
    real x1;
    real x2;
    real x12;

    if (t <= cutoff_age) {
      s = s_step(t, lambda1_b, lambda2_b, s_init);
      x1 = x1_step(t, lambda1_b, lambda2_b, sigma12, s_init, x1_init);
      x2 = x2_step(t, lambda1_b, lambda2_b, sigma21, s_init, x2_init);
      x12 = x12_step(t, lambda1_b, lambda2_b, sigma12, sigma21, s_init, x1_init, x2_init, x12_init);
    } else {
      real t1 = t - cutoff_age;
      real t2 = cutoff_age;

      real S1    = s_step(t1, lambda1_a, lambda2_a, s_init);
      real X1_1  = x1_step(t1, lambda1_a, lambda2_a, sigma12, s_init, x1_init);
      real X2_1  = x2_step(t1, lambda1_a, lambda2_a, sigma21, s_init, x2_init);
      real X12_1 = x12_step(t1, lambda1_a, lambda2_a, sigma12, sigma21, s_init, x1_init, x2_init, x12_init);

      s = s_step(t2, lambda1_b, lambda2_b, S1);
      x1 = x1_step(t2, lambda1_b, lambda2_b, sigma12, S1, X1_1);
      x2 = x2_step(t2, lambda1_b, lambda2_b, sigma21, S1, X2_1);
      x12 = x12_step(t2, lambda1_b, lambda2_b, sigma12, sigma21, S1, X1_1, X2_1, X12_1);
    }

    prob_s[i] = s;
    prob_x1[i] = x1;
    prob_x2[i] = x2;
    prob_x12[i] = x12;
    seroprev[i] = 1 - s;
  }
}
