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
  int<lower=0> y[N, 4];
  int<lower=0> n_tested[N];

  int<lower=1> n_phases;
  vector[n_phases - 1] cutoff_ages;
}

parameters {
  vector<lower=0>[n_phases] lambda1;
  real<lower=0> lambda2;
  real<lower=0> sigma12;
  real<lower=0> sigma21;
}

model {
  lambda1 ~ uniform(0.0, 2.0);
  lambda2 ~ uniform(0.0, 2.0);
  sigma12 ~ uniform(0.0, 20.0);
  sigma21 ~ uniform(0.0, 20.0);

  for (i in 1:N) {
    real t = age[i];
    real s = 1;
    real x1 = 0;
    real x2 = 0;
    real x12 = 0;

    real last = t;
    vector[n_phases] durations;
    for (p in 1:(n_phases - 1)) {
      durations[p] = fmax(0, last - cutoff_ages[p]);
      last = fmin(last, cutoff_ages[p]);
    }
    durations[n_phases] = last;
    durations = reverse(durations);

    for (p in 1:n_phases) {
      real dt = durations[p];
      if (dt == 0) continue;

      real lam1 = lambda1[p];
      s  = s_step(dt, lam1, lambda2, s);
      x1 = x1_step(dt, lam1, lambda2, sigma12, s, x1);
      x2 = x2_step(dt, lam1, lambda2, sigma21, s, x2);
      x12 = x12_step(dt, lam1, lambda2, sigma12, sigma21, s, x1, x2, x12);
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
    real s = 1;
    real x1 = 0;
    real x2 = 0;
    real x12 = 0;

    real last = t;
    vector[n_phases] durations;
    for (p in 1:(n_phases - 1)) {
      durations[p] = fmax(0, last - cutoff_ages[p]);
      last = fmin(last, cutoff_ages[p]);
    }
    durations[n_phases] = last;
    durations = reverse(durations);

    for (p in 1:n_phases) {
      real dt = durations[p];
      if (dt == 0) continue;

      real lam1 = lambda1[p];
      s  = s_step(dt, lam1, lambda2, s);
      x1 = x1_step(dt, lam1, lambda2, sigma12, s, x1);
      x2 = x2_step(dt, lam1, lambda2, sigma21, s, x2);
      x12 = x12_step(dt, lam1, lambda2, sigma12, sigma21, s, x1, x2, x12);
    }

    vector[4] probs = [s, x1, x2, x12]';
    y_rep[i] = multinomial_rng(probs, n_tested[i]);
    log_lik[i] = multinomial_lpmf(y[i] | probs);
  }

  for (i in 1:n_age_fine) {
    real t = age_fine[i];
    real s = 1;
    real x1 = 0;
    real x2 = 0;
    real x12 = 0;

    real last = t;
    vector[n_phases] durations;
    for (p in 1:(n_phases - 1)) {
      durations[p] = fmax(0, last - cutoff_ages[p]);
      last = fmin(last, cutoff_ages[p]);
    }
    durations[n_phases] = last;
    durations = reverse(durations);

    for (p in 1:n_phases) {
      real dt = durations[p];
      if (dt == 0) continue;

      real lam1 = lambda1[p];
      s  = s_step(dt, lam1, lambda2, s);
      x1 = x1_step(dt, lam1, lambda2, sigma12, s, x1);
      x2 = x2_step(dt, lam1, lambda2, sigma21, s, x2);
      x12 = x12_step(dt, lam1, lambda2, sigma12, sigma21, s, x1, x2, x12);
    }

    prob_s[i] = s;
    prob_x1[i] = x1;
    prob_x2[i] = x2;
    prob_x12[i] = x12;
    seroprev[i] = 1 - s;
  }
}
