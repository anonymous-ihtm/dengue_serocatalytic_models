functions {
  real p_s(real t, real lambda1, real lambda2) {
    return exp(-t * (lambda1 + lambda2));
  }
  real p_x1(real t, real lambda1, real lambda2, real sigma12) {
    real num1 = exp(-t * lambda2 * sigma12) * (-1 + exp(t * (-lambda1 - lambda2 + lambda2 * sigma12))) * lambda1;
    real den1 = (-lambda1 - lambda2 + lambda2 * sigma12);
    return num1 / den1;
  }
  real p_x2(real t, real lambda1, real lambda2, real sigma21) {
    real num2 = exp(-t * lambda1 * sigma21) * (-1 + exp(t * (-lambda1 - lambda2 + lambda1 * sigma21))) * lambda2;
    real den2 = (-lambda1 - lambda2 + lambda1 * sigma21);
    return num2 / den2;
  }
  real p_x12(real t, real lambda1, real lambda2, real sigma12, real sigma21) {
      real expPart = exp(-t * lambda2 * sigma12 - t * lambda1 * sigma21);
    real term = 
      exp(t * lambda1 * sigma21) * square(lambda1) -
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * square(lambda1) +
      exp(t * lambda2 * sigma12) * lambda1 * lambda2 +
      exp(t * lambda1 * sigma21) * lambda1 * lambda2 -
      2 * exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 +
      exp(t * lambda2 * sigma12) * square(lambda2) -
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * square(lambda2) +
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 -
      exp(t * (-lambda1 - lambda2) + t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 -
      exp(t * lambda2 * sigma12) * square(lambda2) * sigma12 +
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * square(lambda2) * sigma12 -
      exp(t * lambda1 * sigma21) * square(lambda1) * sigma21 +
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * square(lambda1) * sigma21 +
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma21 -
      exp(t * (-lambda1 - lambda2) + t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma21 -
      exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 * sigma21 +
      exp(t * (-lambda1 - lambda2) + t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 * sigma21;

    real denom = (-lambda1 - lambda2 + lambda2 * sigma12) * (lambda1 + lambda2 - lambda1 * sigma21);
    return expPart * term / denom;
  }
}

data {
  int<lower=1> N;
  vector[N] age;
  int<lower=1> K;
  real cutoff_years[K];
  int<lower=0> n_tested[N];
  int<lower=0> y[N, 4];
  int<lower=1> n_age_fine;
  vector[n_age_fine] age_fine;
}

parameters {
  real<lower=0> lambda1[K+1];
  real<lower=0> lambda2[K+1];
  real<lower=0> sigma12;
  real<lower=0> sigma21;
}

model {
  lambda1 ~ normal(0.1, 0.05);
  lambda2 ~ normal(0.1, 0.05);
  sigma12 ~ normal(1, 3);
  sigma21 ~ normal(1, 3);

  for (i in 1:N) {
    real t = age[i];
    real s = 1;
    real x1 = 0;
    real x2 = 0;
    real x12 = 0;
    real t_start = 0;

    for (k in 1:(K+1)) {
      real t_end = (k <= K) ? cutoff_years[k] : t;
      real dt = fmin(t_end, t) - t_start;
      if (dt > 0) {
        s *= p_s(dt, lambda1[k], lambda2[k]);
        x1 += s * p_x1(dt, lambda1[k], lambda2[k], sigma12);
        x2 += s * p_x2(dt, lambda1[k], lambda2[k], sigma21);
        x12 += s * p_x12(dt, lambda1[k], lambda2[k], sigma12, sigma21);
      }
      t_start = t_end;
    }

    vector[4] probs = [s, x1, x2, x12]';
    if (sum(probs) <= 0) {
      probs = rep_vector(1e-6, 4);
    } else {
      probs /= sum(probs);
    }
    y[i] ~ multinomial(probs);
  }
}
