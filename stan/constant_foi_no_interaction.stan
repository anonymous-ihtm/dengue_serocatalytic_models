// constant_foi_no_interaction.stan

### STAN MODEL 2: CONSTANT FOI WITH NO INTERACTION

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
  int<lower=1> n_age_fine;
  vector[n_age_fine] age_fine;
  int<lower=0> n_tested[N];
  int<lower=0> y[N, 4];
}

transformed data {
  real sigma12 = 1.0; // no interaction
  real sigma21 = 1.0; // no interaction
}

parameters {
  real<lower=0> lambda1;
  real<lower=0> lambda2;
}

model {
  lambda1 ~ uniform(0.0, 2.0);
  lambda2 ~ uniform(0.0, 2.0);

  for (i in 1:N) {
    real t = age[i];
    real s = p_s(t, lambda1, lambda2);
    real x1 = p_x1(t, lambda1, lambda2, sigma12);
    real x2 = p_x2(t, lambda1, lambda2, sigma21);
    real x12 = p_x12(t, lambda1, lambda2, sigma12, sigma21);

    vector[4] probs;
    probs[1] = s;
    probs[2] = x1;
    probs[3] = x2;
    probs[4] = x12;

    y[i] ~ multinomial(probs);
  }
}

generated quantities {
  int y_rep[N, 4];
  vector[N] log_lik;

  vector[n_age_fine] prob_s;
  vector[n_age_fine] prob_x1;
  vector[n_age_fine] prob_x2;
  vector[n_age_fine] prob_x12;
  vector[n_age_fine] seroprev;

  for (i in 1:N) {
    real t = age[i];
    real s = p_s(t, lambda1, lambda2);
    real x1 = p_x1(t, lambda1, lambda2, sigma12);
    real x2 = p_x2(t, lambda1, lambda2, sigma21);
    real x12 = p_x12(t, lambda1, lambda2, sigma12, sigma21);
    vector[4] probs = [s, x1, x2, x12]';
    y_rep[i] = multinomial_rng(probs, n_tested[i]);
    log_lik[i] = multinomial_lpmf(y[i] | probs);
  }

  for (i in 1:n_age_fine) {
    real t = age_fine[i];
    prob_s[i]   = p_s(t, lambda1, lambda2);
    prob_x1[i]  = p_x1(t, lambda1, lambda2, sigma12);
    prob_x2[i]  = p_x2(t, lambda1, lambda2, sigma21);
    prob_x12[i] = p_x12(t, lambda1, lambda2, sigma12, sigma21);
    seroprev[i] = 1 - prob_s[i];
  }
}
