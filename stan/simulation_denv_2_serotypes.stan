### STAN MODEL 1: CONSTANT FOI WITH INTERACTION

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
  int<lower=1> N;                  // number of age groups
  vector[N] age;                  // age midpoints
  int<lower=1> n_age_fine;
  vector[n_age_fine] age_fine;
  int<lower=0> n_tested[N];       // number of individuals tested in each age group
  int<lower=0> y[N, 4];           // counts of [S, DENV1 only, DENV2 only, DENV1+DENV2]
}

parameters {
  real<lower=0> lambda1; // DENV-1 FOI
  real<lower=0> lambda2; // DENV-2 FOI
  real<lower=0> sigma12; // modification of DENV2 susceptibility after DENV1 infection (ADE > 1 or protection < 1)
  real<lower=0> sigma21; // modification of DENV1 susceptibility after DENV2 infection
  #real log_lambda1;
  #real log_lambda2;
  #real log_sigma12;
  #real log_sigma21;
}


#transformed parameters {
#  real lambda1 = exp(log_lambda1);
#  real lambda2 = exp(log_lambda2);
#  real sigma12 = exp(log_sigma12);
#  real sigma21 = exp(log_sigma21);
#}

model {
  lambda1 ~ uniform(0.0, 2.0);
  lambda2 ~ uniform(0.0, 2.0);
  
  sigma12 ~ uniform(0.0, 20.0);
  sigma21 ~ uniform(0.0, 20.0);
  #sigma12 ~ exponential(1);
  #sigma21 ~ exponential(1);
  
  #log_lambda1 ~ normal(-3, 1);
  #log_lambda2 ~ normal(-3, 1);
  #log_sigma12 ~ normal(0, 10);
  #log_sigma21 ~ normal(0, 10);
  
  #sigma12 ~ uniform(0.0, 50.0);
  #sigma21 ~ uniform(0.0, 50.0);
  #sigma12 ~ uniform(0.0, 2.0);
  #sigma21 ~ uniform(0.0, 2.0);
  
  #lambda1 ~ normal(0.05, 0.03);
  #lambda2 ~ normal(0.05, 0.03);
  #sigma12 ~ normal(1.0, 1.0);  // Centered at "no effect"
  #sigma21 ~ normal(1.0, 1.0);


  for (i in 1:N) {
    real t = age[i];
    real s = p_s(t, lambda1, lambda2); // susceptible (never infected)
    real x1 = p_x1(t, lambda1, lambda2, sigma12); // being infected by DENV1 but not yet DENV2
    real x2 = p_x2(t, lambda1, lambda2, sigma21); // being infected by DENV2 but not yet DENV1
    real x12 = p_x12(t, lambda1, lambda2, sigma12, sigma21); // DENV1 then DENV2 OR vice versa

    vector[4] probs;
    probs[1] = s;
    probs[2] = x1;
    probs[3] = x2;
    probs[4] = x12;

    y[i] ~ multinomial(probs);
  }
}

 generated quantities {
   vector[N] log_lik;
  // 1. Posterior predictive counts for observed age groups
  int y_rep[N, 4];
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
    
    y_rep[i] = multinomial_rng(probs, n_tested[i]);
    log_lik[i] = multinomial_lpmf(y[i] | probs);
  }
  // 2. Predicted probabilities over fine age grid
  vector[n_age_fine] prob_s;
  vector[n_age_fine] prob_x1;
  vector[n_age_fine] prob_x2;
  vector[n_age_fine] prob_x12;
  vector[n_age_fine] seroprev;

  for (i in 1:n_age_fine) {
    real t = age_fine[i];

    real s = p_s(t, lambda1, lambda2);
    real x1 = p_x1(t, lambda1, lambda2, sigma12);
    real x2 = p_x2(t, lambda1, lambda2, sigma21);
    real x12 = p_x12(t, lambda1, lambda2, sigma12, sigma21);

    #real total = s + x1 + x2 + x12;

    prob_s[i]    = s;
    prob_x1[i]   = x1;
    prob_x2[i]   = x2;
    prob_x12[i]  = x12;
    seroprev[i]  = 1 - prob_s[i];
  }
}
