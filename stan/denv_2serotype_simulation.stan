data {
  int<lower=0> N;
  vector[N] age;
  int<lower=0> n_tested[N];
  int<lower=0> n_denv1[N];
  int<lower=0> n_denv2[N];
}

parameters {
  real<lower=0> lambda1;
  real<lower=0> lambda2;
  real<lower=0, upper=2> sigma12;
  real<lower=0, upper=2> sigma21;
}

model {
  lambda1 ~ normal(0.05, 0.02);
  lambda2 ~ normal(0.04, 0.02);
  #sigma12 ~ normal(1,0.3);
  #sigma21 ~ normal(1,0.3);
  sigma12 ~ lognormal(log(1),0.3);
  sigma21 ~ lognormal(log(1),0.3);
  #sigma12 ~ gamma(10,10);
  #sigma21 ~ gamma(10,10);
  
  for (i in 1:N) {
    real p1_naive = 1 - exp(-lambda1 * age[i]);
    real p2_naive = 1 - exp(-lambda2 * age[i]);

    real adj_age1 = age[i] * sigma21 * p2_naive + age[i] * (1 - p2_naive);
    real adj_age2 = age[i] * sigma12 * p1_naive + age[i] * (1 - p1_naive);

    real p1 = 1 - exp(-lambda1 * adj_age1);
    real p2 = 1 - exp(-lambda2 * adj_age2);

    n_denv1[i] ~ binomial(n_tested[i], p1);
    n_denv2[i] ~ binomial(n_tested[i], p2);
  }
}

generated quantities {
  int pred_denv1[N];
  int pred_denv2[N];
  for (i in 1:N) {
    real p1_naive = 1 - exp(-lambda1 * age[i]);
    real p2_naive = 1 - exp(-lambda2 * age[i]);

    real adj_age1 = age[i] * sigma21 * p2_naive + age[i] * (1 - p2_naive);
    real adj_age2 = age[i] * sigma12 * p1_naive + age[i] * (1 - p1_naive);

    real p1 = 1 - exp(-lambda1 * adj_age1);
    real p2 = 1 - exp(-lambda2 * adj_age2);

    pred_denv1[i] = binomial_rng(n_tested[i], p1);
    pred_denv2[i] = binomial_rng(n_tested[i], p2);
  }
}