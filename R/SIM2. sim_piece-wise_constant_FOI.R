# -----------------------------------------
# Piece-wise constant FOI Simulation
# -----------------------------------------

library(tidyverse)
library(rstan)


# --- Model functions ----

# s(t)
s <- function(t, lambda1, lambda2, s_init) {
  exp(-t * (lambda1 + lambda2)) * s_init
}

# x1(t)
x1 <- function(t, lambda1, lambda2, sigma12, s_init, x1_init) {
  exp(-t * lambda2 * sigma12) * (
    x1_init +
      ((1 - exp(-t * (lambda1 + lambda2 - lambda2 * sigma12))) * s_init * lambda1) /
      (lambda1 + lambda2 - lambda2 * sigma12)
  )
}

# x2(t)
x2 <- function(t, lambda1, lambda2, sigma21, s_init, x2_init) {
  num <- (-1 + exp(-t * (lambda1 + lambda2 - lambda1 * sigma21))) * s_init * lambda2 -
    x2_init * (lambda1 + lambda2) + x2_init * lambda1 * sigma21
  denom <- -lambda2 + lambda1 * (sigma21 - 1)
  exp(-t * lambda1 * sigma21) * num / denom
}

# x12(t)
x12 <- function(t, lambda1, lambda2, sigma12, sigma21, s_init, x1_init, x2_init, x12_init) {
  A <- exp(-t * (lambda2 * sigma12 + lambda1 * sigma21))
  
  B <- -exp(t * lambda1 * sigma21) * (s_init * lambda1 + x1_init * (lambda1 + lambda2 - lambda2 * sigma12)) * (-lambda2 + lambda1 * (sigma21 - 1))
  
  C <- exp(t * (lambda2 * (sigma12 - 1) + lambda1 * (sigma21 - 1))) * 
    s_init * lambda1 * lambda2 * (sigma12 * (sigma21 - 1) - sigma21)
  
  D <- exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * 
    (s_init + x1_init + x12_init + x2_init) * 
    (-lambda1 + lambda2 * (sigma12 - 1)) * 
    (lambda1 + lambda2 - lambda1 * sigma21)
  
  E <- -exp(t * lambda2 * sigma12) * 
    (-lambda1 + lambda2 * (sigma12 - 1)) * 
    (s_init * lambda2 + x2_init * (lambda1 + lambda2 - lambda1 * sigma21))
  
  denom <- (lambda1 + lambda2 - lambda2 * sigma12) * (lambda1 + lambda2 - lambda1 * sigma21)
  
  -A * (B + C + D + E) / denom
}



p_s <- s(13.4, 0.32, 0.12, 1)
p_s
p_x1 <- x1(13.4, 0.32, 0.12, 15, 1, 0)
p_x1
p_x2 <-x2(13.4, 0.32, 0.12, 0.7, 1, 0)
p_x2
p_x12 <- x12(13.4, 0.32, 0.12, 15, 0.7, 1,0,0,0)
p_x12

sum(p_s, p_x1, p_x2, p_x12)

# --- Age structure ----
age_bins <- seq(5, 65, by = 5)
age_midpoints <- age_bins[-length(age_bins)] + diff(age_bins) / 2

# --- Parameters ----
lambda1_a <- 0.05
lambda1_b <- 0.01
lambda2_a <- 0.04
lambda2_b  <- 0.005
sigma12 <- 0.5
sigma21 <- 2

current_year <- 2010
cutoff_year <- 1995
cutoff_age <- current_year - cutoff_year # = 15

# --- Data simulation ----
set.seed(234)

sim_data_2 <- map_dfr(age_midpoints, function(age) {
  if (age <= cutoff_age) {
    # Fully exposed after the cut-off year
    s_init = 1
    x1_init = 0
    x2_init = 0
    x12_init = 0
    lambda1 <- lambda1_b
    lambda2 <- lambda2_b
    phase <- "after_cut-off"
    p_s   <- s(age, lambda1, lambda2, s_init)
    p_x1  <- x1(age, lambda1, lambda2, sigma12, s_init, x1_init)
    p_x2  <- x2(age, lambda1, lambda2, sigma21, s_init, x2_init)
    p_x12 <- x12(age, lambda1, lambda2, sigma12, sigma21, s_init, x1_init, x2_init, x12_init)
  } else {
    # Two-phase exposure
    phase <- "two_step"
    t1 <- age - cutoff_age  # Years before the cut-off year (lambda1_a)
    t2 <- cutoff_age        # Years after the cut-off year (lambda1_b)
    
    # Step 1: simulate years before the cut-off year under lambda1_a
    s_init = 1
    x1_init = 0
    x2_init = 0
    x12_init = 0
    lambda1 <- lambda1_a
    lambda2 <- lambda2_a
    S1    <- s(t1, lambda1, lambda2, s_init)
    X1_1  <- x1(t1, lambda1, lambda2, sigma12, s_init, x1_init)
    X2_1  <- x2(t1, lambda1, lambda2, sigma21, s_init, x2_init)
    X12_1 <- x12(t1, lambda1, lambda2, sigma12, sigma21, s_init, x1_init, x2_init, x12_init)
    
    # Step 2: continue (current year - cut-off year) more years under lambda1_b
    s_init = S1
    x1_init = X1_1
    x2_init = X2_1
    x12_init = X12_1
    lambda1 <- lambda1_b
    lambda2 <- lambda2_b
    p_s    <- s(t2, lambda1, lambda2, s_init)
    p_x1  <- x1(t2, lambda1, lambda2, sigma12, s_init, x1_init)
    p_x2  <- x2(t2, lambda1, lambda2, sigma21, s_init, x2_init)
    p_x12 <- x12(t2, lambda1, lambda2, sigma12, sigma21, s_init, x1_init, x2_init, x12_init)
    
    #p_s <- S1 * s(t2, lambda1_b, lambda2)
    #p_x1 <- X1_1 + S1 * x1(t2, lambda1_b, lambda2, sigma12)
    #p_x2 <- X2_1 + S1 * x2(t2, lambda1_b, lambda2, sigma21)
    #p_x12 <- X12_1 +
    #  X1_1 * x2(t2, lambda1_b, lambda2, sigma21) +
    #  X2_1 * x1(t2, lambda1_b, lambda2, sigma12) +
    #  S1 * x12(t2, lambda1_b, lambda2, sigma12, sigma21)
  }
  
  
  probs <- c(p_s, p_x1, p_x2, p_x12)
  #probs <- pmax(probs, 0)
  #probs <- probs / sum(probs)
  
  n <- sample(100:200, 1)
  counts <- rmultinom(1, size = n, prob = probs)
  
  tibble(
    age = age,
    phase = phase,
    n_tested = n,
    n_s = counts[1],
    n_denv1 = counts[2],
    n_denv2 = counts[3],
    n_denv12 = counts[4],
    p_s = probs[1],
    p_x1 = probs[2],
    p_x2 = probs[3],
    p_x12 = probs[4],
    sum = sum(p_s, p_x1, p_x2, p_x12) ## exceed 1 slightly????
  )
})

sim_data_2

# FITTING CONSTANT FOI MODEL (WITH INTERACTION) TO NEW SIMULATED DATA ----
age_fine <- seq(5, max(sim_data$age), by=1)
n_age_fine <- length(age_fine)
stan_data_2 <- list(
  N = nrow(sim_data_2),
  age = sim_data_2$age,
  n_tested = sim_data_2$n_tested,
  n_s = sim_data_2$n_s,
  n_denv1 = sim_data_2$n_denv1,
  n_denv2 = sim_data_2$n_denv2,
  n_denv12 = sim_data_2$n_denv12,
  y = as.matrix(sim_data_2[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = n_age_fine
)



stan_model_1 <- stan_model("stan/simulation_denv_2_serotypes.stan")
fit_3 <- sampling(stan_model_1, data = stan_data_2, iter = 4000, chains = 4, seed = 234)

print(fit_3, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), digits =3)

### Fitting PIECE-WISE CONSTANT FOI MODEL (WITH INTERACTION) ----
stan_data_2 <- list(
  N = nrow(sim_data_2),
  age = sim_data_2$age,
  n_tested = sim_data_2$n_tested,
  y = as.matrix(sim_data_2[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = length(age_fine),
  cutoff_age = current_year - cutoff_year
)
stan_model_2 <- stan_model("stan/two_piece-wise_FOI.stan")
fit_4 <- sampling(stan_model_2, data = stan_data_2, iter = 4000, chains = 4, seed = 234)
print(fit_4, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)



### Posterior predictive check ----

posterior <- rstan::extract(fit_4)

y <- stan_data_2$y                 # observed counts: 13 x 4
n_tested <- stan_data_2$n_tested  
age <- stan_data_2$age            

# Predicted category probabilities = count / n_tested
pred_probs <- array(NA, dim = dim(posterior$y_rep))  # y_rep: iterations x N x 4 = 4000 x 13 x 4

for (j in 1:4) {
  pred_probs[,,j] <- sweep(posterior$y_rep[,,j], 2, n_tested, "/")
}

make_pred_df <- function(prob_matrix, age_vec, comp_name, obs_vals, n_vals) {
  ci <- t(apply(prob_matrix, 2, quantile, probs = c(0.025, 0.5, 0.975)))
  tibble(
    age = age_vec,
    median = ci[, 2],
    lower = ci[, 1],
    upper = ci[, 3],
    observed = obs_vals / n_vals,
    lower_obs = observed - 1.96 * sqrt(observed * (1 - observed) / n_vals),
    upper_obs = observed + 1.96 * sqrt(observed * (1 - observed) / n_vals),
    compartment = comp_name
  )
}

prob_s <- 1 - pred_probs[, , 2] - pred_probs[, , 3] - pred_probs[, , 4]
df_s   <- make_pred_df(prob_s, age, "Susceptible", y[, 1], n_tested)
df_d1   <- make_pred_df(pred_probs[,,2], age, "DENV1 only", y[, 2], n_tested)
df_d2   <- make_pred_df(pred_probs[,,3], age, "DENV2 only", y[, 3], n_tested)
df_d12  <- make_pred_df(pred_probs[,,4], age, "DENV1 + DENV2", y[, 4], n_tested)

df_ppc <- bind_rows(df_s, df_d1, df_d2, df_d12)

get_ci_fine <- function(mat) {
  apply(mat, 2, quantile, probs = c(0.025, 0.5, 0.975))
}

ci_s    <- get_ci_fine(posterior$prob_s)
ci_d1   <- get_ci_fine(posterior$prob_x1)
ci_d2   <- get_ci_fine(posterior$prob_x2)
ci_d12  <- get_ci_fine(posterior$prob_x12)

age_fine <- stan_data$age_fine

df_smooth <- tibble(
  age = rep(age_fine, 4),
  median = c(ci_s[2,], ci_d1[2,], ci_d2[2,], ci_d12[2,]),
  lower = c(ci_s[1,], ci_d1[1,], ci_d2[1,], ci_d12[1,]),
  upper = c(ci_s[3,], ci_d1[3,], ci_d2[3,], ci_d12[3,]),
  compartment = rep(c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2"), each = length(age_fine))
)

df_smooth <- df_smooth %>%
  mutate(compartment = factor(compartment, levels = c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2")))

df_ppc <- df_ppc %>%
  mutate(compartment = factor(compartment, levels = c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2")))

g_PPC_sim_time_vary_inter <- ggplot(df_ppc, aes(x = age)) +
  # smooth line from age_fine
  geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
  
  # observed
  geom_point(aes(y = observed), color = "black", size = 2) +
  geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
  
  facet_wrap(~ compartment) +
  labs(
    title = "Posterior Predictive Check on simulation data\nTime-varying FOI model with interactions",
    x = "Age", y = "Probability"
  ) +
  theme_bw() +
  theme(
        plot.title = ggtext::element_markdown(),
        plot.title.position = "plot",
        strip.text = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18))

g_PPC_sim_time_vary_inter
