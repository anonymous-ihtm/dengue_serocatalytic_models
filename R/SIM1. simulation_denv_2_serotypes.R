# -----------------------------------------
# SIMULATION STUDY: MAIN MODEL (CONSTANT FOI WITH INTERACTION) ----
# -----------------------------------------

library(tidyverse)
library(rstan)
library(purrr)
library(patchwork)
library(bayesplot)
library(ggplot2)
library(ggtext)
library(loo)

# FUNCTIONS ----

# Susceptible to both serotypes
s <- function(t, lambda1, lambda2) {
  exp(-t * (lambda1 + lambda2))
}

# Infected with DENV1 only
x1 <- function(t, lambda1, lambda2, sigma12) {
  num <- exp(-t * lambda2 * sigma12) * (-1 + exp(t * (-lambda1 - lambda2 + lambda2 * sigma12))) * lambda1
  denom <- (-lambda1 - lambda2 + lambda2 * sigma12)
  return(num / denom)
}

# Infected with DENV2 only
x2 <- function(t, lambda1, lambda2, sigma21) {
  num <- exp(-t * lambda1 * sigma21) * (-1 + exp(t * (-lambda1 - lambda2 + lambda1 * sigma21))) * lambda2
  denom <- (-lambda1 - lambda2 + lambda1 * sigma21)
  return(num / denom)
}

# Infected with both serotypes
x12 <- function(t, lambda1, lambda2, sigma12, sigma21) {
  expPart <- exp(-t * lambda2 * sigma12 - t * lambda1 * sigma21)
  
  term <- exp(t * lambda1 * sigma21) * lambda1^2 -
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1^2 +
    exp(t * lambda2 * sigma12) * lambda1 * lambda2 +
    exp(t * lambda1 * sigma21) * lambda1 * lambda2 -
    2 * exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 +
    exp(t * lambda2 * sigma12) * lambda2^2 -
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda2^2 +
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 -
    exp(t * (-lambda1 - lambda2) + t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 -
    exp(t * lambda2 * sigma12) * lambda2^2 * sigma12 +
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda2^2 * sigma12 -
    exp(t * lambda1 * sigma21) * lambda1^2 * sigma21 +
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1^2 * sigma21 +
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma21 -
    exp(t * (-lambda1 - lambda2) + t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma21 -
    exp(t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 * sigma21 +
    exp(t * (-lambda1 - lambda2) + t * lambda2 * sigma12 + t * lambda1 * sigma21) * lambda1 * lambda2 * sigma12 * sigma21
  
  denom <- (-lambda1 - lambda2 + lambda2 * sigma12) * (lambda1 + lambda2 - lambda1 * sigma21)
  
  return(expPart * term / denom)
}

# Check if the functions in Stan and R are the same ----

#expose_stan_functions(stan_model)
#s(50, 0.32, 0.12)
#p_s <- p_s(50, 0.32, 0.12)
#p_s

#x1(50, 0.32, 0.12, 1.5)
#p_x1 <- p_x1(50, 0.32, 0.12, 1.5)
#p_x1

#x2(50, 0.32, 0.12, 2)
#p_x2 <- p_x2(50, 0.32, 0.12, 2)
#p_x2

#x12(50, 0.32, 0.12, 1.5, 2)
#p_x12 <- p_x12(50, 0.32, 0.12, 1.5, 2)
#p_x12

#sum(p_s, p_x1, p_x2, p_x12) #Must add up to 1

# DATA SIMULATION ----

# Constant FOI with interaction ----

# Set known values of parameters
lambda1 <- 0.05 # DENV1 FOI  (Review of Peru)
lambda2 <- 0.04 # DENV2 FOI
sigma12 <- 15  # Enhancement of DENV2 after DENV1
sigma21 <- 0.7  # Protection of DENV1 after DENV2

# Age structure
age_bins <- seq(5, 65, by = 5)
age_midpoints <- age_bins[-length(age_bins)] + diff(age_bins) / 2

set.seed(234)

# Simulate data using above functions
sim_data_1 <- map_dfr(age_midpoints, function(age) {
  p_s <- s(age, lambda1, lambda2)
  p_x1 <- x1(age, lambda1, lambda2, sigma12)
  p_x2 <- x2(age, lambda1, lambda2, sigma21)
  p_x12 <- x12(age, lambda1, lambda2, sigma12, sigma21)
  
  n_per_bin <- sample(100:200, 1)
  probs <- c(p_s, p_x1, p_x2, p_x12)
  
  counts <- rmultinom(1, size = n_per_bin, prob = probs)
  
  tibble(
    age = age,
    n_tested = n_per_bin,
    n_s = counts[1],
    n_denv1 = counts[2],
    n_denv2 = counts[3],
    n_denv12 = counts[4],
    p_s = p_s,
    p_x1 = p_x1,
    p_x2 = p_x2,
    p_x12 =  p_x12,
    sum = sum(p_s, p_x1, p_x2, p_x12)
  )
})

sim_data_1


# FITTING CONSTANT FOI MODEL ----
age_fine <- seq(5, max(sim_data$age), by=1)
n_age_fine <- length(age_fine)
stan_data_1 <- list(
  N = nrow(sim_data_1),
  age = sim_data_1$age,
  n_tested = sim_data_1$n_tested,
  n_s = sim_data_1$n_s,
  n_denv1 = sim_data_1$n_denv1,
  n_denv2 = sim_data_1$n_denv2,
  n_denv12 = sim_data_1$n_denv12,
  y = as.matrix(sim_data_1[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = n_age_fine
)

stan_model_1 <- stan_model("stan/simulation_denv_2_serotypes.stan")

fit_1 <- sampling(stan_model, data = stan_data, iter = 4000, chains = 4, seed = 234, control = list(adapt_delta = 0.95))
print(fit_1, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), digits = 3)

#shinystan::launch_shinystan(fit)

# FITTING NO-INTERACTION MODEL ----
fit_2 <- stan(
  file = "stan/constant_foi_no_interaction.stan",
  data = stan_data_1,
  chains = 4,
  iter = 4000,
  seed = 1234,
  control = list(adapt_delta = 0.95)
)

print(fit_2, pars = c("lambda1", "lambda2"), digits = 3)

# COMPARE 2 MODELS ----

log_lik_matrix_1 <- extract_log_lik(fit_1, parameter_name = "log_lik")
log_lik_matrix_2 <- extract_log_lik(fit_2, parameter_name = "log_lik")

loo1 <- loo(log_lik_matrix_1)
loo1
loo2 <- loo(log_lik_matrix_2)
loo2

loo_compare(loo1, loo2)

### Posterior predictive check ----
## observed counts
y <- stan_data_1$y                 
n_tested <- stan_data_1$n_tested  
age <- stan_data_1$age            

## Model with interaction  ----
posterior <- rstan::extract(fit_1) # extract the posterior data

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

g_PPC_sim_constant_inter <- ggplot(df_ppc, aes(x = age)) +
  geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(aes(y = observed), color = "black", size = 2) +
  geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
  facet_wrap(~ compartment) +
  labs(
    #title = "Model 1: Without interaction",
    title = "Model 2: With interaction",
    #title = "<span style='font-size:16pt'><b>Posterior Predictive Check of age-stratified infection probabilities: Model estimates versus simulated data</b></span><br>
    #        <span style='font-size:12pt'>  </span><br>",
    x = "Age", 
    y = NULL
    #y = "Probability"
  ) +
  theme_bw()+
  theme(
    #plot.title = ggtext::element_markdown(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

## Model without interaction  ----
posterior <- rstan::extract(fit_2) # extract the posterior data

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

ci_s_int    <- get_ci_fine(posterior$prob_s)
ci_d1_int   <- get_ci_fine(posterior$prob_x1)
ci_d2_int   <- get_ci_fine(posterior$prob_x2)
ci_d12_int  <- get_ci_fine(posterior$prob_x12)

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

g_PPC_sim_constant_no_inter <- ggplot(df_ppc, aes(x = age)) +
  geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(aes(y = observed), color = "black", size = 2) +
  geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
  facet_wrap(~ compartment) +
  labs(
    title = "Model 1: Without interaction",
    #title = "Model 2: With interaction",
    #title = "<span style='font-size:16pt'><b>Posterior Predictive Check of age-stratified infection probabilities: Model estimates versus simulated data</b></span><br>
    #        <span style='font-size:12pt'>  </span><br>",
    x = "Age", 
    y = NULL
    #y = "Probability"
  ) +
  theme_bw()+
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.title.position = "plot",
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

g_PPC_sim_constant_inter
g_PPC_sim_constant_no_inter
g_PPC_sim_constant_no_inter + g_PPC_sim_constant_inter

#----

# === Utility functions ===
get_ci_fine <- function(mat) {
  apply(mat, 2, quantile, probs = c(0.025, 0.5, 0.975))
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

# === Observed data ===
age_obs <- stan_data_1$age
n_tested <- stan_data_1$n_tested
y <- stan_data_1$y
age_fine <- stan_data_1$age_fine  # fine resolution age for model prediction

# === Model 1: No interaction ===
posterior_1 <- rstan::extract(fit_2)
ci_s_1   <- get_ci_fine(posterior_1$prob_s)
ci_d1_1  <- get_ci_fine(posterior_1$prob_x1)
ci_d2_1  <- get_ci_fine(posterior_1$prob_x2)
ci_d12_1 <- get_ci_fine(posterior_1$prob_x12)

df_smooth_1 <- tibble(
  age = rep(age_fine, 4),
  median = c(ci_s_1[2,], ci_d1_1[2,], ci_d2_1[2,], ci_d12_1[2,]),
  lower = c(ci_s_1[1,], ci_d1_1[1,], ci_d2_1[1,], ci_d12_1[1,]),
  upper = c(ci_s_1[3,], ci_d1_1[3,], ci_d2_1[3,], ci_d12_1[3,]),
  compartment = rep(c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2"), each = length(age_fine)),
  model = "Model 1: No interaction"
)

# === Model 2: With interaction ===
posterior_2 <- rstan::extract(fit_1)
ci_s_2   <- get_ci_fine(posterior_2$prob_s)
ci_d1_2  <- get_ci_fine(posterior_2$prob_x1)
ci_d2_2  <- get_ci_fine(posterior_2$prob_x2)
ci_d12_2 <- get_ci_fine(posterior_2$prob_x12)

df_smooth_2 <- tibble(
  age = rep(age_fine, 4),
  median = c(ci_s_2[2,], ci_d1_2[2,], ci_d2_2[2,], ci_d12_2[2,]),
  lower = c(ci_s_2[1,], ci_d1_2[1,], ci_d2_2[1,], ci_d12_2[1,]),
  upper = c(ci_s_2[3,], ci_d1_2[3,], ci_d2_2[3,], ci_d12_2[3,]),
  compartment = rep(c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2"), each = length(age_fine)),
  model = "Model 2: With interaction"
)

# === Combine both posterior predictions ===
df_smooth_all <- bind_rows(df_smooth_1, df_smooth_2) %>%
  mutate(
    compartment = factor(compartment, levels = c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2")),
    model = factor(model, levels = c("Model 1: No interaction", "Model 2: With interaction"))
  )

# === Observed data summary ===
posterior_rep <- posterior_1$y_rep  # or use posterior_2; same dimensions
pred_probs <- array(NA, dim = dim(posterior_rep))
for (j in 1:4) {
  pred_probs[,,j] <- sweep(posterior_rep[,,j], 2, n_tested, "/")
}

df_obs <- bind_rows(
  make_pred_df(1 - pred_probs[,,2] - pred_probs[,,3] - pred_probs[,,4], age_obs, "Susceptible", y[,1], n_tested),
  make_pred_df(pred_probs[,,2], age_obs, "DENV1 only", y[,2], n_tested),
  make_pred_df(pred_probs[,,3], age_obs, "DENV2 only", y[,3], n_tested),
  make_pred_df(pred_probs[,,4], age_obs, "DENV1 + DENV2", y[,4], n_tested)
)

df_obs <- df_obs %>%
  select(age, observed, lower_obs, upper_obs, compartment) %>%
  mutate(compartment = factor(compartment, levels = levels(df_smooth_all$compartment)))

# === Final plot ===
ggplot() +
  # Observed data
  geom_point(data = df_obs, aes(x = age, y = observed), color = "black", size = 2) +
  geom_errorbar(data = df_obs, aes(x = age, ymin = lower_obs, ymax = upper_obs), width = 0.3) +
  
  # Posterior predictions
  geom_line(data = df_smooth_all, aes(x = age, y = median, color = model), linewidth = 1) +
  geom_ribbon(data = df_smooth_all, aes(x = age, ymin = lower, ymax = upper, fill = model), alpha = 0.2) +
  
  facet_wrap(~ compartment, scales = "free_y") +
  scale_color_manual(values = c("Model 1: No interaction" = "red", "Model 2: With interaction" = "blue")) +
  scale_fill_manual(values = c("Model 1: No interaction" = "red", "Model 2: With interaction" = "blue")) +
  
  labs(
    title = "Posterior Predictive Check: Comparing Two Models vs Simulated Data",
    x = "Age",
    y = "Probability",
    color = "Model",
    fill = "Model"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    strip.text = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )


### Check Fisher test ----
sim_list_with_p <- sim_data_1 %>%
    mutate(
      p_s = n_s / n_tested,
      p_x1 = n_denv1 / n_tested,
      p_x2 = n_denv2 / n_tested,
      p_x12 = n_denv12 / n_tested,
      q_x1 = (n_denv1 + n_denv12) / n_tested,
      q_x2 = (n_denv2 + n_denv12) / n_tested,
      q_x12 = q_x1 * q_x2
    ) %>%
    rowwise() %>%
    mutate(
      expected_infected = round(n_tested * q_x12),
      expected_uninfected = n_tested - expected_infected,
      observed_uninfected = n_tested - n_denv12,
      p_value = fisher.test(
        matrix(c(n_denv12, observed_uninfected,
                 expected_infected, expected_uninfected), nrow = 2)
      )$p.value
    ) %>%
    ungroup()

sim_all_pval <- bind_rows(sim_list_with_p)

ggplot(sim_all_pval, aes(x = age)) +
  geom_line(aes(y = p_x12, colour = "Observed p_x12")) +
  geom_line(aes(y = q_x12, colour = "Estimated q_x12")) +
  geom_text(aes(y = p_x12 + 0.02, label = ifelse(p_value < 0.05, "p<0.05", sprintf("p=%.3f", p_value))), size = 2, hjust = 0) +
  theme_minimal() +
  theme(legend.position = "top") + 
  labs(
    x = "Age", y = "Probability",
    colour = "Legend"
  )


ggplot(sim_all_pval, aes(x = factor(age))) +
  geom_point(aes(y = p_x12, colour = "Observed", shape = p_value < 0.05), size = 3) +
  geom_point(aes(y = q_x12, colour = "Expected"), size = 3) +
  geom_text(
    aes(
      y = p_x12 + 0.02,
      label = ifelse(p_value < 0.05, "p<0.05", sprintf("p=%.3f", p_value))
    ),
    size = 3, hjust = 0
  ) +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 19), name = "p < 0.05") +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    x = "Age midpoint", y = "Probability",
    colour = "Seroprevalence of DENV1 + DENV2"
  )

