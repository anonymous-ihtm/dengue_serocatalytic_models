# -----------------------------------------
# Scenario Simulation & Fit constant FOI model
# -----------------------------------------

library(tidyverse)
library(rstan)
library(ggplot2)

# --- Load Stan model ----
sm <- stan_model("stan/simulation_denv_2_serotypes.stan")

# --- Simulation functions ----
s <- function(t, lambda1, lambda2) exp(-t * (lambda1 + lambda2))
x1 <- function(t, lambda1, lambda2, sigma12) {
  num <- exp(-t * lambda2 * sigma12) * (-1 + exp(t * (-lambda1 - lambda2 + lambda2 * sigma12))) * lambda1
  den <- (-lambda1 - lambda2 + lambda2 * sigma12)
  num / den
}
x2 <- function(t, lambda1, lambda2, sigma21) {
  num <- exp(-t * lambda1 * sigma21) * (-1 + exp(t * (-lambda1 - lambda2 + lambda1 * sigma21))) * lambda2
  den <- (-lambda1 - lambda2 + lambda1 * sigma21)
  num / den
}
x12 <- function(t, lambda1, lambda2, sigma12, sigma21) {
  expPart <- exp(-t * lambda2 * sigma12 - t * lambda1 * sigma21)
  term <- exp(t * lambda1 * sigma21) * lambda1^2 -
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda1^2 +
    exp(t * lambda2 * sigma12) * lambda1 * lambda2 +
    exp(t * lambda1 * sigma21) * lambda1 * lambda2 -
    2 * exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 +
    exp(t * lambda2 * sigma12) * lambda2^2 -
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda2^2 +
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 * sigma12 -
    exp(t * (-lambda1 - lambda2 + lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 * sigma12 -
    exp(t * lambda2 * sigma12) * lambda2^2 * sigma12 +
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda2^2 * sigma12 -
    exp(t * lambda1 * sigma21) * lambda1^2 * sigma21 +
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda1^2 * sigma21 +
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 * sigma21 -
    exp(t * (-lambda1 - lambda2 + lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 * sigma21 -
    exp(t * (lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 * sigma12 * sigma21 +
    exp(t * (-lambda1 - lambda2 + lambda2 * sigma12 + lambda1 * sigma21)) * lambda1 * lambda2 * sigma12 * sigma21
  denom <- (-lambda1 - lambda2 + lambda2 * sigma12) * (lambda1 + lambda2 - lambda1 * sigma21)
  expPart * term / denom
}

# --- Simulate and fit ----
simulate_and_extract <- function(scenario, sigma12, sigma21, lambda1 = 0.05, lambda2 = 0.04) {
  age_bins <- seq(5, 65, by = 5)
  age_midpoints <- age_bins[-length(age_bins)] + diff(age_bins)/2
  
  sim_data_3 <- map_dfr(age_midpoints, function(age) {
    probs <- c(
      s(age, lambda1, lambda2),
      x1(age, lambda1, lambda2, sigma12),
      x2(age, lambda1, lambda2, sigma21),
      x12(age, lambda1, lambda2, sigma12, sigma21)
    )
    n <- sample(1000:2000, 1)
    counts <- rmultinom(1, size = n, prob = probs)
    tibble(age = age, 
           n_tested = n,
           n_s = counts[1], 
           n_denv1 = counts[2],
           n_denv2 = counts[3], 
           n_denv12 = counts[4])
  })
  
  stan_data_3 <- list(
    N = nrow(sim_data_3),
    age = sim_data_3$age,
    n_tested = sim_data_3$n_tested,
    y = as.matrix(sim_data_3[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = seq(5, 65, 1),
    n_age_fine = length(seq(5, 65, 1))
  )
  
  fit <- sampling(sm, data = stan_data_3, chains = 4, iter = 8000, seed = 123)
  draws <- as.data.frame(fit) %>%
    select(sigma12, sigma21) %>%
    mutate(scenario = scenario)
  
  return(draws)
}

# --- Define 7 scenarios with friendly labels ---
scenarios <- tribble(
  ~scenario,                   ~sigma12, ~sigma21,
  "σ12=σ21=2",            2.0, 2.0,
  "σ12=2, σ21=0.5",        2.0, 0.5,
  "σ12=0.5, σ21=2",        0.5, 2.0,
  "σ12=σ21=0.5",     0.5, 0.5,
  "σ12=σ21=1",                  1.0, 1.0,
  "σ12=2, σ21=1",      2.0, 1.0,
  "σ21=1, σ21=2",      1.0, 2.0
)

# --- Run all simulations ----
posterior_all <- pmap_dfr(scenarios, simulate_and_extract)

# --- Prepare data for plotting ----
posterior_long <- posterior_all %>%
  pivot_longer(cols = c(sigma12, sigma21),
               names_to = "parameter", values_to = "value") %>%
  mutate(parameter = recode(parameter,
                            "sigma12" = "σ[12]",
                            "sigma21" = "σ[21]"))

# True values
true_vals <- tribble(
  ~scenario, ~parameter, ~true_value,
  "σ12=σ21=2",        "σ[12]", 2,
  "σ12=σ21=2",        "σ[21]", 2,
  "σ12=2, σ21=0.5",    "σ[12]", 2,
  "σ12=2, σ21=0.5",    "σ[21]", 0.5,
  "σ12=0.5, σ21=2",    "σ[12]", 0.5,
  "σ12=0.5, σ21=2",    "σ[21]", 2,
  "σ12=σ21=0.5", "σ[12]", 0.5,
  "σ12=σ21=0.5", "σ[21]", 0.5,
  "σ12=σ21=1",              "σ[12]", 1,
  "σ12=σ21=1",              "σ[21]", 1,
  "σ12=2, σ21=1",  "σ[12]", 2,
  "σ12=2, σ21=1",  "σ[21]", 1,
  "σ21=1, σ21=2",  "σ[12]", 1,
  "σ21=1, σ21=2",  "σ[21]", 2
)

# Posterior means
posterior_means <- posterior_long %>%
  group_by(scenario, parameter) %>%
  summarise(post_mean = mean(value), .groups = "drop")

# Merge
plot_data <- posterior_long %>%
  left_join(true_vals, by = c("scenario", "parameter")) %>%
  left_join(posterior_means, by = c("scenario", "parameter")) %>%
  mutate(parameter = factor(parameter, levels = c("σ[12]", "σ[21]")))

# --- Plot ----
ggplot(plot_data, aes(x = value, fill = parameter)) +
  geom_density(alpha = 0.5, color = "black") +
  geom_vline(aes(xintercept = post_mean, linetype = "Posterior mean"),
             color = "black", linewidth = 0.5) +
  geom_vline(aes(xintercept = true_value, linetype = "True value"),
             color = "red", linewidth = 0.5) +
  facet_wrap(~ scenario + parameter, scales = "free", ncol = 4) +
  scale_fill_manual(values = c("σ[12]" = "#FBB4AE", "σ[21]" = "#B3CDE3")) +
  scale_linetype_manual(name = NULL,
                        values = c("Posterior mean" = "solid", "True value" = "dashed")) +
  labs(
    title = "Posterior distributions of σ[12] and σ[21] across 7 interaction scenarios",
    x = "Posterior estimate", y = "Density", fill = "Parameter"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    legend.position = c(0.8, -0.03),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = "white", color = "gray80"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  )


###

# ---- Replace with your real posterior draws and credible intervals ----
# Example structure
set.seed(123)
plot_data <- expand_grid(
  scenario = c(
    "σ12=σ21=2", "σ12=2, σ21=0.5", "σ12=0.5, σ21=2", 
    "σ12=σ21=0.5", "σ12=σ21=1", "σ12=2, σ21=1", "σ21=1, σ21=2"
  ),
  parameter = c("σ[12]", "σ[21]")
) %>%
  mutate(
    true_value = case_when(
      scenario == "σ12=σ21=2" & parameter == "σ[12]" ~ 2,
      scenario == "σ12=σ21=2" & parameter == "σ[21]" ~ 2,
      scenario == "σ12=2, σ21=0.5" & parameter == "σ[12]" ~ 2,
      scenario == "σ12=2, σ21=0.5" & parameter == "σ[21]" ~ 0.5,
      scenario == "σ12=0.5, σ21=2" & parameter == "σ[12]" ~ 0.5,
      scenario == "σ12=0.5, σ21=2" & parameter == "σ[21]" ~ 2,
      scenario == "σ12=σ21=0.5" ~ 0.5,
      scenario == "σ12=σ21=1" ~ 1,
      scenario == "σ12=2, σ21=1" & parameter == "σ[12]" ~ 2,
      scenario == "σ12=2, σ21=1" & parameter == "σ[21]" ~ 1,
      scenario == "σ21=1, σ21=2" & parameter == "σ[12]" ~ 1,
      scenario == "σ21=1, σ21=2" & parameter == "σ[21]" ~ 2,
      TRUE ~ NA_real_
    ),
    median = true_value + rnorm(n(), 0, 0.05),  # Fake post. median
    lower = median - runif(n(), 0.05, 0.1),
    upper = median + runif(n(), 0.05, 0.1)
  )


# Ensure Greek expression labels
plot_data <- plot_data %>%
  mutate(
    parameter = factor(parameter, levels = c("σ[12]", "σ[21]")),
    parameter_expr = recode(parameter,
                            "σ[12]" = "sigma[12]",
                            "σ[21]" = "sigma[21]"
    )
  )

plot_data <- plot_data %>%
  mutate(
    parameter_expr = factor(parameter, levels = c("σ[12]", "σ[21]")),
    scenario_expr = recode(scenario,
                           "σ12=σ21=2"     = "sigma[12]==2*','~sigma[21]==2",
                           "σ12=2, σ21=0.5"= "sigma[12]==2*','~sigma[21]==0.5",
                           "σ12=0.5, σ21=2"= "sigma[12]==0.5*','~sigma[21]==2",
                           "σ12=σ21=0.5"   = "sigma[12]==0.5*','~sigma[21]==0.5",
                           "σ12=σ21=1"     = "sigma[12]==1*','~sigma[21]==1",
                           "σ12=2, σ21=1"  = "sigma[12]==2*','~sigma[21]==1",
                           "σ21=1, σ21=2"  = "sigma[12]==1*','~sigma[21]==2"
    ),
    true_param = case_when(
      parameter == "σ[12]" ~ "σ[12] (true)",
      parameter == "σ[21]" ~ "σ[21] (true)"
    )
  )

ggplot(plot_data, aes(x = parameter_expr, y = median)) +
  # Posterior median with legend
  geom_point(aes(shape = "Posterior median", color = "Posterior median"), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color = "black") +
  
  # True values
  geom_point(
    aes(y = true_value, shape = true_param, color = true_param),
    size = 2
  ) +
  facet_wrap(~ scenario_expr, ncol = 4, labeller = label_parsed) +

  # Color legend
  scale_color_manual(
    name = NULL,
    values = c("Posterior median" = "black",
               "σ[12] (true)" = "#D7191C",
               "σ[21] (true)" = "#2C7BB6"),
    labels = c("Posterior median (95% CrI)",
               expression(sigma[12]~"(true)"),
               expression(sigma[21]~"(true)"))
  ) +
  
  # Shape legend
  scale_shape_manual(
    name = NULL,
    values = c("Posterior median" = 16,
               "σ[12] (true)" = 17,
               "σ[21] (true)" = 17),
    labels = c("Posterior median (95% CrI)",
               expression(sigma[12]~"(true)"),
               expression(sigma[21]~"(true)"))
  ) +

  labs(
    title = expression("Posterior medians and 95% CrI of " * sigma[12] * " and " * sigma[21]),
    y = "Posterior estimate", x = NULL
  ) +
  scale_x_discrete(
    labels = c(expression(sigma[12]), expression(sigma[21]))
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.3),
    axis.line = element_line(color = "black"),
    legend.position = c(0.99, 0.10),
    legend.justification = c("right", "bottom"),
    legend.direction = "vertical",
    legend.box = "vertical",
    legend.key = element_blank(),
    legend.spacing.y = unit(0.6, "lines"),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    strip.text = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )



