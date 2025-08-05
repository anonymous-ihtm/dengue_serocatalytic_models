# -----------------------------------------
# REAL-WORLD DATA FITTING: CUBA 1997 ----
# -----------------------------------------

library(readxl)
library(rstan)
library(table1)
library(tidyverse)
library(ggplot2)
library(purrr)
library(ggtext)
library(loo)
library(patchwork)
#library(writexl)

options(mc.cores=4)

data_cuba <- read_excel("data/cuba_1997.xlsx")

cuba_1997 <- data_cuba %>% 
  mutate(
    #age = case_when(
    #Age_group == "< 1" ~ 0.5,
    #Age_group == "1-4" ~ 2.5,
    #Age_group == "5-9" ~ 7.5,
    #Age_group == "10-14" ~ 12.5,
    #Age_group == "15-19" ~ 17.5,
    #Age_group == "20-24" ~ 22.5,
    #Age_group == "25-34" ~ 29.5,
    #Age_group == "35-44" ~ 39.5,
    #Age_group == "45-54" ~ 49.5,
    #Age_group == "55-64" ~ 59.5,
    #Age_group == "65-74" ~ 69.5,
    #Age_group == "75-84" ~ 79.5,
    #Age_group == "85-94" ~ 89.5,
    #TRUE ~ NA_real_
    age = factor(Age_group, levels = c(
      "< 1", "1-4", "5-9", "10-14", "15-19",
      "20-24", "25-34", "35-44", "45-54",
      "55-64", "65-74", "75-84", "85-94"
    ), ordered = TRUE)
    ) %>% group_by(age)


## Data visualisation

data_cuba <- cuba_1997 %>%
  mutate(
    p_s = n_s/n_tested,
    p_x1 =  n_denv1/n_tested,
    p_x2 =  n_denv2/n_tested,
    p_x12 =  n_denv12/n_tested,
    q_x1 = (n_denv1 + n_denv12) / n_tested,
    q_x2 = (n_denv2 + n_denv12) / n_tested,
    q_x12 = q_x1 * q_x2,
    p_1_2 = p_x12 / q_x2,
    p_2_1 = p_x12 / q_x1,
    lower_q_x1 = pmax(0, q_x1 - 1.96 * sqrt(q_x1 * (1 - q_x1) / n_tested)),
    upper_q_x1 = pmin(1, q_x1 + 1.96 * sqrt(q_x1 * (1 - q_x1) / n_tested)),
    lower_q_x2 = pmax(0, q_x2 - 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested)),
    upper_q_x2 = pmin(1, q_x2 + 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested)),
  )

cuba_long <- data_cuba %>%
  pivot_longer(cols = c(p_s, p_x1, p_x2, p_x12, q_x1, q_x2, q_x12, p_1_2, p_2_1, lower_q_x1, upper_q_x1, lower_q_x2, upper_q_x2),
               names_to = "prob_type",
               values_to = "probability") %>% 
  filter(prob_type %in% c("q_x1", "q_x2","lower_q_x1", "upper_q_x1", "lower_q_x2", "upper_q_x2"))

cuba_long <- data_cuba %>%
  select(age, n_tested,
         q_x1, lower_q_x1, upper_q_x1,
         q_x2, lower_q_x2, upper_q_x2) %>%
  pivot_longer(
    cols = c(q_x1, q_x2, lower_q_x1, lower_q_x2, upper_q_x1, upper_q_x2),
    names_to = c(".value", "serotype"),
    names_pattern = "(q_x|lower_q_x|upper_q_x)(1|2)"
  ) %>%
  mutate(
    serotype = recode(serotype, "1" = "DENV1", "2" = "DENV2")
  )

ggplot(cuba_long, aes(x = age, y = q_x, color = serotype, group = serotype)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower_q_x, ymax = upper_q_x),
                position = position_dodge(width = 0.6), width = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14, angle = 30, vjust = 0.8, hjust = 0.8),
    axis.text.y = element_text(size = 14)
  ) +
  labs(
    x = "Age", y = "Seroprevalence",
    colour = "Serotype"
  )

## Check Indepence of 2 serotypes
cuba_list_with_p <- data_cuba %>%
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

cuba_all_pval <- bind_rows(cuba_list_with_p)

ggplot(cuba_all_pval, aes(x = factor(age))) +
  # Line to connect observed points
  geom_line(aes(y = p_x12, colour = "Observed", group = 1)) +
  # Observed points with shape by significance
  geom_point(aes(y = p_x12, colour = "Observed", shape = p_value < 0.05), size = 2.5) +
  # Expected points
  geom_line(aes(y = q_x12, colour = "Expected", group = 1)) +
  geom_point(aes(y = q_x12, colour = "Expected"), size = 2) +
  # Shape and colour adjustments
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), name = "p < 0.05") +
  scale_colour_manual(values = c("Observed" = "steelblue", "Expected" = "tomato")) +
  theme_bw() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12)) +
  labs(
    x = "Age midpoint", y = "Probability",
    colour = "Seroprevalence of DENV1 + DENV2"
  )



## FITTING: CONSTANT FOI MODEL WITH INTERACTION ----
age_fine <- seq(0, max(cuba_1997$age), by=1)
n_age_fine <- length(age_fine)
stan_cuba <- list(
  N = nrow(cuba_1997),
  age = cuba_1997$age,
  n_tested = cuba_1997$n_tested,
  n_s = cuba_1997$n_s,
  n_denv1 = cuba_1997$n_denv1,
  n_denv2 = cuba_1997$n_denv2,
  n_denv12 = cuba_1997$n_denv12,
  y = as.matrix(cuba_1997[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = n_age_fine
)

stan_constant_model <- stan_model("stan/simulation_denv_2_serotypes.stan")
fit_cuba <- sampling(stan_constant_model, data = stan_cuba, iter = 4000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_cuba, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), digits = 4)

# FITTING CONSTANT FOI, NO-INTERACTION MODEL ----
fit_cuba_no_inter <- stan(
  file = "stan/constant_foi_no_interaction.stan",
  data = stan_cuba,
  chains = 4,
  iter = 2000,
  seed = 1234,
  control = list(adapt_delta = 0.95)
)

print(fit_cuba_no_inter, pars = c("lambda1", "lambda2"), digits = 4)

### POSTERIOR PREDICTIVE CHECK ----
#posterior <- rstan::extract(fit_cuba_no_inter)
posterior <- rstan::extract(fit_cuba)

y <- stan_cuba$y                 # observed counts: 13 x 4
n_tested <- stan_cuba$n_tested  
age <- stan_cuba$age            

pred_probs <- array(NA, dim = dim(posterior$y_rep))  # 4000 x 13 x 4

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

age_fine <- stan_cuba$age_fine

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

ppc_cuba <- ggplot(df_ppc, aes(x = age)) +
  geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
  geom_point(aes(y = observed), color = "black", size = 2) +
  geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
  
  facet_wrap(~ compartment) +
  labs(
    title = "<span style='font-size:16pt'><b>Posterior Predictive Check of age-stratified infection probabilities: Model estimates versus Cuba data </b></span><br>
            <span style='font-size:12pt'>  </span><br>",
    x = "Age", y = "Probability"
  ) +
  theme_bw()+
  theme(
    plot.title = ggtext::element_markdown(),
    plot.title.position = "plot",
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )
ppc_cuba
####### SUMMARY OF PARAMETER ESTIMATES ----
# Prepare the data
summary_cuba_grouped <- summary_cuba %>%
  filter(!str_detect(parameter, "^log_lik")) %>%
  mutate(
    label = recode(parameter,
                   "lambda1" = "lambda[1]",
                   "lambda2" = "lambda[2]",
                   "sigma12" = "sigma[12]",
                   "sigma21" = "sigma[21]"
    ),
    group = case_when(
      parameter %in% c("lambda1", "lambda2") ~ "lambda",
      parameter %in% c("sigma12", "sigma21") ~ "sigma"
    )
  ) %>%
  filter(!is.na(group))  # Keep only lambda and sigma

# λ plot
gg_lambda <- ggplot(filter(summary_cuba_grouped, group == "lambda"),
                    aes(x = label, y = mean, ymin = q025, ymax = q975)) +
  geom_point(size = 3, color = "#1b9e77") +
  geom_errorbar(width = 0.3, color = "#1b9e77") +
  scale_x_discrete(labels = c(expression(lambda[1]), expression(lambda[2]))) +
  labs(
    title = "Posterior Mean and 95% CI for λ",
    x = expression(lambda),
    y = "Estimate"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold")
  )

# σ plot
gg_sigma <- ggplot(filter(summary_cuba_grouped, group == "sigma"),
                   aes(x = label, y = mean, ymin = q025, ymax = q975)) +
  geom_point(size = 3, color = "#377eb8") +
  geom_errorbar(width = 0.3, color = "#377eb8") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  scale_x_discrete(labels = c(expression(sigma[12]), expression(sigma[21]))) +
  labs(
    title = "Posterior Mean and 95% CI for σ",
    x = expression(sigma),
    y = "Estimate"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    plot.title = element_text(size = 20, face = "bold")
  )

gg_lambda + gg_sigma + labs(
  title = "Posterior Mean and 95% CI"
)

### LAMBDA COMPARISON WITH INTERACTIONS AND NO INTERACTIONS IN CUBA
fits_cuba_no_inter <- list(
  Cuba = fit_cuba_no_inter
)

fits_cuba_inter <- list(
  Cuba = fit_cuba
)


extract_cuba_lambda_summary <- function(fit, name) {
  summ <- summary(fit, pars = c("lambda1", "lambda2"),
                  probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    country = name,
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}


summary_lambda_cuba_no_inter <- imap_dfr(fits_cuba_no_inter, extract_cuba_lambda_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "lambda1" = "lambda[1]",
                                  "lambda2" = "lambda[2]"))


summary_lambda_cuba_inter <- imap_dfr(fits_cuba_inter, extract_cuba_lambda_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "lambda1" = "lambda[1]",
                                  "lambda2" = "lambda[2]"))

# Add model type
summary_lambda_cuba_no_inter <- summary_lambda_cuba_no_inter %>%
  mutate(model = "No interaction")

summary_lambda_cuba_inter <- summary_lambda_cuba_inter %>%
  mutate(model = "With interaction")


# Combine both datasets
combined_lambda_cuba <- bind_rows(summary_lambda_cuba_no_inter, summary_lambda_cuba_inter)


# Plot with dodging
shared_color_scale <- scale_color_manual(
  name = "Parameter",
  values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
  labels = c(expression(lambda[1]), expression(lambda[2]))
)

g_lambda_cuba <- ggplot(combined_lambda_cuba, aes(x = country, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.3), width = 0.4) +
  geom_line(aes(group = parameter_label), 
            linetype = "dotted", position = position_dodge(width = 0.3)) +
  facet_wrap(~ model) +
  scale_color_manual(
    name = "Force of infection",
    values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
    labels = c(expression(lambda[1]), expression(lambda[2]))
  ) +
  #scale_x_continuous(breaks = unique(combined_lambda_peru$year)) +
  labs(
    title = "Force of Infection (λ)",
    x = "Location", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(angle = 30, vjust = 0.8, hjust = 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  )

g_lambda_cuba

### SIGMA COMPARISON IN CUBA
extract_sigma_cuba_summary <- function(fit, location) {
  summ <- summary(fit, pars = c("sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    location = location,
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}

summary_sigma_cuba <- imap_dfr(fits_cuba_inter, extract_sigma_cuba_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "sigma12" = "sigma[12]",
                                  "sigma21" = "sigma[21]")) %>% 
  mutate(label_x = as.numeric(factor(location)) + ifelse(parameter == "sigma12", -0.2, 0.2),
         label_text = sprintf("%.1f", mean))  # Format to 1 decimal place

g_sigma_cuba <- ggplot(summary_sigma_cuba , aes(x = location, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.6), width = 0.1) +
  #geom_line(aes(group = parameter_label), 
  #          linetype = "dotted", position = position_dodge(width = 0.3)) +
  geom_text(aes(x = label_x, label = label_text), 
            vjust = 0, size = 5, show.legend = FALSE) +  # ⬅ Add labels
  scale_color_manual(
    name = "Parameter",
    values = c("sigma[12]" = "#1b9e77", "sigma[21]" = "#d95f02"),
    labels = c(expression(sigma[12]), expression(sigma[21]))
  ) +
  labs(
    title = expression("Interaction parameter (" * sigma * ")"),
    x = "Country", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 15, vjust = 0.8, hjust = 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

g_sigma_cuba

####### Check best cut-off year for Cuba

current_year <- 1997
cutoff_years <- seq(1997, 1937, by = -5)


loo_list <- list()

# --- constant FOI models ---
log_lik_1 <- extract_log_lik(fit_cuba, parameter_name = "log_lik")
log_lik_3 <- extract_log_lik(fit_cuba_no_inter, parameter_name = "log_lik")

loo_list[["Constant_FOI_with_interaction"]] <- loo(log_lik_1)
loo_list[["Constant_FOI_no_interaction"]] <- loo(log_lik_3)

# --- time-varying FOI model ---
for (cutoff_year in cutoff_years) {
  message("Running for cutoff year: ", cutoff_year)
  
  cutoff_age <- current_year - cutoff_year
  stan_data <- list(
    N = nrow(cuba_1997),
    age = cuba_1997$age,  # <--- use correct name
    n_tested = cuba_1997$n_tested,
    y = as.matrix(cuba_1997[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = length(age_fine),
    cutoff_age = cutoff_age
  )
  
  # Try-catch block to handle Stan errors
  fit <- tryCatch({
    sampling(
      stan_two_piece_wise_model,
      data = stan_data,
      iter = 4000, chains = 4,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      refresh = 0
    )
  }, error = function(e) {
    message("Sampling failed for cutoff year ", cutoff_year, ": ", e$message)
    return(NULL)
  })
  
  # Skip if model failed
  if (is.null(fit)) next
  
  # LOO evaluation
  log_lik_matrix <- extract_log_lik(fit, parameter_name = "log_lik")
  loo_result <- loo(log_lik_matrix)
  
  model_name <- paste0("TimeVarying_Cutoff_", cutoff_year)
  loo_list[[model_name]] <- loo_result
}



elpd_values_cuba <- sapply(loo_list, function(x) x$estimates["elpd_loo", "Estimate"])
se_values_cuba   <- sapply(loo_list, function(x) x$estimates["elpd_loo", "SE"])

ref_model <- "Constant_FOI_with_interaction"

elpd_diff_cuba <- elpd_values_cuba - elpd_values_cuba[ref_model]
se_diff_cuba  <- sqrt(se_values_cuba^2 + se_values_cuba[ref_model]^2)

elpd_table_cuba <- tibble(
  Model = names(loo_list),
  elpd_loo = elpd_values_cuba,
  se_elpd_loo = se_values_cuba,
  elpd_diff = elpd_diff_cuba,
  se_diff = se_diff_cuba,
  is_reference = names(loo_list) == ref_model,
  is_no_interaction = names(loo_list) == "Constant_FOI_no_interaction",
  is_significantly_better = elpd_diff> se_diff,
  is_best = elpd_diff == max(elpd_diff),
  is_worst = elpd_diff == min(elpd_diff)
)

elpd_table_cuba <- elpd_table_cuba %>%
  mutate(
    cutoff_year = as.integer(str_extract(Model, "\\d{4}")),
    shape_type = case_when(
      is_reference ~ "Reference",
      is_best ~ "Best",
      is_worst ~ "Worst",
      TRUE ~ "Other"
    )
  )

ordered_models_cuba <- c(
  "Constant_FOI_with_interaction",
  "Constant_FOI_no_interaction",
  elpd_table_cuba %>%
    filter(str_detect(Model, "TimeVarying_Cutoff_")) %>%
    arrange(desc(cutoff_year)) %>%
    pull(Model)
)

elpd_table_cuba <- elpd_table_cuba %>%
  mutate(Model = factor(Model, levels = rev(ordered_models)))

shape_values <- c("Reference" = 17, "Best" = 16, "Worst" = 16, "Other" = 16)  # 17 = triangle, 16 = circle
color_values <- c("Reference" = "black", "Best" = "blue", "Worst" = "red" ,"Other" = "black")

ggplot(elpd_table_cuba, aes(x = elpd_diff, y = Model)) +
  geom_point(aes(shape = shape_type, color = shape_type), size = 3) +
  geom_errorbarh(aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  
  scale_shape_manual(values = shape_values, guide = "none") +
  scale_color_manual(values = color_values, guide = "none") +
  
  labs(
    title = "ELPD Differences Compared to Constant FOI with Interaction",
    x = "ELPD Difference (vs Reference)",
    y = "Model"
  ) +
  xlim(min(elpd_table_cuba$elpd_diff - elpd_table_cuba$se_diff) - 5,
       max(elpd_table_cuba$elpd_diff + elpd_table_cuba$se_diff) + 25) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

######
## FIT NEW MODEL
current_year <- 1997
cutoff_year <- 1979

stan_cuba_new <- list(
  N = nrow(cuba_1997),
  age = cuba_1997$age,
  n_tested = cuba_1997$n_tested,
  n_s = cuba_1997$n_s,
  n_denv1 = cuba_1997$n_denv1,
  n_denv2 = cuba_1997$n_denv2,
  n_denv12 = cuba_1997$n_denv12,
  y = as.matrix(cuba_1997[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = n_age_fine,
  cutoff_age = current_year - cutoff_year
)

stan_two_piece_wise_model <- stan_model("stan/two_piece-wise_FOI.stan")
fit_cuba_new <- sampling(stan_two_piece_wise_model, data = stan_cuba_new, iter = 4000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_cuba_new, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)

### POSTERIOR PREDICTIVE CHECK: NEW MODEL

posterior <- rstan::extract(fit_cuba_new)

y <- stan_cuba_new$y                 # observed counts: 13 x 4
n_tested <- stan_cuba_new$n_tested  
age <- stan_cuba_new$age            

pred_probs <- array(NA, dim = dim(posterior$y_rep))  # 4000 x 13 x 4

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

age_fine <- stan_cuba_new$age_fine

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

ggplot(df_ppc, aes(x = age)) +
  # smooth line from age_fine
  geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
  geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
  
  # observed
  geom_point(aes(y = observed), color = "black", size = 2) +
  geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
  
  facet_wrap(~ compartment) +
  labs(
    title = "<span style='font-size:16pt'><b>Posterior Predictive Check of age-stratified infection probabilities: Model estimates versus Cuba data</b></span><br>
            <span style='font-size:12pt'>  </span><br>",
    x = "Age", y = "Probability"
  ) +
  theme_bw()+
  theme(
    plot.title = ggtext::element_markdown(),
    plot.title.position = "plot",
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


### COMPARE LOO
log_lik_matrix_1 <- extract_log_lik(fit_cuba, parameter_name = "log_lik")
log_lik_matrix_2 <- extract_log_lik(fit_cuba_new, parameter_name = "log_lik")
log_lik_matrix_3 <- extract_log_lik(fit_cuba_no_inter, parameter_name = "log_lik")

loo1 <- loo(log_lik_matrix_1)
loo1
loo2 <- loo(log_lik_matrix_2)
loo2
loo3 <- loo(log_lik_matrix_3)
loo3
loo_compare(loo1, loo2, loo3)

# Extract the summary matrix
summary_matrix <- summary(fit_cuba)$summary

# Convert to data frame and rename correctly
summary_cuba <- summary_matrix %>%
  as.data.frame() %>%
  rownames_to_column("parameter") %>%
  select(parameter, mean = mean, q025 = `2.5%`, q975 = `97.5%`)

# Filter and assign groups
summary_cuba_grouped <- summary_cuba %>%
  filter(!str_detect(parameter, "^log_lik")) %>%
  mutate(
    group = case_when(
      str_detect(parameter, "^lambda") ~ "lambda",
      str_detect(parameter, "^sigma") ~ "sigma",
      TRUE ~ "other"
    ),
    label = recode(parameter,
                   "lambda1" = "lambda[1]",
                   "lambda2" = "lambda[2]",
                   "sigma12" = "sigma[12]",
                   "sigma21" = "sigma[21]"
    )) %>%
  filter(group %in% c("lambda", "sigma"))  # exclude any unrelated ones

ggplot(summary_cuba_grouped, aes(x = parameter, y = mean, ymin = q025, ymax = q975)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(width = 0.3, color = "steelblue") +
  geom_hline(
    data = data.frame(group = "sigma", yintercept = 1),
    aes(yintercept = yintercept),
    linetype = "dashed",
    color = "red"
  ) +
  facet_wrap(~group, scales = "free_y", nrow = 1, labeller = label_parsed) + 
  labs(
    title = "Posterior Mean and 95% CI",
    x = "Parameter",
    y = "Estimate"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    strip.text = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 18)
  )

### MODEL COMPARISON - LOO
loo(extract_log_lik(fit_cuba_no_inter))
loo(extract_log_lik(fit_cuba))
loo_compare(loo(extract_log_lik(fit_cuba_no_inter)), loo(extract_log_lik(fit_cuba)))
