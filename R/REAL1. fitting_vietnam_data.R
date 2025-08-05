# -----------------------------------------
# REAL-WORLD DATA FITTING: VIETNAM STUDY 2013-2017 ----
# -----------------------------------------

library(readxl)
library(rstan)
library(table1)
library(tidyverse)
library(ggplot2)
library(purrr)
library(forcats)
library (loo)

# DATA PREPARATION ----
data_vietnam <- read_excel("data/cleaned_data_dengue_vietnam.xlsx")

data_vietnam_preparation <- function (data) {
  data %>%
    select("Age", "DV1", "DV2", "DV3", "DV4") %>% 
    rename("age" = "Age") %>% 
    mutate(
      denv1_status = case_when(
        DV1 > 5 ~ 1,
        TRUE ~ 0
      ),
      denv2_status = case_when(
        DV2 > 5 ~ 1,
        TRUE ~ 0
      ),
      denv3_status = case_when(
        DV3 > 5 ~ 1,
        TRUE ~ 0
      ),
      denv4_status = case_when(
        DV4 > 5 ~ 1,
        TRUE ~ 0
      ),
      age = case_when(
        age > 0 & age <= 5 ~ 2.5,
        age > 5  & age <= 10 ~ 7.5,
        age > 10 & age <= 15 ~ 12.5,
        age > 15 & age <= 20 ~ 17.5,
        age > 20 & age <= 25 ~ 22.5,
        age > 25 & age <= 30 ~ 27.5,
        age > 30 & age <= 35 ~ 32.5,
        age > 35 & age <= 40 ~ 37.5,
        age > 40 & age <= 45 ~ 42.5,
        age > 45 & age <= 50 ~ 47.5,
        age > 50 & age <= 55 ~ 52.5,
        age > 55 & age <= 60 ~ 57.5,
        age > 60 ~ 62.5,
        #age > 0 & age <= 5 ~ "0-5",
        #age > 5  & age <= 10 ~ "5-10",
        #age > 10 & age <= 15 ~ "10-15",
        #age > 15 & age <= 20 ~ "15-20",
        #age > 20 & age <= 25 ~ "20-25",
        #age > 25 & age <= 30 ~ "25-30",
        #age > 30 & age <= 35 ~ "30-35",
        #age > 35 & age <= 40 ~ "35-40",
        #age > 40 & age <= 45 ~ "40-45",
        #age > 45 & age <= 50 ~ "45-50",
        #age > 50 & age <= 55 ~ "50-55",
        #age > 55 & age <= 60 ~ "55-60",
        #age > 60 ~ "> 60",
        TRUE ~ NA_real_
        #TRUE ~ NA_character_
      ),
      only_denv1 = as.numeric(denv1_status == 1 & denv2_status == 0),
      only_denv2 = as.numeric(denv1_status == 0 & denv2_status == 1),
      both_denv12 = as.numeric(denv1_status == 1 & denv2_status == 1)
    ) %>%
    filter(denv3_status %in% c(0, "0", "NA"), ### Filter for data seronegative to DENV3&4
           denv4_status %in% c(0, "0", "NA")) %>%
    group_by(age) %>%
    summarise(
      n_tested = n(),
      n_denv1 = sum(only_denv1, na.rm = TRUE),
      n_denv2 = sum(only_denv2, na.rm = TRUE),
      n_denv12 = sum(both_denv12, na.rm = TRUE),
      n_s = n_tested - n_denv1 - n_denv2 - n_denv12,
      .groups = "drop"
    )
}

data_vietnam_total <- data_vietnam_preparation(data_vietnam)
data_vietnam_HCM <- data_vietnam %>%
  filter(Site == "HC") %>% data_vietnam_preparation()
data_vietnam_KH <- data_vietnam %>%
  filter(Site == "KH") %>% data_vietnam_preparation()
## Data visualisation ----

data_visualisation_preparation <- function(data){
  data %>%
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
      upper_q_x2 = pmin(1, q_x2 + 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested))
    )
}

data_vietnam_total <- data_visualisation_preparation(data_vietnam_total)
data_vietnam_HCM <- data_visualisation_preparation(data_vietnam_HCM)
data_vietnam_KH <- data_visualisation_preparation(data_vietnam_KH)

# Combined data visualisation

vietnam_total_plot <- data_vietnam_total %>% mutate(site = "Total")
vietnam_HCM_plot <- data_vietnam_HCM %>% mutate(site = "Ho Chi Minh City")
vietnam_KH_plot <- data_vietnam_KH %>% mutate(site = "Khanh Hoa")

# Combine all into one long dataframe
vietnam_combined <- bind_rows(vietnam_total_plot, vietnam_HCM_plot, vietnam_KH_plot) %>%
  select(age, site,
         q_x1, lower_q_x1, upper_q_x1,
         q_x2, lower_q_x2, upper_q_x2) %>%
  pivot_longer(
    cols = c(q_x1, q_x2, lower_q_x1, lower_q_x2, upper_q_x1, upper_q_x2),
    names_to = c(".value", "serotype"),
    names_pattern = "(q_x|lower_q_x|upper_q_x)(1|2)"
  ) %>%
  mutate(
    serotype = recode(serotype, "1" = "DENV1", "2" = "DENV2"),
    #age = factor(age, levels = sort(unique(age)))
    age = factor(age, levels = c(
      "0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45",
      "45-50","50-55","55-60","> 60"
    ), ordered = TRUE)
  )

ggplot(vietnam_combined, aes(x = age, y = q_x, color = serotype, group = serotype)) +
  geom_point(position = position_dodge(width = 0.6), size = 2) +
  geom_errorbar(aes(ymin = lower_q_x, ymax = upper_q_x),
                position = position_dodge(width = 0.6), width = 0.6) +
  scale_colour_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  facet_wrap(~ site, nrow = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    strip.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 30, vjust = 0.8, hjust = 0.8),
    axis.text.y = element_text(size = 12)
  ) +
  labs(
    x = "Age", y = "Seroprevalence",
    colour = "Serotype"
  )

## Check Independence of 2 serotypes ----

fisher_test_serotype_independence <- function(data){
  vietnam_list_with_p <- data %>%
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
  vietnam_all_pval <- bind_rows(vietnam_list_with_p)
  ggplot(vietnam_all_pval, aes(x = factor(age))) +
    geom_line(aes(y = p_x12, colour = "Observed", group = 1)) +
    geom_point(aes(y = p_x12, colour = "Observed", shape = p_value < 0.05), size = 2.5, na.rm = TRUE) +
    
    geom_line(aes(y = q_x12, colour = "Expected", group = 1)) +
    geom_point(aes(y = q_x12, colour = "Expected"), size = 2, na.rm = TRUE) +
    
    scale_shape_manual(
      values = c(`TRUE` = 17, `FALSE` = 16),
      name = "p < 0.05",
      labels = c("TRUE", "FALSE"),
      drop = FALSE
    ) +
    scale_colour_manual(
      values = c("Observed" = "steelblue", "Expected" = "tomato"),
      name = "Seroprevalence of DENV1 + DENV2"
    ) +
    labs(
      x = "Age midpoint",
      y = "Probability"
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      strip.text = element_text(size = 12)
    )
}

fisher_test_serotype_independence(data_vietnam_total)
fisher_test_serotype_independence(data_vietnam_HCM)
fisher_test_serotype_independence(data_vietnam_KH)


### FITTING CONSTANT FOI WITH INTERACTION ----
fit_constant_foi_VN <- function(data){
  age_fine <- seq(0, max(data$age), by=1)
  n_age_fine <- length(age_fine)
  stan_data <- list(
    N = nrow(data),
    age = data$age,
    n_tested = data$n_tested,
    n_s = data$n_s,
    n_denv1 = data$n_denv1,
    n_denv2 = data$n_denv2,
    n_denv12 = data$n_denv12,
    y = as.matrix(data[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = n_age_fine
  )
  stan_model <- stan_model("stan/simulation_denv_2_serotypes.stan")
  fit <- sampling(stan_model, data = stan_data, iter = 20000, chains = 4)
}

fit_Vietnam_total <- fit_constant_foi_VN(data_vietnam_total)
print(fit_Vietnam_total, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 3)

fit_Vietnam_HCM <- fit_constant_foi_VN(data_vietnam_HCM)
print(fit_Vietnam_HCM, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 3)

fit_Vietnam_KH <- fit_constant_foi_VN(data_vietnam_KH)
print(fit_Vietnam_KH, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 3)

# Wrap the Vietnam fits into a list
fits_vietnam_inter <- list(
  Total = fit_Vietnam_total,
  HCMC = fit_Vietnam_HCM,
  KhanhHoa = fit_Vietnam_KH
)

# Extract summary statistics for each location
extract_vietnam_summary <- function(fit, location) {
  summ <- summary(fit, pars = c("lambda1", "lambda2", "sigma12", "sigma21"),
                  probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    location = location,
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}

# Combine all into one dataframe
summary_vietnam_inter <- imap_dfr(fits_vietnam_inter, extract_vietnam_summary)

# Assuming `summary_vietnam` is your data frame
summary_vietnam_inter <- summary_vietnam_inter %>%
  mutate(
    label = recode(parameter,
                   "lambda1" = "lambda[1]",
                   "lambda2" = "lambda[2]",
                   "sigma12" = "sigma[12]",
                   "sigma21" = "sigma[21]"),
    group = case_when(
      parameter %in% c("lambda1", "lambda2") ~ "lambda",
      parameter %in% c("sigma12", "sigma21") ~ "sigma"
    )
  )

# Define dodge position for consistent spacing
position_dodge_val <- position_dodge(width = 0.5)

# Create the plot

ggplot(summary_vietnam_inter, aes(x = location, y = mean, color = label)) +
  #geom_line(aes(group = label), linetype = "dotted", position = position_dodge_val) +
  geom_point(position = position_dodge_val, size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.4, position = position_dodge_val) +
  geom_text(aes(label = round(mean, 4)), 
            position = position_dodge_val, 
            vjust = 1.2,
            hjust = -0.1,
            size = 3.5, 
            show.legend = FALSE) +
  geom_hline(data = filter(summary_vietnam, group == "sigma"),
             aes(yintercept = 1), linetype = "dashed", color = "red", inherit.aes = FALSE) +
  scale_color_manual(
    name = "Parameter",
    values = c("lambda[1]" = "#1b9e77", "lambda[2]" = "#d95f02",
               "sigma[12]" = "steelblue", "sigma[21]" = "#e7298a"),
    labels = c(
      expression(lambda[1]),
      expression(lambda[2]),
      expression(sigma[12]),
      expression(sigma[21])
    )
  ) +
  facet_wrap(~ group, scales = "free_y", labeller = label_parsed) +
  labs(
    title = "Posterior Mean and 95% CrI by Location",
    x = "Location", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 16),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14)
  )

## Posterior Predictive Check
make_ppc_plot_VN <- function(data_name, data, fit) {
  age_fine <- seq(0, max(data$age), by=1) #age start from 0 instead of 5 (for Vietnam data)
  n_age_fine <- length(age_fine)
  stan_data <- list(
    N = nrow(data),
    age = data$age,
    n_tested = data$n_tested,
    n_s = data$n_s,
    n_denv1 = data$n_denv1,
    n_denv2 = data$n_denv2,
    n_denv12 = data$n_denv12,
    y = as.matrix(data[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = n_age_fine
  )
  posterior <- rstan::extract(fit)
  y <- stan_data$y
  n_tested <- stan_data$n_tested
  age <- stan_data$age
  age_fine <- stan_data$age_fine
  
  # Predicted probabilities
  pred_probs <- array(NA, dim = dim(posterior$y_rep))
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
  
  # Posterior predictive data frame
  prob_s <- 1 - pred_probs[, , 2] - pred_probs[, , 3] - pred_probs[, , 4]
  df_s   <- make_pred_df(prob_s, age, "Susceptible", y[, 1], n_tested)
  df_d1  <- make_pred_df(pred_probs[,,2], age, "DENV1 only", y[, 2], n_tested)
  df_d2  <- make_pred_df(pred_probs[,,3], age, "DENV2 only", y[, 3], n_tested)
  df_d12 <- make_pred_df(pred_probs[,,4], age, "DENV1 + DENV2", y[, 4], n_tested)
  
  df_ppc <- bind_rows(df_s, df_d1, df_d2, df_d12) %>%
    mutate(compartment = factor(compartment, levels = c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2")))
  
  get_ci_fine <- function(mat) {
    apply(mat, 2, quantile, probs = c(0.025, 0.5, 0.975))
  }
  
  ci_s    <- get_ci_fine(posterior$prob_s)
  ci_d1   <- get_ci_fine(posterior$prob_x1)
  ci_d2   <- get_ci_fine(posterior$prob_x2)
  ci_d12  <- get_ci_fine(posterior$prob_x12)
  
  df_smooth <- tibble(
    age = rep(age_fine, 4),
    median = c(ci_s[2,], ci_d1[2,], ci_d2[2,], ci_d12[2,]),
    lower = c(ci_s[1,], ci_d1[1,], ci_d2[1,], ci_d12[1,]),
    upper = c(ci_s[3,], ci_d1[3,], ci_d2[3,], ci_d12[3,]),
    compartment = rep(c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2"), each = length(age_fine))
  ) %>%
    mutate(compartment = factor(compartment, levels = c("Susceptible", "DENV1 only", "DENV2 only", "DENV1 + DENV2")))
  
  title_text <- paste0(
    "<span style='font-size:16pt'><b>Posterior Predictive Check of age-stratified infection probabilities: </b></span><br>",
    "<span style='font-size:14pt'>Model estimates versus real-world data of ",
    data_name,
    "</span>"
  )
  
  ggplot(df_ppc, aes(x = age)) +
    geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
    geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
    geom_point(aes(y = observed), color = "black", size = 2) +
    geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
    facet_wrap(~ compartment) +
    labs(
      title = title_text,
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
      axis.text.y = element_text(size = 18)
    )
}


ppc_vietnam <- make_ppc_plot_VN(data_name = "Vietnam - Total population", data=data_vietnam_total, fit=fit_Vietnam_total)
ppc_vietnam
make_ppc_plot_VN(data_name = "Vietnam - Ho Chi Minh City", data=data_vietnam_HCM, fit=fit_Vietnam_HCM)
make_ppc_plot_VN(data_name = "Vietnam - Khanh Hoa", data=data_vietnam_KH, fit=fit_Vietnam_KH)


### FITTING NO INTER ----
stan_two_piece_wise_no_inter_model <- stan_model("stan/two_piece-wise_FOI-no-inter.stan")

fit_vietnam_two_piece_wise_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = data_vietnam_total, iter = 40000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_vietnam_two_piece_wise_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)


fit_constant_foi_VN_no_inter <- function(data){
  age_fine <- seq(0, max(data$age), by=1)
  n_age_fine <- length(age_fine)
  stan_data <- list(
    N = nrow(data),
    age = data$age,
    n_tested = data$n_tested,
    n_s = data$n_s,
    n_denv1 = data$n_denv1,
    n_denv2 = data$n_denv2,
    n_denv12 = data$n_denv12,
    y = as.matrix(data[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = n_age_fine
  )
  stan_model <- stan_model("stan/constant_foi_no_interaction.stan")
  fit <- sampling(stan_model, data = stan_data, iter = 20000, chains = 4)
}

fit_vietnam_constant_no_inter <- fit_constant_foi_VN_no_inter(data_vietnam_total)
print(fit_vietnam_constant_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)

fit_HCM_constant_no_inter <- fit_constant_foi_VN_no_inter(data_vietnam_HCM)
print(fit_HCM_constant_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)

fit_KH_constant_no_inter <- fit_constant_foi_VN_no_inter(data_vietnam_KH)
print(fit_KH_constant_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)


### LAMBDA COMPARISON ACROSS CITIES ----
fits_vietnam_no_inter <- list(
  Total = fit_vietnam_constant_no_inter,
  HCMC = fit_HCM_constant_no_inter,
  KhanhHoa = fit_KH_constant_no_inter
)



extract_vietnam_lambda_summary <- function(fit, location) {
  summ <- summary(fit, pars = c("lambda1", "lambda2"),
                  probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    location = location,
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}

summary_lambda_vietnam_no_inter <- imap_dfr(fits_vietnam_no_inter, extract_vietnam_lambda_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "lambda1" = "lambda[1]",
                                  "lambda2" = "lambda[2]"))


ggplot(summary_lambda_vietnam_no_inter, aes(x = location, y = mean, color = parameter_label)) +
  geom_line(linetype = "dotted") +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), width = 0.3) +
  scale_color_manual(
    name = "Parameter",
    values = c("lambda[1]" = "#1b9e77", "lambda[2]" = "#d95f02"),
    labels = c(expression(lambda[1]), expression(lambda[2]))
  ) +
  labs(
    title = "Posterior Mean and 95% CrI of FOI (No Interaction Model)",
    x = "Year", y = "Force of Infection"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

# Add model type
summary_lambda_vietnam_no_inter <- summary_lambda_vietnam_no_inter %>%
  mutate(model = "No interaction")

summary_lambda_vietnam_inter <- summary_vietnam_inter %>%
  filter(parameter %in% c("lambda1", "lambda2")) %>%
  mutate(
    
    parameter_label = recode(parameter,
                             "lambda1" = "lambda[1]",
                             "lambda2" = "lambda[2]"),
    model = "With interaction"
  ) %>%
  select(location, parameter, parameter_label, mean, q025, median, q975, model)

# Combine both datasets
combined_lambda_vietnam <- bind_rows(summary_lambda_vietnam_no_inter, summary_lambda_vietnam_inter)


# Plot with dodging
shared_color_scale <- scale_color_manual(
  name = "Parameter",
  values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
  labels = c(expression(lambda[1]), expression(lambda[2]))
)

g_lambda_vietnam <- ggplot(combined_lambda_vietnam, aes(x = location, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.3), width = 0.4) +
  #geom_line(aes(group = parameter_label), 
  #          linetype = "dotted", position = position_dodge(width = 0.3)) +
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

g_lambda_vietnam 

### SIGMA COMPARISON ACROSS CITIES ----
extract_sigma_vietnam_summary <- function(fit, location) {
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

summary_sigma_vietnam <- imap_dfr(fits_vietnam_inter, extract_sigma_vietnam_summary) %>% 
  mutate(location = fct_relevel(location, "Total"),
          parameter_label = recode(parameter,
                                  "sigma12" = "sigma[12]",
                                  "sigma21" = "sigma[21]"))

summary_sigma_vietnam <- summary_sigma_vietnam %>%
  mutate(label_x = as.numeric(factor(location)) + ifelse(parameter == "sigma12", -0.2, 0.2),
         label_text = sprintf("%.1f", mean))  # Format to 1 decimal place

g_sigma_vietnam <- ggplot(summary_sigma_vietnam, aes(x = location, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.3), width = 0.3) +  # ⬅ Shrink width
  geom_text(aes(x = label_x, label = label_text), 
            vjust = 0, size = 5, show.legend = FALSE) +  # ⬅ Add labels
  scale_color_manual(
    name = "Parameter",
    values = c("sigma[12]" = "#1b9e77", "sigma[21]" = "#d95f02"),
    labels = c(expression(sigma[12]), expression(sigma[21]))
  ) +
  labs(
    title = expression("Interaction parameter (" * sigma * ")"),
    x = "Location", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 15, vjust = 0.8, hjust = 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14)
  )

g_sigma_vietnam

### MODEL COMPARISON - LOO
loo(extract_log_lik(fit_vietnam_constant_no_inter))
loo(extract_log_lik(fit_Vietnam_total))
loo_compare(loo(extract_log_lik(fit_vietnam_constant_no_inter)), loo(extract_log_lik(fit_Vietnam_total)))

loo(extract_log_lik(fit_HCM_constant_no_inter))
loo(extract_log_lik(fit_Vietnam_HCM))
loo_compare(loo(extract_log_lik(fit_HCM_constant_no_inter)), loo(extract_log_lik(fit_Vietnam_HCM)))
  
loo(extract_log_lik(fit_KH_constant_no_inter))
loo(extract_log_lik(fit_Vietnam_KH))
loo_compare(loo(extract_log_lik(fit_KH_constant_no_inter)), loo(extract_log_lik(fit_Vietnam_KH)))
