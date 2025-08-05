# -----------------------------------------
# REAL-WORLD DATA FITTING: LAOS 2009 ----
# -----------------------------------------

library(readxl)
library(rstan)
library(table1)
library(dplyr)
library(ggplot2)
library(purrr)
library(loo)

options(mc.cores=4)

data_lao <- read_excel("data/Data_Lao.xlsx")

### DATA PREPARATION ----

data_lao <- data_lao %>% 
  filter(JEV %in% c("<10", "10")) %>% 
  select("Province", "Age (years)", "DENV1", "DENV2", "DENV3", "DENV4") %>% 
  rename("age" = "Age (years)") %>% 
  mutate(
    denv1_status = case_when(
    DENV1 %in% c("<10", "10") ~ 0,
    TRUE ~ 1
    ),
    denv2_status = case_when(
      DENV2 %in% c("<10", "10") ~ 0,
      TRUE ~ 1
    ),
    denv3_status = case_when(
      DENV3 %in% c("<10", "10") ~ 0,
      TRUE ~ 1
    ),
    denv4_status = case_when(
      DENV4 %in% c("<10", "10") ~ 0,
      TRUE ~ 1
    ),
    age = case_when(
      age >= 5  & age <= 10 ~ 7.5,
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
      TRUE ~ NA_real_
    ),
      only_denv1 = as.numeric(denv1_status == 1 & denv2_status == 0),
      only_denv2 = as.numeric(denv1_status == 0 & denv2_status == 1),
      both_denv12 = as.numeric(denv1_status == 1 & denv2_status == 1)
    ) %>%
  filter(denv3_status %in% c(0, "0", "NA"),
         denv4_status %in% c(0, "0", "NA")) 

create_data <- function(data){
  data %>% 
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

data_lao_total <- data_lao %>% create_data()
data_lao_Houaphan <- data_lao %>% filter(Province == "Houaphan") %>% create_data()
data_lao_Luangprabang <- data_lao %>% filter(Province == "Luangprabang") %>% create_data()
data_lao_Oudomxay <- data_lao %>% filter(Province == "Oudomxay") %>% create_data()
data_lao_Xiengkhouang <- data_lao %>% filter(Province == "Xiengkhouang") %>% create_data()

## Data visualisation

data_lao <- data_lao %>%
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
    lower_q_x1 = q_x1 - 1.96 * sqrt(q_x1 * (1 - q_x1) / n_tested),
    upper_q_x1 = q_x1 + 1.96 * sqrt(q_x1 * (1 - q_x1) / n_tested),
    lower_q_x2 = q_x2 - 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested),
    upper_q_x2 = q_x2 + 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested),
  )
ggplot(data=data_lao) + geom_line(aes(x = age, y = p_s, colour = "p_s")) + geom_line(aes(x = age, y = p_x1, colour = "p_x1")) + geom_line(aes(x= age, y = p_x2, colour = "p_x2")) + geom_line(aes(x = age, y = p_x12, colour = "p_x12"))+ theme_minimal()


lao_long <- data_lao %>%
  pivot_longer(cols = c(p_s, p_x1, p_x2, p_x12, q_x1, q_x2, q_x12, p_1_2, p_2_1, lower_q_x1, upper_q_x1, lower_q_x2, upper_q_x2),
               names_to = "prob_type",
               values_to = "probability") %>% 
  #filter(prob_type %in% c("p_x12", "q_x1", "q_x2", "q_x12")) #%>% 
  filter(prob_type %in% c("q_x1", "q_x2","lower_q_x1", "upper_q_x1", "lower_q_x2", "upper_q_x2"))
#filter(prob_type %in% c("p_s", "p_x1", "p_x2", "p_x12"))
#filter(prob_type %in% c("p_1_2", "p_2_1"))

ggplot(data_lao, aes(x = factor(age))) +
  geom_point(aes(y = q_x1, colour = "DENV1"), size = 2) +
  geom_errorbar(aes(ymin = lower_q_x1, ymax = upper_q_x1, colour = "DENV1"), width = 0.2) +
  geom_point(aes(y = q_x2, colour = "DENV2"), size = 2) +
  geom_errorbar(aes(ymin = lower_q_x2, ymax = upper_q_x2, colour = "DENV2"), width = 0.2) +
  theme_bw() +
  theme(legend.position = "top") + 
  labs(
    x = "Age", y = "Probability",
    colour = "Seroprevalence"
  )

####
# Combine and prepare data
combine_data <- function(df, region_name) {
  df %>%
    mutate(Province = region_name) %>%
    mutate(
      q_x1 = (n_denv1 + n_denv12) / n_tested,
      q_x2 = (n_denv2 + n_denv12) / n_tested,
      lower_q_x1 = pmax(q_x1 - 1.96 * sqrt(q_x1 * (1 - q_x1) / n_tested), 0),
      upper_q_x1 = pmin(q_x1 + 1.96 * sqrt(q_x1 * (1 - q_x1) / n_tested), 1),
      lower_q_x2 = pmax(q_x2 - 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested), 0),
      upper_q_x2 = pmin(q_x2 + 1.96 * sqrt(q_x2 * (1 - q_x2) / n_tested), 1)
    )
}

# Apply to all regions
lao_total <- combine_data(data_lao_total, "Total")
lao_Houaphan <- combine_data(data_lao_Houaphan, "Houaphan")
lao_Luangprabang <- combine_data(data_lao_Luangprabang, "Luangprabang")
lao_Oudomxay <- combine_data(data_lao_Oudomxay, "Oudomxay")
lao_Xiengkhouang <- combine_data(data_lao_Xiengkhouang, "Xiengkhouang")

# Bind all
lao_combined <- bind_rows(lao_total, lao_Houaphan, lao_Luangprabang, lao_Oudomxay, lao_Xiengkhouang) %>%
  mutate(
    age = case_when(
      age >= 5  & age <= 10 ~ "05-10",
      age > 10 & age <= 15 ~ "10-15",
      age > 15 & age <= 20 ~ "15-20",
      age > 20 & age <= 25 ~ "20-25",
      age > 25 & age <= 30 ~ "25-30",
      age > 30 & age <= 35 ~ "30-35",
      age > 35 & age <= 40 ~ "35-40",
      age > 40 & age <= 45 ~ "40-45",
      age > 45 & age <= 50 ~ "45-50",
      age > 50 & age <= 55 ~ "50-55",
      age > 55 & age <= 60 ~ "55-60",
      age > 60 ~ "60+",
      TRUE ~ NA_character_),
    age_group = factor(age))

# Pivot to long format for ggplot
lao_long <- lao_combined %>%
  pivot_longer(cols = c(q_x1, q_x2, lower_q_x1, upper_q_x1, lower_q_x2, upper_q_x2),
               names_to = c(".value", "Serotype"),
               names_pattern = "(q_x|lower_q_x|upper_q_x)(1|2)") %>%
  mutate(Serotype = recode(Serotype, "1" = "DENV1", "2" = "DENV2"),
         Province = factor(Province, levels = c("Houaphan", "Luangprabang", "Oudomxay", "Xiengkhouang", "Total"))
  )


# Plot
ggplot(lao_long, aes(x = age_group, y = q_x, colour = Serotype)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_q_x, ymax = upper_q_x), width = 0.2,
                position = position_dodge(width = 0.5)) +
  facet_wrap(~Province, ncol = 2) +
  theme_bw() +
  theme(
    legend.position = c(0.80, 0.05),
    legend.justification = c("right", "bottom"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.8, hjust = 0.8),
    axis.text.y = element_text(size = 14)
  ) +
  labs(x = "Age", y = "Seroprevalence", colour = "Serotype")




## Check Indepence of 2 serotypes

lao_list_with_p <- data_lao %>%
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

lao_all_pval <- bind_rows(lao_list_with_p)

ggplot(lao_all_pval, aes(x = age)) +
  geom_line(aes(y = p_x12, colour = "Observed p_x12")) +
  geom_line(aes(y = q_x12, colour = "Estimated q_x12")) +
  geom_text(aes(y = p_x12 + 0.02, label = ifelse(p_value < 0.05, "p<0.05", sprintf("p=%.3f", p_value))), size = 2, hjust = 0) +
  theme_minimal() +
  theme(legend.position = "top") + 
  labs(
    x = "Age", y = "Probability",
    colour = "Legend"
  )


ggplot(lao_all_pval, aes(x = factor(age))) +
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

#### FITTING CONSTANT FOI MODEL WITH INTERACTION ----

fit_constant_foi <- function(data, iterations = 2000){
  age_fine <- seq(5, max(data$age), by=1)
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
  fit <- sampling(stan_model, data = stan_data, iter = iterations, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
}

#fit_lao_total <- fit_constant_foi(data_lao_total, iterations = 20000) #38346 divergent transitions after warmup
print(fit_lao_total, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 4)

#fit_lao_Houaphan <- fit_constant_foi(data_lao_Houaphan, iterations = 8000) #37678 divergent transitions after warmup
print(fit_lao_Houaphan, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 4)
shinystan::launch_shinystan(fit_lao_Houaphan)

#fit_lao_Luangprabang <- fit_constant_foi(data_lao_Luangprabang, iterations = 20000) #37386 divergent transitions after warmup
print(fit_lao_Luangprabang, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 4)

#fit_lao_Oudomxay <- fit_constant_foi(data_lao_Oudomxay, iterations = 20000) #38310 divergent transitions after warmup
print(fit_lao_Oudomxay, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 4)

#fit_lao_Xiengkhouang <- fit_constant_foi(data_lao_Xiengkhouang, iterations = 20000) #37786 divergent transitions after warmup
print(fit_lao_Xiengkhouang, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 4)

fits_lao_inter <- list(
  Total = fit_lao_total,
  Houaphan = fit_lao_Houaphan,
  Luangprabang = fit_lao_Luangprabang,
  Oudomxay = fit_lao_Oudomxay,
  Xiengkhouang = fit_lao_Xiengkhouang
)

extract_lao_summary <- function(fit, location) {
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
summary_lao_inter <- imap_dfr(fits_lao_inter, extract_lao_summary)

# Assuming `summary_lao` is your extracted and combined tibble
summary_lao_inter <- summary_lao_inter %>%
  mutate(
    location = factor(location, levels = unique(location)),
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

position_dodge_val <- position_dodge(width = 0.5)

ggplot(summary_lao_inter, aes(x = location, y = mean, color = label)) +
  #geom_line(aes(group = label), linetype = "dotted", position = position_dodge_val) +
  geom_point(aes(group = label), size = 2, position = position_dodge_val) +
  geom_errorbar(aes(ymin = q025, ymax = q975, group = label), 
                width = 0.4, position = position_dodge_val) +
  geom_text(aes(label = format(round(mean, 4), scientific = FALSE)), 
             position = position_dodge(width = 0.5), 
             vjust = -1, hjust = 0, size = 4) +
  geom_hline(data = filter(summary_lao_inter, group == "sigma"), 
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
    title = "Posterior Mean and 95% CrI by Region (Laos)",
    x = "Region", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18, angle = 30, vjust = 0.8, hjust = 0.8),
    axis.text.y = element_text(size = 18)
  )

### POSTERIOR PREDICTIVE CHECK ----
make_ppc_plot <- function(data_name, data, fit) {
  age_fine <- seq(5, max(data$age), by=1)
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

ppc_laos <- make_ppc_plot(data_name = "Laos - Total population", data=data_lao_total, fit=fit_lao_total)
ppc_laos
make_ppc_plot(data_name = "Laos - Houaphan", data=data_lao_Houaphan, fit=fit_lao_Houaphan)
make_ppc_plot(data_name = "Laos - Luangprabang", data=data_lao_Luangprabang, fit=fit_lao_Luangprabang)
make_ppc_plot(data_name = "Laos - Oudomxay", data=data_lao_Oudomxay, fit=fit_lao_Oudomxay)
make_ppc_plot(data_name = "Laos - Xiengkhouang", data=data_lao_Xiengkhouang, fit=fit_lao_Xiengkhouang)

### FITTING CONSTANT, NO-INTERACTION MODEL ----
fit_no_inter <- function(data, iterations = 2000){
  age_fine <- seq(5, max(data$age), by=1)
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
  fit <- sampling(stan_model, data = stan_data, iter = iterations, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
}

fit_lao_total_no_inter <- fit_no_inter(data_lao_total, iterations = 20000)
print(fit_lao_total_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 4)
make_ppc_plot(data_name = "Laos", data=data_lao_total, fit=fit_lao_total_no_inter)

fit_lao_Houaphan_no_inter <- fit_no_inter(data_lao_Houaphan, iterations = 20000)
print(fit_lao_Houaphan_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 4)

fit_lao_Luangprabang_no_inter <- fit_no_inter(data_lao_Luangprabang, iterations = 20000)
print(fit_lao_Luangprabang_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 4)

fit_lao_Oudomxay_no_inter <- fit_no_inter(data_lao_Oudomxay, iterations = 20000)
print(fit_lao_Oudomxay_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 4)

fit_lao_Xiengkhouang_no_inter <- fit_no_inter(data_lao_Xiengkhouang, iterations = 20000)
print(fit_lao_Xiengkhouang_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 4)

### LAMBDA COMPARISON ACROSS PROVINCES ----
fits_lao_no_inter <- list(
  Total = fit_lao_total,
  Houaphan = fit_lao_Houaphan,
  Luangprabang = fit_lao_Luangprabang,
  Oudomxay = fit_lao_Oudomxay,
  Xiengkhouang = fit_lao_Xiengkhouang
)

extract_lao_lambda_summary <- function(fit, location) {
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

summary_lambda_lao_no_inter <- imap_dfr(fits_lao_no_inter, extract_lao_lambda_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "lambda1" = "lambda[1]",
                                  "lambda2" = "lambda[2]"))

summary_lambda_lao_no_inter <- summary_lambda_lao_no_inter %>%
  mutate(model = "No interaction")

summary_lambda_lao_inter <- summary_lao_inter %>%
  filter(parameter %in% c("lambda1", "lambda2")) %>%
  mutate(
    
    parameter_label = recode(parameter,
                             "lambda1" = "lambda[1]",
                             "lambda2" = "lambda[2]"),
    model = "With interaction"
  ) %>%
  select(location, parameter, parameter_label, mean, q025, median, q975, model)

# Combine both datasets
combined_lambda_lao <- bind_rows(summary_lambda_lao_no_inter, summary_lambda_lao_inter)

shared_color_scale <- scale_color_manual(
  name = "Parameter",
  values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
  labels = c(expression(lambda[1]), expression(lambda[2]))
)

g_lambda_lao <- ggplot(combined_lambda_lao, aes(x = location, y = mean, color = parameter_label)) +
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

g_lambda_lao

### SIGMA COMPARISON ACROSS PROVINCES ----
extract_sigma_lao_summary <- function(fit, location) {
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

summary_sigma_lao <- imap_dfr(fits_lao_inter, extract_sigma_lao_summary) %>% 
  mutate(location = fct_relevel(location, "Total"),
          parameter_label = recode(parameter,
                                  "sigma12" = "sigma[12]",
                                  "sigma21" = "sigma[21]")) %>% 
  mutate(label_x = as.numeric(factor(location)) + ifelse(parameter == "sigma12", -0.3, 0.3),
         label_text = sprintf("%.1f", mean))  # Format to 1 decimal place

g_sigma_lao <- ggplot(summary_sigma_lao , aes(x = location, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.3), width = 0.4) +
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

g_sigma_lao

### MODEL COMPARISON - LOO
loo(extract_log_lik(fit_lao_total_no_inter))
loo(extract_log_lik(fit_lao_total))
loo_compare(loo(extract_log_lik(fit_lao_total_no_inter)), loo(extract_log_lik(fit_lao_total)))

loo(extract_log_lik(fit_lao_Houaphan_no_inter))
loo(extract_log_lik(fit_lao_Houaphan))
loo_compare(loo(extract_log_lik(fit_lao_Houaphan_no_inter)), loo(extract_log_lik(fit_lao_Houaphan)))

loo(extract_log_lik(fit_lao_Luangprabang_no_inter))
loo(extract_log_lik(fit_lao_Luangprabang))
loo_compare(loo(extract_log_lik(fit_lao_Luangprabang_no_inter)), loo(extract_log_lik(fit_lao_Luangprabang)))

loo(extract_log_lik(fit_lao_Oudomxay_no_inter))
loo(extract_log_lik(fit_lao_Oudomxay))
loo_compare(loo(extract_log_lik(fit_lao_Oudomxay_no_inter)), loo(extract_log_lik(fit_lao_Oudomxay)))

loo(extract_log_lik(fit_lao_Xiengkhouang_no_inter))
loo(extract_log_lik(fit_lao_Xiengkhouang))
loo_compare(loo(extract_log_lik(fit_lao_Xiengkhouang_no_inter)), loo(extract_log_lik(fit_lao_Xiengkhouang)))
