# -----------------------------------------
# REAL-WORLD DATA FITTING: PERU STUDY 1993-2010 ----
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
library(cowplot)
#library(writexl)

options(mc.cores=4)
## DATA CLEANING ----
data_peru <- read_excel("data/cohorts_data.xlsx")

data_cleaning <- function(data, year) {
  data %>%
    filter(sampleyear == year) %>%
    filter(denv3_status %in% c(0, "0", "NA"),
           denv4_status %in% c(0, "0", "NA")) %>%
    select(agegrp, denv1_status, denv2_status, denv3_status, denv4_status) %>%
    filter(!is.na(denv1_status), !is.na(denv2_status)) %>%
    mutate(
      denv1_status = case_when(
        denv1_status %in% c(1, "1") ~ 1,
        denv1_status %in% c(0, "0") ~ 0,
        TRUE ~ NA_real_
      ),
      denv2_status = case_when(
        denv2_status %in% c(1, "1") ~ 1,
        denv2_status %in% c(0, "0") ~ 0,
        TRUE ~ NA_real_
      ),
      age = case_when(
        #agegrp == "age05" ~ 5,
        #agegrp == "age05-10" ~ 7.5,
        #agegrp %in% c("age05", "age05-10") ~ 7.5,
        #agegrp == "age10-15" ~ 12.5,
        #agegrp == "age15-20" ~ 17.5,
        #agegrp == "age20-25" ~ 22.5,
        #agegrp == "age25-30" ~ 27.5,
        #agegrp == "age30-35" ~ 32.5,
        #agegrp == "age35-40" ~ 37.5,
        #agegrp == "age40-45" ~ 42.5,
        #agegrp == "age45-50" ~ 47.5,
        #agegrp == "age50-55" ~ 52.5,
        #agegrp == "age55-60" ~ 57.5,
        #agegrp == "age60+" ~ 62.5,
        agegrp %in% c("age05", "age05-10") ~ "05-10",
        agegrp == "age10-15" ~ "10-15",
        agegrp == "age15-20" ~ "15-20",
        agegrp == "age20-25" ~ "20-25",
        agegrp == "age25-30" ~ "25-30",
        agegrp == "age30-35" ~ "30-35",
        agegrp == "age35-40" ~ "35-40",
        agegrp == "age40-45" ~ "40-45",
        agegrp == "age45-50" ~ "45-50",
        agegrp == "age50-55" ~ "50-55",
        agegrp == "age55-60" ~ "55-60",
        agegrp == "age60+" ~ "60+",
        #TRUE ~ NA_real_
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age), !is.na(denv1_status), !is.na(denv2_status)) %>%
    mutate(
      only_denv1 = as.numeric(denv1_status == 1 & denv2_status == 0),
      only_denv2 = as.numeric(denv1_status == 0 & denv2_status == 1),
      both_denv12 = as.numeric(denv1_status == 1 & denv2_status == 1)
    ) %>%
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


peru_2010 <- data_cleaning (data_peru, 2010)
peru_2008 <- data_cleaning (data_peru, 2008)
peru_2006 <- data_cleaning (data_peru, 2006)
peru_2004 <- data_cleaning (data_peru, 2004)
peru_2002 <- data_cleaning (data_peru, 2002)
peru_2001 <- data_cleaning (data_peru, 2001)
peru_1999 <- data_cleaning (data_peru, 1999)
peru_1996 <- data_cleaning (data_peru, 1996)
peru_1995 <- data_cleaning (data_peru, 1995)
peru_1994 <- data_cleaning (data_peru, 1994)
peru_1993 <- data_cleaning (data_peru, 1993)

peru_2010 <- peru_2010 %>%
  mutate(
    p_s = n_s/n_tested,
    p_x1 =  n_denv1/n_tested,
    p_x2 =  n_denv2/n_tested,
    p_x12 =  n_denv12/n_tested
  )

## Plot seroprevalence of DENV-1 and DENV-2
years <- c(2010,2008, 2006, 2004, 2002, 2001, 1999, 1996, 1995, 1994, 1993)
peru_list <- map(years, function(y) {
  df <- get(paste0("peru_", y))
  df %>%
    mutate(
      year = y,
      p_s = n_s / n_tested,
      p_x1 = n_denv1 / n_tested,
      p_x2 = n_denv2 / n_tested,
      p_x12 = n_denv12 / n_tested,
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
})

peru_all <- bind_rows(peru_list)

peru_all <- peru_all %>%
  mutate(
    age_num = as.numeric(as.character(age)),
    age_jitter = age_num + runif(n(), -0.2, 0.2),
    xmin = age_jitter - 0.2,
    xmax = age_jitter + 0.2
  )


# Boxplot beside each other
peru_long <- peru_all %>%
  select(age, year, n_tested,
         q_x1, lower_q_x1, upper_q_x1,
         q_x2, lower_q_x2, upper_q_x2) %>%
  pivot_longer(
    cols = c(q_x1, q_x2, lower_q_x1, lower_q_x2, upper_q_x1, upper_q_x2),
    names_to = c(".value", "serotype"),
    names_pattern = "(q_x|lower_q_x|upper_q_x)(1|2)"
  ) %>%
  mutate(
    serotype = recode(serotype, "1" = "DENV1", "2" = "DENV2"),
    age = factor(age)
  )

ggplot(peru_long, aes(x = age, y = q_x, color = serotype, group = serotype)) +
  geom_point(position = position_dodge(width = 0.6), size = 1) +
  geom_errorbar(aes(ymin = lower_q_x, ymax = upper_q_x),
                position = position_dodge(width = 0.6), width = 0.6) +
  scale_colour_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  facet_wrap(~ year) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  theme(
    legend.position = c(0.90, 0.05),
    legend.justification = c("right", "bottom"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 14, angle = 60, vjust = 0.8, hjust = 0.8),
    axis.text.y = element_text(size = 18)
  ) +
  labs(
    x = "Age", y = "Probability",
    colour = "Seroprevalence"
  )

###########

## Testing for independence between DENV1 and DENV2 ----
peru_list_with_p <- map(years, function(y) {
  df <- get(paste0("peru_", y)) %>%
    mutate(
      year = y,
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
  return(df)
})

peru_all_pval <- bind_rows(peru_list_with_p)

ggplot(peru_all_pval, aes(x = factor(age))) +
  # Line to connect observed points
  geom_line(aes(y = p_x12, group = year, colour = "Observed")) +
  # Observed points with shape by significance
  geom_point(aes(y = p_x12, colour = "Observed", shape = p_value < 0.05), size = 2.5) +
  # Expected points
  geom_line(aes(y = q_x12, group = year, colour = "Expected")) +
  geom_point(aes(y = q_x12, colour = "Expected"), size = 2) +
  # Shape and colour adjustments
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 17), name = "p < 0.05") +
  scale_colour_manual(values = c("Observed" = "steelblue", "Expected" = "tomato")) +
  facet_wrap(~ year) +
  theme_bw() +
  theme(legend.position = "top",
        strip.text = element_text(size = 12)) +
  labs(
    x = "Age midpoint", y = "Probability",
    colour = "Seroprevalence of DENV1 + DENV2"
  )



## FITTING DATA EACH YEAR ----
fit_constant_foi <- function(data, iterations = 4000){
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

#### Fit all datasets
#fit_peru_2010 <- fit_constant_foi(peru_2010, iterations = 10000)
print(fit_peru_2010, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits = 3)
#shinystan::launch_shinystan(fit_peru_2010)

#fit_peru_2008 <- fit_constant_foi(peru_2008)
print(fit_peru_2008, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_2006 <- fit_constant_foi(peru_2006, iterations = 6000)
print(fit_peru_2006, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_2004 <- fit_constant_foi(peru_2004, iterations = 10000) #23922 divergent transitions after warmup
print(fit_peru_2004, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)
                
#fit_peru_2002 <- fit_constant_foi(peru_2002, iterations = 8000) #3139 divergent transitions after warmup
print(fit_peru_2002, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_2001 <- fit_constant_foi(peru_2001)
print(fit_peru_2001, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_1999 <- fit_constant_foi(peru_1999)
print(fit_peru_1999, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_1996 <- fit_constant_foi(peru_1996)
print(fit_peru_1996, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_1995 <- fit_constant_foi(peru_1995, iterations = 6000) #2828 divergent transitions after warmup
print(fit_peru_1995, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_1994 <- fit_constant_foi(peru_1994, iterations = 6000) #3456 divergent transitions after warmup
print(fit_peru_1994, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

#fit_peru_1993 <- fit_constant_foi(peru_1993)
print(fit_peru_1993, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975), digits=3)

# List of model fits and associated years
fits_peru_inter <- list(
  `2010` = fit_peru_2010,
  `2008` = fit_peru_2008,
  `2006` = fit_peru_2006,
  `2004` = fit_peru_2004,
  `2002` = fit_peru_2002,
  `2001` = fit_peru_2001,
  `1999` = fit_peru_1999,
  `1996` = fit_peru_1996,
  `1995` = fit_peru_1995,
  `1994` = fit_peru_1994,
  `1993` = fit_peru_1993
)

## SUMMARY & COMPARE PARAMETER ESTIMATES - CONSTANT FOI MODEL ----
# Function to extract summary for selected parameters
extract_summary <- function(fit, year) {
  summ <- summary(fit, pars = c("lambda1", "lambda2", "sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    year = year,
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}

summary_all_years_peru <- imap_dfr(fits, extract_summary)
  
summary_all_years_peru <- summary_all_years_peru %>%
  mutate(
    year_numeric = as.numeric(as.character(year)),
    label = recode(parameter,
                   "lambda1" = "lambda[1]",
                   "lambda2" = "lambda[2]",
                   "sigma12" = "sigma[12]",
                   "sigma21" = "sigma[21]"),
    group = case_when(
      parameter %in% c("lambda1", "lambda2") ~ "paste('Force of infection (', lambda, ')')",
      parameter %in% c("sigma12", "sigma21") ~ "paste('Interaction parameter (', sigma, ')')"
    )
  ) %>%
  filter(!is.na(group))  # remove unintended σ-only panel

position_dodge_val <- position_dodge(width = 0.5)

ggplot(summary_all_years_peru, aes(x = year_numeric, y = mean, color = label)) +
  geom_line(aes(group = label), linetype = "dotted", position = position_dodge_val) +
  geom_point(aes(group = label), size = 2, position = position_dodge_val) +
  geom_errorbar(aes(ymin = q025, ymax = q975, group = label), width = 0.4, position = position_dodge_val) +
  
  # Add red dashed σ = 1 line to Interaction panel
  geom_hline(
    data = data.frame(group = "paste('Interaction parameter (', sigma, ')')", yintercept = 1),
    aes(yintercept = yintercept),
    linetype = "dashed",
    color = "red"
  ) +
  
  # Red σ = 1 label
  geom_text(
    data = data.frame(
      group = "paste('Interaction parameter (', sigma, ')')",
      year_numeric = max(summary_all_years_peru$year_numeric),
      y = 1
    ),
    aes(x = year_numeric, y = y, label = "bold(sigma == 1)"),
    color = "red",
    parse = TRUE,
    vjust = -0.5,
    hjust = 1.1,
    inherit.aes = FALSE,
    size = 8
  ) +
  
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
  scale_x_continuous(breaks = unique(summary_all_years_peru$year_numeric)) +
  facet_wrap(~ group, scales = "free_y", labeller = label_parsed) +
  labs(
    title = "Posterior Mean and 95% CrI by Year",
    x = "Year", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    strip.text = element_text(size = 18),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18, angle = 60, vjust = 0.8, hjust = 0.8),
    axis.text.y = element_text(size = 18)
  )


#### POSTERIOR PREDICTIVE CHECK - CONSTANT FOI MODEL ----
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
      #plot.title = ggtext::element_markdown(),
      #plot.title.position = "plot",
      strip.text = element_text(size = 18),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18)
    )
}

ppc_peru_2010 <- make_ppc_plot(data_name = "Peru 2010", data=peru_2010, fit=fit_peru_2010) 
ppc_peru_2010
make_ppc_plot(data_name = "Peru 2008", data=peru_2008, fit=fit_peru_2008)
make_ppc_plot(data_name = "Peru 2006", data=peru_2006, fit=fit_peru_2006)
make_ppc_plot(data_name = "Peru 2004", data=peru_2004, fit=fit_peru_2004)
make_ppc_plot(data_name = "Peru 2002", data=peru_2002, fit=fit_peru_2002)
make_ppc_plot(data_name = "Peru 2001", data=peru_2001, fit=fit_peru_2001)
make_ppc_plot(data_name = "Peru 1999", data=peru_1999, fit=fit_peru_1999)
make_ppc_plot(data_name = "Peru 1996", data=peru_1996, fit=fit_peru_1996)
make_ppc_plot(data_name = "Peru 1995", data=peru_1995, fit=fit_peru_1995)
make_ppc_plot(data_name = "Peru 1994", data=peru_1994, fit=fit_peru_1994)
make_ppc_plot(data_name = "Peru 1993", data=peru_1993, fit=fit_peru_1993)

### Fit time-varying model (cut-off 2000) to Peru 2010 & COMPARE ----
current_year <- 2010
cutoff_year <- 2000
peru_2010_time_varying_stan_data <- list(
  N = nrow(peru_2010),
  age = peru_2010$age,
  n_tested = peru_2010$n_tested,
  y = as.matrix(peru_2010[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = length(age_fine),
  cutoff_age = current_year - cutoff_year
)

stan_two_piece_wise_model <- stan_model("stan/two_piece-wise_FOI.stan")
stan_two_piece_wise_no_inter_model <- stan_model("stan/two_piece-wise_FOI-no-inter.stan")
#fit_peru_2010_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2010_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2010_time_varying, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)
#fit_peru_2010_time_varying_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = peru_2010_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2010_time_varying_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)

time_varying_stan_data <- function(data, current_year) {
  list(
  N = nrow(data),
  age = data$age,
  n_tested = data$n_tested,
  y = as.matrix(data[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = length(age_fine),
  cutoff_age = current_year - cutoff_year
)
}

peru_2008_time_varying_stan_data <- time_varying_stan_data(peru_2008, 2008)
peru_2006_time_varying_stan_data <- time_varying_stan_data(peru_2006, 2006)
peru_2004_time_varying_stan_data <- time_varying_stan_data(peru_2004, 2004)
peru_2002_time_varying_stan_data <- time_varying_stan_data(peru_2002, 2002)
peru_2001_time_varying_stan_data <- time_varying_stan_data(peru_2001, 2001)
peru_1999_time_varying_stan_data <- time_varying_stan_data(peru_1999, 1999)
peru_1996_time_varying_stan_data <- time_varying_stan_data(peru_1996, 1996)

#fit_peru_2008_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2008_time_varying_stan_data, iter = 4000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2008_time_varying, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)
#fit_peru_2008_time_varying_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = peru_2008_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2008_time_varying_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)
make_ppc_plot(data_name = "Peru 2008", data=peru_2008, fit=fit_peru_2008_time_varying) 
make_ppc_plot(data_name = "Peru 2008", data=peru_2008, fit=fit_peru_2008_time_varying_no_inter) 

#fit_peru_2006_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2006_time_varying_stan_data, iter = 4000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2006_time_varying, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)
#fit_peru_2006_time_varying_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = peru_2006_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2006_time_varying_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)
make_ppc_plot(data_name = "Peru 2006", data=peru_2006, fit=fit_peru_2006_time_varying) 
make_ppc_plot(data_name = "Peru 2006", data=peru_2006, fit=fit_peru_2006_time_varying_no_inter) 

#fit_peru_2004_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2004_time_varying_stan_data, iter = 40000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2004_time_varying, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)
#fit_peru_2004_time_varying_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = peru_2004_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2004_time_varying_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)
make_ppc_plot(data_name = "Peru 2004", data=peru_2004, fit=fit_peru_2004_time_varying) 
make_ppc_plot(data_name = "Peru 2004", data=peru_2004, fit=fit_peru_2004_time_varying_no_inter) 

fit_peru_2002_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2002_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2002_time_varying, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)

fit_peru_2002_time_varying_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = peru_2002_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2002_time_varying_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)
make_ppc_plot(data_name = "Peru 2002", data=peru_2002, fit=fit_peru_2002_time_varying) 
make_ppc_plot(data_name = "Peru 2002", data=peru_2002, fit=fit_peru_2002_time_varying_no_inter) 

#fit_peru_2001_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2001_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2001_time_varying, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b", "sigma12", "sigma21"), digits = 3)
#fit_peru_2001_time_varying_no_inter <- sampling(stan_two_piece_wise_no_inter_model, data = peru_2001_time_varying_stan_data, iter = 4000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))
print(fit_peru_2001_time_varying_no_inter, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"), digits = 3)
make_ppc_plot(data_name = "Peru 2001", data=peru_2001, fit=fit_peru_2001_time_varying) 
make_ppc_plot(data_name = "Peru 2001", data=peru_2001, fit=fit_peru_2001_time_varying_no_inter) 

### Fit time-varying model (cut-off 2000) to Peru 2010 & COMPARE
current_year <- 2010
cutoff_year <- 2000
peru_2010_time_varying_stan_data <- list(
  N = nrow(peru_2010),
  age = peru_2010$age,
  n_tested = peru_2010$n_tested,
  y = as.matrix(peru_2010[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = length(age_fine),
  cutoff_age = current_year - cutoff_year
)

stan_two_piece_wise_model <- stan_model("stan/two_piece-wise_FOI.stan")
stan_two_piece_wise_no_inter_model <- stan_model("stan/two_piece-wise_FOI-no-inter.stan")
#fit_peru_2010_time_varying <- sampling(stan_two_piece_wise_model, data = peru_2010_time_varying_stan_data, iter = 8000, chains = 4, control = list(adapt_delta = 0.99, max_treedepth = 15))


## PPC time-varying (with or without interaction)
posterior <- rstan::extract(fit_peru_2010_time_varying)
#posterior <- rstan::extract(fit_peru_2010_time_varying_no_inter)

y <- peru_2010_time_varying_stan_data$y              
n_tested <- peru_2010_time_varying_stan_data$n_tested  
age <- peru_2010_time_varying_stan_data$age            

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

age_fine <- peru_2010_time_varying_stan_data$age_fine

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
    title = "<span style='font-size:16pt'><b>Posterior Predictive Check of age-stratified infection probabilities: Model estimates versus simulated data</b></span><br>
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


# FITTING NO-INTERACTION MODEL ----
fit_no_inter <- function(data, iterations = 4000){
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

fit_peru_2010_no_inter <- fit_no_inter(peru_2010)
print(fit_peru_2010_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 2010", data=peru_2010, fit=fit_peru_2010_no_inter) 

fit_peru_2008_no_inter <- fit_no_inter(peru_2008)
print(fit_peru_2008_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 2008", data=peru_2008, fit=fit_peru_2008_no_inter) 

fit_peru_2006_no_inter <- fit_no_inter(peru_2006)
print(fit_peru_2006_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 2006", data=peru_2006, fit=fit_peru_2006_no_inter) 

fit_peru_2004_no_inter <- fit_no_inter(peru_2004)
print(fit_peru_2004_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 2004", data=peru_2004, fit=fit_peru_2004_no_inter) 

fit_peru_2002_no_inter <- fit_no_inter(peru_2002)
print(fit_peru_2002_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 2002", data=peru_2002, fit=fit_peru_2002_no_inter) 

fit_peru_2001_no_inter <- fit_no_inter(peru_2001)
print(fit_peru_2001_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 4)
make_ppc_plot(data_name = "Peru 2001", data=peru_2001, fit=fit_peru_2001_no_inter) 

fit_peru_1999_no_inter <- fit_no_inter(peru_1999)
print(fit_peru_1999_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 1999", data=peru_1999, fit=fit_peru_1999_no_inter) 

fit_peru_1996_no_inter <- fit_no_inter(peru_1996)
print(fit_peru_1996_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 1996", data=peru_1996, fit=fit_peru_1996_no_inter) 

fit_peru_1995_no_inter <- fit_no_inter(peru_1995)
print(fit_peru_1995_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 1995", data=peru_1995, fit=fit_peru_1995_no_inter) 

fit_peru_1994_no_inter <- fit_no_inter(peru_1994)
print(fit_peru_1994_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 1994", data=peru_1994, fit=fit_peru_1994_no_inter) 

fit_peru_1993_no_inter <- fit_no_inter(peru_1993)
print(fit_peru_1993_no_inter, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975), digits = 3)
make_ppc_plot(data_name = "Peru 1993", data=peru_1993, fit=fit_peru_1993_no_inter) 

## LOO-CV COMPARE 4 MODEL FITS TO PERU 2010 -----
log_lik_matrix_1 <- extract_log_lik(fit_peru_2010, parameter_name = "log_lik")
log_lik_matrix_2 <- extract_log_lik(fit_peru_2010_time_varying, parameter_name = "log_lik")
log_lik_matrix_3 <- extract_log_lik(fit_peru_2010_no_inter, parameter_name = "log_lik")
log_lik_matrix_4 <- extract_log_lik(fit_peru_2010_time_varying_no_inter, parameter_name = "log_lik")

loo1 <- loo(log_lik_matrix_1)
loo1
loo2 <- loo(log_lik_matrix_2)
loo2
loo3 <- loo(log_lik_matrix_3)
loo3
loo4 <- loo(log_lik_matrix_4)
loo4

comp <- loo_compare(loo1, loo2, loo3, loo4)

comp

pareto_k_vals <- loo2$diagnostics$pareto_k
which(pareto_k_vals > 0.7)
plot(loo2)

### LOOP FOR CUT-OFFs

current_year <- 2010
cutoff_years <- seq(2010, 1950, by = -5)


stan_two_piece_wise_model <- stan_model("stan/two_piece-wise_FOI.stan")


elpd_results <- list()


for (cutoff_year in cutoff_years) {
  message("Running for cutoff year: ", cutoff_year)
  
  cutoff_age <- current_year - cutoff_year
  stan_data <- list(
    N = nrow(peru_2010),
    age = peru_2010$age,
    n_tested = peru_2010$n_tested,
    y = as.matrix(peru_2010[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = length(age_fine),
    cutoff_age = cutoff_age
  )
  
  fit <- sampling(
    stan_two_piece_wise_model,
    data = stan_data,
    iter = 4000, chains = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
  
  posterior <- rstan::extract(fit)
  
  #log_lik <- extract_log_lik(fit, parameter_name = "log_lik")
  #loo_result <- loo(log_lik)
  
  #elpd_results[[as.character(cutoff_year)]] <- loo_result$estimates["elpd_loo", "Estimate"]
  
  if (!exists("loo1")) {
    log_lik_matrix_1 <- extract_log_lik(fit_peru_2010, parameter_name = "log_lik")
    log_lik_matrix_3 <- extract_log_lik(fit_peru_2010_no_inter, parameter_name = "log_lik")
    
    loo1 <- loo(log_lik_matrix_1)
    loo3 <- loo(log_lik_matrix_3)
  }
  
  log_lik_matrix_current <- extract_log_lik(fit, parameter_name = "log_lik")
  loo_current <- loo(log_lik_matrix_current)
  
  comp <- loo_compare(loo1, loo_current, loo3)
  
  elpd_diff <- as.data.frame(comp) %>%
    rownames_to_column("model") %>%
    mutate(cutoff_year = cutoff_year)
  
  if (!exists("loo_comparisons")) {
    loo_comparisons <- elpd_diff
  } else {
    loo_comparisons <- bind_rows(loo_comparisons, elpd_diff)
  }
  
  # PPC for each cutoff
  pred_probs <- array(NA, dim = dim(posterior$y_rep))
  for (j in 1:4) {
    pred_probs[,,j] <- sweep(posterior$y_rep[,,j], 2, stan_data$n_tested, "/")
  }
  
  y <- stan_data$y
  n_tested <- stan_data$n_tested
  age <- stan_data$age
  
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
  
  prob_s  <- 1 - pred_probs[, , 2] - pred_probs[, , 3] - pred_probs[, , 4]
  df_s    <- make_pred_df(prob_s, age, "Susceptible", y[, 1], n_tested)
  df_d1   <- make_pred_df(pred_probs[,,2], age, "DENV1 only", y[, 2], n_tested)
  df_d2   <- make_pred_df(pred_probs[,,3], age, "DENV2 only", y[, 3], n_tested)
  df_d12  <- make_pred_df(pred_probs[,,4], age, "DENV1 + DENV2", y[, 4], n_tested)
  
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
  
  p <- ggplot(df_ppc, aes(x = age)) +
    geom_ribbon(data = df_smooth, aes(x = age, ymin = lower, ymax = upper), fill = "red", alpha = 0.15, inherit.aes = FALSE) +
    geom_line(data = df_smooth, aes(x = age, y = median), color = "red", linewidth = 0.8, inherit.aes = FALSE) +
    geom_point(aes(y = observed), color = "black", size = 2) +
    geom_errorbar(aes(ymin = lower_obs, ymax = upper_obs), width = 0.4) +
    facet_wrap(~ compartment) +
    labs(
      title = paste0("<span style='font-size:16pt'><b>Posterior Predictive Check - Cutoff Year: ", cutoff_year, "</b></span>"),
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
  
  ggsave(filename = paste0("ppc_plots/ppc_cutoff_", cutoff_year, ".png"), plot = p, width = 10, height = 6, dpi = 300)
}

print(loo_comparisons)


ggplot(loo_comparisons, aes(x = cutoff_year, y = elpd_diff, color = model)) +
  geom_line() +
  geom_point() +
  scale_x_reverse() +
  labs(title = "ELPD Difference vs Best Model by Cut-off Year",
       x = "Cut-off year", y = "ELPD difference") +
  theme_minimal(base_size = 14)

best_cutoff <- loo_comparisons %>%
  filter(model == "model2") %>%
  arrange(desc(elpd_diff)) %>%
  slice(1)

print(best_cutoff)


#### IN ONE COMPARISON

# Step 1: Extract LOO for constant models
log_lik_matrix_1 <- extract_log_lik(fit_peru_2010, parameter_name = "log_lik")
log_lik_matrix_3 <- extract_log_lik(fit_peru_2010_no_inter, parameter_name = "log_lik")
loo1 <- loo(log_lik_matrix_1)
loo3 <- loo(log_lik_matrix_3)

# Step 2: Store LOO in a named list
loo_list <- list()
loo_list[["Constant_FOI_with_interaction"]] <- loo1
loo_list[["Constant_FOI_no_interaction"]] <- loo3

# Step 3: Loop through time-varying cutoff years
for (cutoff_year in cutoff_years) {
  message("Running for cutoff year: ", cutoff_year)
  
  cutoff_age <- current_year - cutoff_year
  stan_data <- list(
    N = nrow(peru_2010),
    age = peru_2010$age,
    n_tested = peru_2010$n_tested,
    y = as.matrix(peru_2010[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = length(age_fine),
    cutoff_age = cutoff_age
  )
  
  fit <- sampling(
    stan_two_piece_wise_model,
    data = stan_data,
    iter = 4000, chains = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 15)
  )
  
  log_lik_matrix_current <- extract_log_lik(fit, parameter_name = "log_lik")
  loo_current <- loo(log_lik_matrix_current)
  
  model_name <- paste0("TimeVarying_Cutoff_", cutoff_year)
  loo_list[[model_name]] <- loo_current
}

# Step 4: Reorder to ensure reference model is first
loo_list <- loo_list[c("Constant_FOI_with_interaction", 
                       setdiff(names(loo_list), "Constant_FOI_with_interaction"))]

print(names(loo_list))

loo_comparison_all <- loo_compare(loo_list)

elpd_table <- as.data.frame(loo_comparison_all) %>%
  rownames_to_column("Model") %>%
  mutate(
    Model = factor(Model, levels = rev(Model)),
    cutoff_year = str_extract(Model, "\\d{4}"),
    cutoff_year = as.integer(cutoff_year)
  )

ggplot(elpd_table, aes(x = elpd_diff, y = Model)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = elpd_diff - se_diff, xmax = elpd_diff + se_diff), height = 0.3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "ELPD Differences Compared to Constant FOI with Interaction",
    x = "ELPD Difference (vs Reference Model)",
    y = "Model"
  ) +
  theme_minimal(base_size = 14)


best_model <- elpd_table %>% filter(elpd_diff == max(elpd_diff)) %>% pull(Model)
message("Best model: ", best_model)

print(rownames(loo_compare(loo_list)))

#### TRIAL

current_year <- 2010
cutoff_years <- seq(2010, 1950, by = -5)

loo_list <- list()

# --- constant FOI models ---
log_lik_1 <- extract_log_lik(fit_peru_2010, parameter_name = "log_lik")
log_lik_3 <- extract_log_lik(fit_peru_2010_no_inter, parameter_name = "log_lik")

loo_list[["Constant_FOI_with_interaction"]] <- loo(log_lik_1)
loo_list[["Constant_FOI_no_interaction"]] <- loo(log_lik_3)

# --- time-varying FOI model ---
for (cutoff_year in cutoff_years) {
  message("Running for cutoff year: ", cutoff_year)
  
  cutoff_age <- current_year - cutoff_year
  stan_data <- list(
    N = nrow(peru_2010),
    age = peru_2010$age,
    n_tested = peru_2010$n_tested,
    y = as.matrix(peru_2010[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = length(age_fine),
    cutoff_age = cutoff_age
  )
  
  fit <- sampling(
    stan_two_piece_wise_model,
    data = stan_data,
    iter = 4000, chains = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    refresh = 0
  )
  
  log_lik_matrix <- extract_log_lik(fit, parameter_name = "log_lik")
  loo_result <- loo(log_lik_matrix)
  
  model_name <- paste0("TimeVarying_Cutoff_", cutoff_year)
  loo_list[[model_name]] <- loo_result
}


elpd_values_peru_2010 <- sapply(loo_list, function(x) x$estimates["elpd_loo", "Estimate"])
se_values_peru_2010   <- sapply(loo_list, function(x) x$estimates["elpd_loo", "SE"])

ref_model <- "Constant_FOI_with_interaction"

elpd_diff_peru_2010 <- elpd_values_peru_2010 - elpd_values_peru_2010[ref_model]
se_diff_peru_2010   <- sqrt(se_values_peru_2010^2 + se_values_peru_2010[ref_model]^2)

elpd_table_peru_2010 <- tibble(
  Model = names(loo_list),
  elpd_loo = elpd_values,
  se_elpd_loo = se_values,
  elpd_diff = elpd_diff,
  se_diff = se_diff,
  is_reference = names(loo_list) == ref_model,
  is_no_interaction = names(loo_list) == "Constant_FOI_no_interaction",
  is_significantly_better = elpd_diff > se_diff,
  is_best = elpd_diff == max(elpd_diff),
  is_worst = elpd_diff == min(elpd_diff)
)

elpd_table_peru_2010 <- elpd_table_peru_2010 %>%
  mutate(
    cutoff_year = as.integer(str_extract(Model, "\\d{4}")),
    shape_type = case_when(
      is_reference ~ "Reference",
      is_best ~ "Best",
      is_worst ~ "Worst",
      TRUE ~ "Other"
    )
  )

ordered_models_peru_2010 <- c(
  "Constant_FOI_with_interaction",
  "Constant_FOI_no_interaction",
  elpd_table %>%
    filter(str_detect(Model, "TimeVarying_Cutoff_")) %>%
    arrange(desc(cutoff_year)) %>%
    pull(Model)
)

elpd_table_peru_2010 <- elpd_table_peru_2010 %>%
  mutate(Model = factor(Model, levels = rev(ordered_models)))

shape_values <- c("Reference" = 17, "Best" = 16, "Worst" = 16, "Other" = 16)  # 17 = triangle, 16 = circle
color_values <- c("Reference" = "black", "Best" = "blue", "Worst" = "red" ,"Other" = "black")

# ---- ELPD DIFFERENCE PLOT ----
ggplot(elpd_table_peru_2010, aes(x = elpd_diff, y = Model)) +
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
  xlim(min(elpd_table$elpd_diff - elpd_table$se_diff) - 5,
       max(elpd_table$elpd_diff + elpd_table$se_diff) + 25) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


### FITTING PERU 2010 (CUT-OFF YEAR 2000)
current_year <- 2010
cutoff_year <- 2000
cutoff_age <- current_year - cutoff_year
stan_data <- list(
  N = nrow(peru_2010),
  age = peru_2010$age,
  n_tested = peru_2010$n_tested,
  y = as.matrix(peru_2010[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
  age_fine = age_fine,
  n_age_fine = length(age_fine),
  cutoff_age = cutoff_age
)

fit <- sampling(
  stan_two_piece_wise_model,
  data = stan_data,
  iter = 4000, chains = 4,
  control = list(adapt_delta = 0.99, max_treedepth = 15)
)

### LAMBDA COMPARISON ACROSS YEARS IN PERU ----
fits_peru_no_inter <- list(
  `2010` = fit_peru_2010_no_inter,
  `2008` = fit_peru_2008_no_inter,
  `2006` = fit_peru_2006_no_inter,
  `2004` = fit_peru_2004_no_inter,
  `2002` = fit_peru_2002_no_inter,
  `2001` = fit_peru_2001_no_inter,
  `1999` = fit_peru_1999_no_inter,
  `1996` = fit_peru_1996_no_inter,
  `1995` = fit_peru_1995_no_inter,
  `1994` = fit_peru_1994_no_inter,
  `1993` = fit_peru_1993_no_inter
)


extract_lambda_peru_summary <- function(fit, year) {
  summ <- summary(fit, pars = c("lambda1", "lambda2"), probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    year = as.numeric(year),
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}

summary_lambda_peru_no_inter <- imap_dfr(fits_peru_no_inter, extract_lambda_peru_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "lambda1" = "lambda[1]",
                                  "lambda2" = "lambda[2]"))


ggplot(summary_lambda_peru_no_inter, aes(x = year, y = mean, color = parameter_label)) +
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

summary_lambda_peru_no_inter <- summary_lambda_peru_no_inter %>%
  mutate(model = "No interaction")

summary_lambda_peru_inter <- summary_all_years_peru %>%
  filter(parameter %in% c("lambda1", "lambda2")) %>%
  mutate(
    parameter_label = recode(parameter,
                             "lambda1" = "lambda[1]",
                             "lambda2" = "lambda[2]"),
    year = year_numeric,
    model = "With interaction"
  ) %>%
  select(year, parameter, parameter_label, mean, q025, median, q975, model)

combined_lambda_peru <- bind_rows(summary_lambda_peru_no_inter, summary_lambda_peru_inter)

shared_color_scale <- scale_color_manual(
  name = "Parameter",
  values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
  labels = c(expression(lambda[1]), expression(lambda[2]))
)

g_lambda_peru <- ggplot(combined_lambda_peru, aes(x = year, y = mean, color = parameter_label)) +
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
  scale_x_continuous(breaks = unique(combined_lambda_peru$year)) +
  labs(
    title = "Force of Infection (λ)",
    x = "Year", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10, angle = 60, vjust = , hjust = 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.direction = "vertical",  # stacked
  )

g_lambda_peru


#### SIGMA COMPARISON ACROSS YEARS IN PERU ----

extract_sigma_peru_summary <- function(fit, year) {
  summ <- summary(fit, pars = c("sigma12", "sigma21"), probs = c(0.025, 0.5, 0.975))$summary
  tibble(
    year = as.numeric(year),
    parameter = rownames(summ),
    mean = summ[, "mean"],
    q025 = summ[, "2.5%"],
    median = summ[, "50%"],
    q975 = summ[, "97.5%"]
  )
}

summary_sigma_peru <- imap_dfr(fits_peru_inter, extract_sigma_peru_summary) %>% 
  mutate(parameter_label = recode(parameter,
                                  "sigma12" = "sigma[12]",
                                  "sigma21" = "sigma[21]"))

g_sigma_peru <- ggplot(summary_sigma_peru , aes(x = year, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.3), width = 0.4) +
  geom_line(aes(group = parameter_label), 
            linetype = "dotted", position = position_dodge(width = 0.3)) +
  scale_color_manual(
    name = "Interaction\nparameter",
    values = c("sigma[12]" = "#1b9e77", "sigma[21]" = "#d95f02"),
    labels = c(expression(sigma[12]), expression(sigma[21]))
  ) +
  scale_x_continuous(breaks = unique(summary_sigma_peru$year)) +
  labs(
    title = expression("Interaction parameter (" * sigma * ")"),
    x = "Year", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 45, vjust = 0.8, hjust = 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14),
    legend.direction = "vertical",  # stacked
  )

g_sigma_peru


##### PLOT TIME-VARYING FOI PERU 2010

foi_summary <- summary(fit_peru_2010_time_varying,
                       pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"))$summary


lambda_peru_2010_time_varying_df <- tibble(
  serotype = rep(c("DENV1", "DENV2"), each = 2),
  period = rep(c("Before", "After"), times = 2),
  start = rep(c(1990, 2000), times = 2),
  end = rep(c(2000, 2010), times = 2),
  mean = foi_summary[, "mean"],
  lower = foi_summary[, "2.5%"],
  upper = foi_summary[, "97.5%"]
) %>%
  mutate(
    label = sprintf("%.3f", mean),
    x_label = (start + end) / 2,
    y_label = mean + 0.0015  # điều chỉnh khoảng cách label trên segment
  )

# Plot
ggplot(lambda_peru_2010_time_varying_df) +
  # CrI as shaded rectangles
  geom_rect(aes(xmin = start, xmax = end, ymin = lower, ymax = upper, fill = serotype),
            alpha = 0.2, color = NA) +
  # Step FOI
  geom_segment(aes(x = start, xend = end, y = mean, yend = mean, color = serotype), size = 1.5) +
  # Add lambda values
  geom_text(aes(x = x_label, y = y_label, label = label, color = serotype), 
            size = 4.5, show.legend = FALSE) +
  # Vertical lines
  geom_vline(xintercept = 2000, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_fill_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_x_continuous(breaks = c(1990, 2000, 2010)) +
  labs(
    x = "Time (year)",
    y = "Force of Infection (λ)",
    color = "Serotype",
    fill = "Serotype",
    title = "Estimated Force of Infection with 95% CrI\n(Time-Varying Model, Peru 2010)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Extract posterior draws
draws <- rstan::extract(fit_peru_2010_time_varying,
                        pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"))

draws_summary <- tibble(
  value = c(draws$lambda1_a, draws$lambda1_b, draws$lambda2_a, draws$lambda2_b),
  serotype = rep(c("DENV1", "DENV1", "DENV2", "DENV2"), each = length(draws$lambda1_a)),
  period = rep(c("Before", "After", "Before", "After"), each = length(draws$lambda1_a)),
  x = rep(c(1995, 2005, 1995, 2005), each = length(draws$lambda1_a))  # midpoints
)

summary_points <- draws_summary %>%
  group_by(serotype, period, x) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    .groups = "drop"
  )

ggplot(lambda_peru_2010_time_varying_df) +
  geom_rect(aes(xmin = start, xmax = end, ymin = lower, ymax = upper, fill = serotype),
            alpha = 0.1, color = NA) +
  geom_segment(aes(x = start, xend = end, y = mean, yend = mean, color = serotype), size = 1) +
  #geom_pointrange(data = summary_points,
  #                aes(x = x, y = mean, ymin = lower, ymax = upper, color = serotype),
  #                position = position_dodge(width = 0.4),
  #                shape = 21, size = 0.8, stroke = 1) +
  geom_point(data = summary_points,
               aes(x = x, y = mean, color = serotype),
               position = position_dodge(width = 0.3), size = 2) +
  
  geom_errorbar(data = summary_points,
                aes(x = x, ymin = lower, ymax = upper, color = serotype),
                position = position_dodge(width = 0.3), width = 0.4) +
  # Vertical lines
  geom_vline(xintercept = 2000, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.8) +
  scale_color_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_fill_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010)) +
  labs(
    x = "Time (year)",
    y = "Force of Infection (λ)",
    color = "Serotype",
    fill = "Serotype",
    title = "Estimated Force of Infection with 95% CrI\n(Time-Varying Model, Peru 2010)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 14))

####
library(patchwork)

# Function to extract FOI draws and create plot input
extract_foi_df <- function(fit, model_label) {
  draws <- rstan::extract(fit, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"))
  foi_summary <- summary(fit,
                         pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"))$summary
  
  tibble(
    serotype = rep(c("DENV1", "DENV2"), each = 2),
    period = rep(c("Before", "After"), times = 2),
    start = rep(c(1990, 2000), times = 2),
    end = rep(c(2000, 2010), times = 2),
    mean = foi_summary[, "mean"],
    lower = foi_summary[, "2.5%"],
    upper = foi_summary[, "97.5%"],
    model = model_label,
    x_label = (rep(c(1990, 2000), times = 2) + rep(c(2000, 2010), times = 2)) / 2
  )
}

# Extract for both models
df_with <- extract_foi_df(fit_peru_2010_time_varying, "With interaction")
df_no <- extract_foi_df(fit_peru_2010_time_varying_no_inter, "No interaction")
df_all <- bind_rows(df_with, df_no)

# Plot
p_combined <- ggplot(df_all) +
  geom_rect(aes(xmin = start, xmax = end, ymin = lower, ymax = upper, fill = serotype),
            alpha = 0.2, color = NA) +
  geom_segment(aes(x = start, xend = end, y = mean, yend = mean, color = serotype), size = 1.2) +
  geom_point(aes(x = x_label, y = mean, color = serotype), size = 2) +
  geom_errorbar(aes(x = x_label, ymin = lower, ymax = upper, color = serotype), width = 0.4) +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.8) +
  facet_wrap(~model) +
  scale_color_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_fill_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  labs(
    x = "Time (year)",
    y = "Force of Infection (λ)",
    color = "Serotype",
    fill = "Serotype",
    title = "Estimated Force of Infection with 95% CrI\n(Time-Varying Model, Peru 2010)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 14)
  ) +
  theme_bw()

p_combined

library(tidyverse)
library(rstan)
library(patchwork)

# Function to extract FOI summaries
extract_foi_df <- function(fit, model_label) {
  draws <- rstan::extract(fit, pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"))
  foi_summary <- summary(fit,
                         pars = c("lambda1_a", "lambda1_b", "lambda2_a", "lambda2_b"))$summary
  
  tibble(
    serotype = rep(c("DENV1", "DENV2"), each = 2),
    period = rep(c("Before", "After"), times = 2),
    start = rep(c(1990, 2000), times = 2),
    end = rep(c(2000, 2010), times = 2),
    mean = foi_summary[, "mean"],
    lower = foi_summary[, "2.5%"],
    upper = foi_summary[, "97.5%"],
    model = model_label,
    x_label = (rep(c(1990, 2000), times = 2) + rep(c(2000, 2010), times = 2)) / 2,
    y_label = foi_summary[, "mean"] + 0.0015,  # vertical offset for text
    label = sprintf("%.3f", foi_summary[, "mean"])
  )
}

# Combine both models
df_with <- extract_foi_df(fit_peru_2010_time_varying, "With interaction")
df_no <- extract_foi_df(fit_peru_2010_time_varying_no_inter, "No interaction")
df_all <- bind_rows(df_with, df_no) %>%
  mutate(
    label = sprintf("%.3f", mean),
    x_label = (start + end) / 2,
    y_label = case_when(
      serotype == "DENV1" ~ mean + 0.003,  
      serotype == "DENV2" ~ mean - 0.003  
    )
  )


# Final plot
p_combined_peru <- ggplot(df_all) +
  # CrI bands
  geom_rect(aes(xmin = start, xmax = end, ymin = lower, ymax = upper, fill = serotype),
            alpha = 0.2, color = NA) +
  # Step segments
  geom_segment(aes(x = start, xend = end, y = mean, yend = mean, color = serotype), size = 0.5) +
  # Posterior mean + CrI point with errorbar
  #geom_point(aes(x = x_label, y = mean, color = serotype), size = 2) +
  #geom_errorbar(aes(x = x_label, ymin = lower, ymax = upper, color = serotype), width = 0.4) +
  # Add FOI values as labels
  geom_text(aes(x = x_label, y = y_label, label = label, color = serotype),
            size = 3.5, show.legend = FALSE) +
  # Vertical reference lines
  geom_vline(xintercept = 2000, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.8) +
  facet_wrap(~model) +
  scale_color_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_fill_manual(values = c("DENV1" = "steelblue", "DENV2" = "tomato")) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010)) +
  labs(
    x = "Time (year)",
    y = "Force of Infection (λ)",
    color = "Serotype",
    fill = "Serotype",
    title = "Estimated Force of Infection with 95% CrI\n(Time-Varying Models, Peru 2010)"
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )+
  theme_bw()


p_combined_peru

p_combined_peru <- ggplot(df_all) +
  # CrI bands
  geom_rect(aes(xmin = start, xmax = end, ymin = lower, ymax = upper, fill = serotype),
            alpha = 0.2, color = NA) +
  # Step segments
  geom_segment(aes(x = start, xend = end, y = mean, yend = mean, color = serotype), size = 0.5) +
  # Add FOI values as labels
  geom_text(aes(x = x_label, y = y_label, label = label, color = serotype),
            size = 3.5, show.legend = FALSE) +
  # Vertical reference lines
  geom_vline(xintercept = 2000, linetype = "dashed", color = "red", size = 0.8) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", size = 0.8) +
  facet_wrap(~model) +
  scale_color_manual(
    values = c("DENV1" = "steelblue", "DENV2" = "tomato"),
    labels = c(DENV1 = expression(lambda[1]), DENV2 = expression(lambda[2]))
  ) +
  scale_fill_manual(
    values = c("DENV1" = "steelblue", "DENV2" = "tomato"),
    labels = c(DENV1 = expression(lambda[1]), DENV2 = expression(lambda[2]))
  ) +
  scale_x_continuous(breaks = c(1990, 1995, 2000, 2005, 2010)) +
  labs(
    x = "Time (year)",
    y = expression("Force of Infection (" * lambda * ")"),
    color = "Serotype",
    fill = "Serotype",
    title = "Estimated Force of Infection with 95% CrI\n(Time-Varying Models, Peru 2010)"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

p_combined_peru

### Sigma from time-varying FOI models

fits_peru_time_varying_inter <- list(
  `2010` = fit_peru_2010_time_varying
)

#fits_peru_time_varying_inter <- list(
#  `2010` = fit_peru_2010_time_varying,
#  `2008` = fit_peru_2008_time_varying,
#  `2006` = fit_peru_2006_time_varying,
#  `2004` = fit_peru_2004_time_varying,
#  `2002` = fit_peru_2002_time_varying,
#  `2001` = fit_peru_2001_time_varying,
#  `1999` = fit_peru_1999,
#  `1996` = fit_peru_1996,
#  `1995` = fit_peru_1995,
#  `1994` = fit_peru_1994,
#  `1993` = fit_peru_1993
#)

summary_sigma_peru_new <- imap_dfr(fits_peru_time_varying_inter, extract_sigma_peru_summary) %>% 
  mutate(parameter = as.character(parameter),
         parameter_label = recode(parameter,
                                  "sigma12" = "sigma[12]",
                                  "sigma21" = "sigma[21]"))

summary_sigma_peru_new <- summary_sigma_peru_new %>%
  mutate(label_x = year + ifelse(parameter == "sigma12", -0.06, 0.06),
         label_text = sprintf("%.1f", mean))  # Format to 1 decimal place


g_sigma_peru_new <- ggplot(summary_sigma_peru_new , aes(x = year, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), 
                position = position_dodge(width = 0.2), width = 0.02) +
  #geom_line(aes(group = parameter_label), 
  #          linetype = "dotted", position = position_dodge(width = 0.3)) +
  scale_color_manual(
    name = "Interaction\nparameter",
    values = c("sigma[12]" = "#1b9e77", "sigma[21]" = "#d95f02"),
    labels = c(expression(sigma[12]), expression(sigma[21]))
  ) +
  scale_x_continuous(breaks = unique(summary_sigma_peru_new$year)) +
  geom_text(aes(x = label_x, label = label_text), 
            vjust = 0, size = 5, show.legend = FALSE) +  # ⬅ Add labels
  labs(
    title = expression("Interaction parameter (" * sigma * ")"),
    x = "Year", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14, angle = 15, vjust = 0.8, hjust = 0.8),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.direction = "vertical",  # stacked
  )

g_sigma_peru_new





### Check best cut-off year for Peru 2008

current_year <- 2004
cutoff_years <- seq(2004, 1980, by = -2)


loo_list <- list()

# --- constant FOI models ---
log_lik_1 <- extract_log_lik(fit_peru_2004, parameter_name = "log_lik")
log_lik_3 <- extract_log_lik(fit_peru_2004_no_inter, parameter_name = "log_lik")

loo_list[["Constant_FOI_with_interaction"]] <- loo(log_lik_1)
loo_list[["Constant_FOI_no_interaction"]] <- loo(log_lik_3)

# --- time-varying FOI model ---
for (cutoff_year in cutoff_years) {
  message("Running for cutoff year: ", cutoff_year)
  
  cutoff_age <- current_year - cutoff_year
  stan_data <- list(
    N = nrow(peru_2004),
    age = peru_2004$age,
    n_tested = peru_2004$n_tested,
    y = as.matrix(peru_2004[, c("n_s", "n_denv1", "n_denv2", "n_denv12")]),
    age_fine = age_fine,
    n_age_fine = length(age_fine),
    cutoff_age = cutoff_age
  )
  
  fit <- sampling(
    stan_two_piece_wise_model,
    data = stan_data,
    iter = 4000, chains = 4,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    refresh = 0
  )
  
  log_lik_matrix <- extract_log_lik(fit, parameter_name = "log_lik")
  loo_result <- loo(log_lik_matrix)
  
  model_name <- paste0("TimeVarying_Cutoff_", cutoff_year)
  loo_list[[model_name]] <- loo_result
}


elpd_values_peru_2004 <- sapply(loo_list, function(x) x$estimates["elpd_loo", "Estimate"])
se_values_peru_2004   <- sapply(loo_list, function(x) x$estimates["elpd_loo", "SE"])

ref_model <- "Constant_FOI_with_interaction"


elpd_diff_peru_2004 <- elpd_values_peru_2004 - elpd_values_peru_2004[ref_model]
se_diff_peru_2004  <- sqrt(se_values_peru_2004^2 + se_values_peru_2004[ref_model]^2)

elpd_table_peru_2004 <- tibble(
  Model = names(loo_list),
  elpd_loo = elpd_values_peru_2004,
  se_elpd_loo = se_values_peru_2004,
  elpd_diff = elpd_diff_peru_2004,
  se_diff = se_diff_peru_2004,
  is_reference = names(loo_list) == ref_model,
  is_no_interaction = names(loo_list) == "Constant_FOI_no_interaction",
  is_significantly_better = elpd_diff> se_diff,
  is_best = elpd_diff == max(elpd_diff),
  is_worst = elpd_diff == min(elpd_diff)
)

elpd_table_peru_2004 <- elpd_table_peru_2004 %>%
  mutate(
    cutoff_year = as.integer(str_extract(Model, "\\d{4}")),
    shape_type = case_when(
      is_reference ~ "Reference",
      is_best ~ "Best",
      is_worst ~ "Worst",
      TRUE ~ "Other"
    )
  )


ordered_models_peru_2004 <- c(
  "Constant_FOI_with_interaction",
  "Constant_FOI_no_interaction",
  elpd_table_peru_2004 %>%
    filter(str_detect(Model, "TimeVarying_Cutoff_")) %>%
    arrange(desc(cutoff_year)) %>%
    pull(Model)
)

elpd_table_peru_2004 <- elpd_table_peru_2004 %>%
  mutate(Model = factor(Model, levels = rev(ordered_models)))


shape_values <- c("Reference" = 17, "Best" = 16, "Worst" = 16, "Other" = 16)  # 17 = triangle, 16 = circle
color_values <- c("Reference" = "black", "Best" = "blue", "Worst" = "red" ,"Other" = "black")


ggplot(elpd_table_peru_2004, aes(x = elpd_diff, y = Model)) +
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
  xlim(min(elpd_table_peru_2004$elpd_diff - elpd_table_peru_2004$se_diff) - 5,
       max(elpd_table_peru_2004$elpd_diff + elpd_table_peru_2004$se_diff) + 25) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


#### PPCs of 4 model fits to Peru 2010 ----
ppc_model_1 <- make_ppc_plot(data_name = "Peru 2010", data=peru_2010, fit=fit_peru_2010_no_inter) +
  labs(title = "Model 1 - Constant FOI with no interaction", x = NULL) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))


ppc_model_2 <- make_ppc_plot(data_name = "Peru 2010", data=peru_2010, fit=fit_peru_2010) +
  labs(title = "Model 2 - Constant FOI with interaction", y = NULL, x = NULL)+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))

ppc_model_3 <- make_ppc_plot(data_name = "Peru 2008", data=peru_2010, fit=fit_peru_2010_time_varying_no_inter) +
  labs(title = "Model 3 - Time-varying FOI with no interaction")+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))

ppc_model_4 <- make_ppc_plot(data_name = "Peru 2008", data=peru_2010, fit=fit_peru_2010_time_varying) +
  labs(title = "Model 4 - Time-varying FOI with interaction", y = NULL)+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.title.position = "plot",
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))

(ppc_model_1 | ppc_model_2) /
  (ppc_model_3 | ppc_model_4)+
  plot_layout(guides = "collect")  & theme(plot.margin = margin(3, 3, 3, 3))

### MODEL COMPARISON - LOO (CONSTANT FOI MODELS)
loo(extract_log_lik(fit_peru_2010_no_inter))
loo(extract_log_lik(fit_peru_2010))
loo_compare(loo(extract_log_lik(fit_peru_2010_no_inter)), loo(extract_log_lik(fit_peru_2010)))

loo(extract_log_lik(fit_peru_2008_no_inter))
loo(extract_log_lik(fit_peru_2008))
loo_compare(loo(extract_log_lik(fit_peru_2008_no_inter)), loo(extract_log_lik(fit_peru_2008)))

loo(extract_log_lik(fit_peru_2006_no_inter))
loo(extract_log_lik(fit_peru_2006))
loo_compare(loo(extract_log_lik(fit_peru_2006_no_inter)), loo(extract_log_lik(fit_peru_2006)))

loo(extract_log_lik(fit_peru_2004_no_inter))
loo(extract_log_lik(fit_peru_2004))
loo_compare(loo(extract_log_lik(fit_peru_2004_no_inter)), loo(extract_log_lik(fit_peru_2004)))

loo(extract_log_lik(fit_peru_2002_no_inter))
loo(extract_log_lik(fit_peru_2002))
loo_compare(loo(extract_log_lik(fit_peru_2002_no_inter)), loo(extract_log_lik(fit_peru_2002)))

loo(extract_log_lik(fit_peru_2001_no_inter))
loo(extract_log_lik(fit_peru_2001))
loo_compare(loo(extract_log_lik(fit_peru_2001_no_inter)), loo(extract_log_lik(fit_peru_2001)))

loo(extract_log_lik(fit_peru_1999_no_inter))
loo(extract_log_lik(fit_peru_1999))
loo_compare(loo(extract_log_lik(fit_peru_1999_no_inter)), loo(extract_log_lik(fit_peru_1999)))

loo(extract_log_lik(fit_peru_1996_no_inter))
loo(extract_log_lik(fit_peru_1996))
loo_compare(loo(extract_log_lik(fit_peru_1996_no_inter)), loo(extract_log_lik(fit_peru_1996)))

loo(extract_log_lik(fit_peru_1995_no_inter))
loo(extract_log_lik(fit_peru_1995))
loo_compare(loo(extract_log_lik(fit_peru_1995_no_inter)), loo(extract_log_lik(fit_peru_1995)))

loo(extract_log_lik(fit_peru_1994_no_inter))
loo(extract_log_lik(fit_peru_1994))
loo_compare(loo(extract_log_lik(fit_peru_1994_no_inter)), loo(extract_log_lik(fit_peru_1994)))

loo(extract_log_lik(fit_peru_1993_no_inter))
loo(extract_log_lik(fit_peru_1993))
loo_compare(loo(extract_log_lik(fit_peru_1993_no_inter)), loo(extract_log_lik(fit_peru_1993)))


