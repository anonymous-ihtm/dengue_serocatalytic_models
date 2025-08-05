# -----------------------------------------
# MULTI-PANEL PLOTS - USE INPUTS FROM OTHER R FILES ----
# -----------------------------------------

library(patchwork)
library(tibble)
library(tidyverse)
library(rstan)
library(ggtext)
library(gridExtra)
library(grid)

### FOI

g_lambda_peru <- g_lambda_peru +
  labs(title = "(A) Peru", y = NULL) +
  coord_cartesian(ylim = c(0, 0.21)) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"))


g_lambda_cuba <- g_lambda_cuba +
  #labs(title = "(B) Cuba") +
  labs(title = "(B) Cuba", y = NULL) +
  #coord_cartesian(ylim = c(0, 0.02)) +
  theme_bw() +
  theme(
        axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank(),
        #strip.background = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 12, angle = 30, vjust = 0.8, hjust = 0.8),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 16),
        legend.position = "none")

g_lambda_vietnam <- g_lambda_vietnam +
  labs(title = "(C) Vietnam", y = expression("Force of Infection (" * lambda * ")")) +
  #coord_cartesian(ylim = c(0, 0.04)) +
  theme(
        #axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank(),
        #strip.background = element_blank(),
        legend.title = element_blank(),
        #legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 12, angle = 30, vjust = 0.8, hjust = 0.8),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 16),
        legend.position = c(0.90, 0.05)) #+ facet_grid(model ~ ., switch = "y")

g_lambda_lao <- g_lambda_lao +
  #labs(title = "(D) Laos") +
  labs(title = "(D) Laos", y = NULL) +
  #coord_cartesian(ylim = c(0, 0.008)) +
  theme_bw() +
  theme(
        axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        strip.text.y = element_blank(),
        #strip.background = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 12, angle = 30, vjust = 0.8, hjust = 0.8),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 16),
        legend.position = "none")



final_lambda_plot <- (g_lambda_peru | g_lambda_cuba) /
  (g_lambda_vietnam | g_lambda_lao) +
  plot_layout(guides = "collect", 
              widths = c(1, 1),    
              heights = c(1, 1),   
              design = NULL) & 
  theme(legend.position = 'bottom',
        plot.margin = margin(5, 5, 5, 5))

final_lambda_plot <- wrap_plots(
  list(g_lambda_peru, g_lambda_cuba, g_lambda_vietnam, g_lambda_lao),
  ncol = 2
) +
  plot_layout(guides = "collect") &  # gộp tất cả legend
  theme(legend.position = "bottom",
        plot.margin = margin(5, 5, 5, 5))


final_lambda_plot

(g_lambda_peru | g_lambda_cuba) /
  (g_lambda_vietnam | g_lambda_lao)+
  plot_layout(guides = "collect")  & theme(plot.margin = margin(3, 3, 3, 3))

p_combined_peru <- p_combined_peru +
  labs(title = "(A) Peru 2010", y = expression("Force of Infection (" * lambda * ")")) +
  coord_cartesian(ylim = c(0, 0.10)) +
  theme(
    #axis.title.y = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = c(0.90, 0.05),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    axis.text.x = element_text(size = 12, angle = 30, vjust = 0.8, hjust = 0.8),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 16)
  ) #+ facet_grid(model ~ ., switch = "y")

(p_combined_peru | g_lambda_cuba) /
  (g_lambda_vietnam | g_lambda_lao)+
  plot_layout(guides = "collect")  & theme(plot.margin = margin(3, 3, 3, 3))

### SIGMA

g_sigma_peru_new <- g_sigma_peru_new +
  labs(title = "(A) Peru*", y = NULL) +
  #coord_cartesian(ylim = c(0, 0.21)) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold")) +
  geom_hline(
    aes(yintercept = 1),
    linetype = "dashed",
    color = "red"
  )


g_sigma_cuba <- g_sigma_cuba +
  labs(title = "(B) Cuba", y = NULL) +
  #coord_cartesian(ylim = c(0, 0.02)) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  geom_hline(
    aes(yintercept = 1),
    linetype = "dashed",
    color = "red"
  )

g_sigma_vietnam <- g_sigma_vietnam +
  labs(title = "(C) Vietnam", y = NULL) +
  #coord_cartesian(ylim = c(0, 0.04)) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  geom_hline(
    aes(yintercept = 1),
    linetype = "dashed",
    color = "red"
  )

g_sigma_lao <- g_sigma_lao +
  labs(title = "(D) Laos", y = NULL) +
  #coord_cartesian(ylim = c(0, 0.006)) +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "none") +
  geom_hline(
    aes(yintercept = 1),
    linetype = "dashed",
    color = "red"
  )

final_sigma_plot <- (g_sigma_peru_new | g_sigma_cuba) /
  (g_sigma_vietnam | g_sigma_lao)+
  plot_layout(guides = "collect")  & theme(plot.margin = margin(3, 3, 3, 3))

final_sigma_plot

#### NUMBER of INFECTIONS
# FOI estimates
lambda_df <- tibble(
  Location = c("Total", "Ho Chi Minh City", "Khanh Hoa Province"),
  lambda1 = c(0.007, 0.006, 0.009),
  lambda2 = c(0.017, 0.014, 0.023)
)


# Age vector
age <- 0:80

# Compute expected infections for each location and serotype
expected_df <- lambda_df %>%
  rowwise() %>%
  mutate(
    denv1 = list(lambda1 * age),
    denv2 = list(lambda2 * age),
    total = list((lambda1 + lambda2) * age)
  ) %>%
  unnest(cols = c(denv1, denv2, total)) %>%
  mutate(age = rep(age, times = nrow(lambda_df)))

# Pivot longer for ggplot
plot_df <- expected_df %>%
  pivot_longer(cols = c(denv1, denv2, total),
               names_to = "serotype", values_to = "expected")

# Set labels
serotype_labels <- c(denv1 = "DENV1", denv2 = "DENV2", total = "Total")

# Create individual plots
annots <- plot_df %>% 
  filter(age == 74)

p1 <- ggplot(filter(plot_df, serotype == "denv1"),
             aes(x = age, y = expected, color = Location)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 74, linetype = "dashed", color = "black") +
  geom_text(data = filter(annots, serotype == "denv1"),
            aes(label = round(expected, 2)),
            hjust = -0.2, vjust = 1, size = 4, show.legend = FALSE) +
  labs(title = "(C-1) DENV1",
       x = "Age (years)", y = "Expected number of infections") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_x_continuous(
    breaks = c(seq(0, 80, by = 20), 74),
    labels = sapply(c(seq(0, 80, by = 20), 74), function(x) {
      if (x == 74) paste0("**", x, "**") else as.character(x)
    })
  ) +
  theme(axis.text.x = element_markdown(size = 12))

p2 <- ggplot(filter(plot_df, serotype == "denv2"),
             aes(x = age, y = expected, color = Location)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 74, linetype = "dashed", color = "black") +
  geom_text(data = filter(annots, serotype == "denv2"),
            aes(label = round(expected, 2)),
            hjust = -0.2, vjust = 1, size = 4, show.legend = FALSE) +
  labs(title = "(C-2) DENV2",
       x = "Age (years)", y = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_x_continuous(
    breaks = c(seq(0, 80, by = 20), 74),
    labels = sapply(c(seq(0, 80, by = 20), 74), function(x) {
      if (x == 74) paste0("**", x, "**") else as.character(x)
    })
  ) +
  theme(axis.text.x = element_markdown(size = 12))

p3 <- ggplot(filter(plot_df, serotype == "total"),
             aes(x = age, y = expected, color = Location)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 74, linetype = "dashed", color = "black") +
  geom_text(data = filter(annots, serotype == "total"),
            aes(label = round(expected, 2)),
            hjust = -0.2, vjust = 1, size = 4, show.legend = FALSE) +
  labs(title = "(C-3) Total",
       x = "Age (years)", y = NULL) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_x_continuous(
    breaks = c(seq(0, 80, by = 20), 74),
    labels = sapply(c(seq(0, 80, by = 20), 74), function(x) {
      if (x == 74) paste0("**", x, "**") else as.character(x)
    })
  ) +
  theme(axis.text.x = element_markdown(size = 12))

p4 <- ggplot(summary_lambda_vietnam_inter, aes(x = location, y = mean, color = parameter_label)) +
  geom_line(linetype = "dotted") +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = q025, ymax = q975), position = position_dodge(width = 0.3), width = 0.3) +
  geom_text(
    data = subset(summary_lambda_vietnam_inter, parameter_label == "lambda[1]"),
    aes(label = round(mean, 3)),
    position = position_nudge(x = -0.3, y = 0),
    vjust = 0,
    size = 5,
    show.legend = FALSE
  ) +
  geom_text(
    data = subset(summary_lambda_vietnam_inter, parameter_label == "lambda[2]"),
    aes(label = round(mean, 3)),
    position = position_nudge(x = 0.3, y = 0),
    vjust = 0,
    size = 5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Parameter",
    values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
    labels = c(expression(lambda[1]), expression(lambda[2]))
  ) +
  labs(
    title = "(A) Posterior Mean and 95% CrI of FOI\n     (Constant FOI with Interaction Model)",
    x = "Location", y = "Force of Infection"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16)
  )

p4

# Define layout
layout <- c(
  area(t = 1, l = 2, b = 1, r = 2),   # p4 ở giữa
  area(t = 2, l = 1, b = 2, r = 1),   # p1 bên trái
  area(t = 2, l = 2, b = 2, r = 2),   # p2 giữa
  area(t = 2, l = 3, b = 2, r = 3)    # p3 bên phải
)

# Combine plots
(p4 + p1 + p2 + p3) + plot_layout(design = layout)

##### COMBINE PPC
g_PPC_sim_constant_inter <- g_PPC_sim_constant_inter +
  labs(title = "(A) Constant FOI model with interactions") +
  theme(plot.title = element_markdown(size = 20, face = "bold"))

g_PPC_sim_time_vary_inter <- g_PPC_sim_time_vary_inter +
  labs(title = "(B) Time-varying FOI model with interactions", y = NULL) +
  theme(plot.title = element_markdown(size = 20, face = "bold"))

g_PPC_sim_constant_inter|g_PPC_sim_time_vary_inter
####
# Extract function
extract_summary_lambda <- function(fit, location) {
  post <- rstan::extract(fit, pars = c("lambda1", "lambda2"))
  lambda_total <- post$lambda1 + post$lambda2
  tibble(
    location = location,
    lambda1_mean = mean(post$lambda1),
    lambda1_q025 = quantile(post$lambda1, 0.025),
    lambda1_q975 = quantile(post$lambda1, 0.975),
    lambda2_mean = mean(post$lambda2),
    lambda2_q025 = quantile(post$lambda2, 0.025),
    lambda2_q975 = quantile(post$lambda2, 0.975),
    lambda_total_mean = mean(lambda_total),
    lambda_total_q025 = quantile(lambda_total, 0.025),
    lambda_total_q975 = quantile(lambda_total, 0.975)
  )
}

# Combine results from all three locations
lambda_summary <- bind_rows(
  extract_summary_lambda(fit_Vietnam_total, "Total"),
  extract_summary_lambda(fit_Vietnam_HCM, "HCMC"),
  extract_summary_lambda(fit_Vietnam_KH, "Khanh Hoa")
)

# Define age range
age <- 0:80

# Generate expected number of infections
expected_df <- lambda_summary %>%
  rowwise() %>%
  mutate(
    denv1 = list(tibble(
      age = age,
      mean = lambda1_mean * age,
      lower = lambda1_q025 * age,
      upper = lambda1_q975 * age
    )),
    denv2 = list(tibble(
      age = age,
      mean = lambda2_mean * age,
      lower = lambda2_q025 * age,
      upper = lambda2_q975 * age
    )),
    total = list(tibble(
      age = age,
      mean = lambda_total_mean * age,
      lower = lambda_total_q025 * age,
      upper = lambda_total_q975 * age
    ))
  ) %>%
  select(location, denv1, denv2, total) %>%
  pivot_longer(cols = c(denv1, denv2, total), names_to = "serotype", values_to = "data") %>%
  unnest(cols = c(data)) %>%
  mutate(
    serotype = recode(serotype,
                   denv1 = "DENV1",
                   denv2 = "DENV2",
                   total = "Total")
  )

# Order for serotype panels
expected_df$serotype <- factor(expected_df$serotype, levels = c("DENV1", "DENV2", "Total"))

# Extract values at age 74 to label
label_df <- expected_df %>%
  filter(age == 74) %>%
  mutate(
    label = sprintf("%.2f", mean),
    x_pos = 80
  )


# Plot
p6 <- ggplot(expected_df, aes(x = age, y = mean, color = location, fill = location)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, color = NA) +
  geom_line(size = 1) +
  geom_text(
    data = label_df,
    aes(x = x_pos, y = mean, label = label, color = location),
    hjust = 0.9, vjust = 0.9, size = 3.5, show.legend = FALSE
  ) +
  facet_wrap(~ serotype, nrow = 1, labeller = labeller(
    serotype = c("DENV1" = "(C-1) DENV1", "DENV2" = "(C-2) DENV2", "Total" = "(C-3) Total")
  )) +
  geom_vline(xintercept = 74, linetype = "dashed", color = "black") +
  #coord_cartesian(ylim = c(0, 3)) +  # LIMIT Y AXIS
  labs(
    x = "Age (years)",
    y = "Expected number of infections",
    color = "Location", fill = "Location"
  ) +
  scale_x_continuous(
    breaks = c(seq(0, 80, 20), 74),
    labels = sapply(c(seq(0, 80, 20), 74), function(x) {
      if (x == 74) paste0("**", x, "**") else as.character(x)
    })
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "bottom",
    axis.text.x = element_markdown()
  )

p6

# Define new layout
layout <- c(
  area(t = 1, l = 1, b = 1, r = 1), 
  area(t = 1, l = 2, b = 1, r = 3),
  area(t = 2, l = 1, b = 3, r = 3)
)

# Combine and plot
(p4 + p5 + p6) + plot_layout(design = layout, heights = c(1, 0.5))

### NEW PLOT FOR AGE 50% OR 80% BECOME SEROPOSITIVE

#Extract posterior draws
extract_lambda <- function(fit, location) {
  draws <- rstan::extract(fit, pars = c("lambda1", "lambda2"))
  tibble(
    location = location,
    lambda_total = draws$lambda1 + draws$lambda2
  )
}

#Combine fits from all sites
lambda_df <- bind_rows(
  extract_lambda(fit_Vietnam_HCM, "HCMC"),
  extract_lambda(fit_Vietnam_KH, "Khanh Hoa"),
  extract_lambda(fit_Vietnam_total, "Total")
)

#Define target seroprevalence thresholds
p_vals <- c(0.5, 0.8)

#Calculate age at seroconversion (only for λ_total)
seroconv_df <- lambda_df %>%
  slice(rep(1:n(), each = length(p_vals))) %>%
  mutate(p = rep(p_vals, times = nrow(lambda_df))) %>%
  mutate(age_total = -log(1 - p) / lambda_total)

#Summarise posterior draws
summary_df <- seroconv_df %>%
  group_by(location, p) %>%
  summarise(
    mean = mean(age_total),
    q025 = quantile(age_total, 0.025),
    q975 = quantile(age_total, 0.975),
    .groups = "drop"
  )

#Plot (only for "Total" — i.e., at least 1 serotype)
summary_df <- summary_df %>%
  mutate(location_num = recode(location,
                               "HCMC" = 1,
                               "Khanh Hoa" = 2,
                               "Total" = 3))

p8 <- ggplot(summary_df, aes(x = location_num, y = mean, shape = as.factor(p))) +
  geom_point(position = position_dodge(width = 0.5), size = 3, color = "black") +
  geom_errorbar(aes(ymin = q025, ymax = q975),
                position = position_dodge(width = 0.5),
                width = 0.3,
                color = "black") +
  geom_text(aes(label = round(mean, 1)),
            position = position_dodge(width = 0.5),
            vjust = -0.2, hjust = -0.3, size = 5, show.legend = FALSE) +
  scale_x_continuous(
    name = "Location",
    breaks = c(1, 2, 3),
    labels = c("HCMC", "Khanh Hoa", "Total"),
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_shape_manual(
    name = "Seroconversion threshold",
    values = c(`0.5` = 16, `0.8` = 17),
    labels = c("p = 0.5", "p = 0.8")
  ) +
  labs(
    title = "(B) Age at which 50% and 80% of individuals\n      are seropositive to ≥ 1 serotype",
    y = "Age (years)"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank()
  )

p8

(p4 + p8)/(p1|p2|p3)

############------------#######

# STEP 1: Add region column to each dataset
combined_lambda_vietnam$region <- "Vietnam"
combined_lambda_cuba$region <- "Cuba"
combined_lambda_lao$region <- "Laos"

# Subset Peru into 2 groups with region label
df_all <- df_all %>%
  mutate(
    region = "Peru",
    location = "Peru",
    parameter_label = ifelse(serotype == "DENV1", "lambda[1]", "lambda[2]")  # for consistency
  )

# STEP 2: Standardize column names across datasets for binding
df_peru_plot <- df_all %>%
  select(model, region, location, parameter_label, mean, lower, upper)

df_vietnam_plot <- combined_lambda_vietnam %>%
  rename(lower = q025, upper = q975)

df_cuba_plot <- combined_lambda_cuba %>%
  rename(location = country, lower = q025, upper = q975)

df_lao_plot <- combined_lambda_lao %>%
  rename(lower = q025, upper = q975)

# STEP 3: Combine all data
df_combined_all <- bind_rows(df_peru_plot, df_vietnam_plot, df_cuba_plot, df_lao_plot)

# STEP 4: Plot with 2-row layout: No interaction (top), With interaction (bottom)
gg_combined <- ggplot(df_combined_all, aes(x = location, y = mean, color = parameter_label)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.3), width = 0.4) +
  scale_color_manual(
    name = "Force of Infection",
    values = c("lambda[1]" = "steelblue", "lambda[2]" = "tomato"),
    labels = c(expression(lambda[1]), expression(lambda[2]))
  ) +
  facet_grid(model ~ region, scales = "free_x", space = "free_x") +
  labs(
    title = "Estimated Force of Infection (λ) across Countries",
    x = "Location", y = "Estimate"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.text = element_text(size = 14),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 14)
  )

gg_combined
####
### PPC comparison across countries - under constant FOI model with interaction ----
ppc_peru_2010 <- ppc_peru_2010 +
  labs(title = "(A) Peru 2010", x = NULL) +
  theme(plot.title = element_markdown(size = 16, face = "bold"),
        plot.title.position = "plot",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  coord_cartesian(ylim = c(0.00, 1.00))
ppc_cuba <- ppc_cuba +
  labs(title = "(B) Cuba", x = NULL, y = NULL) +
  theme(plot.title = element_markdown(size = 16, face = "bold"),
        plot.title.position = "plot",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  coord_cartesian(ylim = c(0.00, 1.00))
ppc_vietnam <- ppc_vietnam +
  labs(title = "(C) Vietnam") +
  theme(plot.title = element_markdown(size = 16, face = "bold"),
        plot.title.position = "plot",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  coord_cartesian(ylim = c(0.00, 1.00))
ppc_laos <- ppc_laos +
  labs(title = "(D) Laos", y = NULL) +
  theme(plot.title = element_markdown(size = 16, face = "bold"),
        plot.title.position = "plot",
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        strip.text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  coord_cartesian(ylim = c(0.00, 1.00))
compare_ppc <- (ppc_peru_2010 | ppc_cuba) /
  (ppc_vietnam | ppc_laos) +
  plot_layout(guides = "collect")
compare_ppc

##### FOI plot
# Supplementary Figure: Seroprevalence vs. age when λ = 2 yr⁻¹
library(ggplot2)

# Parameters
lambda <- 2             # Force of infection (per person‑year)
age_seq <- seq(0, 10, 0.1)   # Age range in years

# Calculate seroprevalence
seroprev <- 1 - exp(-lambda * age_seq)

# Assemble data frame
df <- data.frame(
  age = age_seq,
  seroprevalence = seroprev
)

# Plot
ggplot(df, aes(x = age, y = seroprevalence)) +
  geom_line(size = 1.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = expression(paste(
      "Age-specific seroprevalence with constant FOI: ",
      lambda == 2, " yr"^{-1}
    )),
    x = "Age (years)",
    y = "Seroprevalence"
  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 18))

####

library(ggplot2)
library(dplyr)
library(viridis)

# Define age range and FOI values
age_seq <- seq(0, 60, by = 0.1)
lambda_values <- c(0.001, 0.01, 0.05, 0.1, 0.5, 1.0, 1.5, 2.0)

# Compute seroprevalence curves
df <- expand.grid(age = age_seq, lambda = lambda_values) %>%
  mutate(
    seroprevalence = 1 - exp(-lambda * age),
    lambda_label = paste0("λ = ", lambda)
  )

# Plot with viridis color palette
ggplot(df, aes(x = age, y = seroprevalence, color = lambda_label)) +
  geom_line(size = 1.3) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  #scale_color_viridis_d(name = "FOI (λ)", option = "D") +  # "D" is a clean option, "C" or "E" also good
  scale_color_brewer(palette = "Set2", name = "FOI (λ)") +
  labs(
    title = "Age-specific seroprevalence under different FOI values",
    subtitle = expression(P(a) == 1 - e^{-λ * a}),
    x = "Age (years)",
    y = "Seroprevalence"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
