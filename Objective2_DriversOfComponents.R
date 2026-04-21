library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)
library(car)
library(lmtest)  # for bptest
library(ggpubr)  # for stat_cor

#RESILIENCE- DISTURBED FIRST AND CONTROL SECOND
# Step 1: Load data
disturbed_file_path <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"  
control_file_path <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"

df_disturbed <- read_csv(disturbed_file_path) %>%
  filter(TIMESTAMP >= 2009 & TIMESTAMP <= 2021) %>%
  mutate(
    NEP_dist = -NEE_VUT_REF,
    RECO_dist = RECO_NT_VUT_REF,
    GPP_dist = GPP_NT_VUT_REF
  ) %>%
  select(TIMESTAMP, NEP_dist, RECO_dist, GPP_dist)

df_control <- read_csv(control_file_path) %>%
  filter(TIMESTAMP >= 2009 & TIMESTAMP <= 2021) %>%
  mutate(
    NEP_con = -NEE_VUT_REF,
    RECO_con = RECO_NT_VUT_REF,
    GPP_con = GPP_NT_VUT_REF
  ) %>%
  select(TIMESTAMP, NEP_con, RECO_con, GPP_con)

# Step 2: Combine and create log ratios
df_combined <- inner_join(df_disturbed, df_control, by = "TIMESTAMP") %>%
  mutate(
    ln_ratio_NEP = log(NEP_dist / NEP_con),
    ln_ratio_RECO = log(RECO_dist / RECO_con),
    ln_ratio_GPP = log(GPP_dist / GPP_con),
    t = TIMESTAMP - 2009
  )

# Fit OLS linear models for resilience (log ratios ~ time)
ols_NEP <- lm(ln_ratio_NEP ~ t, data = df_combined)
ols_RECO <- lm(ln_ratio_RECO ~ t, data = df_combined)
ols_GPP <- lm(ln_ratio_GPP ~ t, data = df_combined)

# Resilience driver models
driver_model_GPP <- lm(ln_ratio_NEP ~ ln_ratio_GPP, data = df_combined)
driver_model_RECO <- lm(ln_ratio_NEP ~ ln_ratio_RECO, data = df_combined)


# Extract residuals for later use
df_residuals <- df_combined %>%
  mutate(
    res_NEP = resid(ols_NEP),
    res_GPP = resid(ols_GPP),
    res_RECO = resid(ols_RECO)
  )

# Temporal stability driver models (residuals)
residual_driver_GPP <- lm(res_NEP ~ res_GPP, data = df_residuals)
residual_driver_RECO <- lm(res_NEP ~ res_RECO, data = df_residuals)

summary(driver_model_GPP)   # ln(NEP) ~ ln(GPP)
summary(driver_model_RECO)  # ln(NEP) ~ ln(RECO)
summary(residual_driver_GPP)   # res(NEP) ~ res(GPP)
summary(residual_driver_RECO)  # res(NEP) ~ res(RECO)


#Check OLS slope test assumptions
check_assumptions <- function(model) {
  par(mfrow = c(2, 2))
  plot(model)  # Basic diagnostic plots
  print(dwtest(model))
  print(bptest(model))
  print(shapiro.test(residuals(model)))
  par(mfrow = c(1, 1))
}

# Example
check_assumptions(ols_NEP)
check_assumptions(ols_RECO)
check_assumptions(ols_GPP)
check_assumptions(driver_model_GPP)
check_assumptions(driver_model_RECO)
check_assumptions(residual_driver_RECO)
check_assumptions(residual_driver_GPP)

# R² for ln-ratio plots
r2_ln_GPP <- summary(driver_model_GPP)$r.squared
label_r2_ln_GPP <- paste0("R² = ", round(r2_ln_GPP, 3))

r2_ln_RECO <- summary(driver_model_RECO)$r.squared
label_r2_ln_RECO <- paste0("R² = ", round(r2_ln_RECO, 3))

# R² for residual plots
r2_res_GPP <- summary(residual_driver_GPP)$r.squared
label_r2_res_GPP <- paste0("R² = ", round(r2_res_GPP, 3))

r2_res_RECO <- summary(residual_driver_RECO)$r.squared
label_r2_res_RECO <- paste0("R² = ", round(r2_res_RECO, 3))


plot_ln_NEP_vs_ln_GPP <- ggplot(df_combined, aes(x = ln_ratio_GPP, y = ln_ratio_NEP)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", label.x = min(df_combined$ln_ratio_GPP),
           label.y = max(df_combined$ln_ratio_NEP),
           size = 5, color = "black") +
  annotate("text", x = min(df_combined$ln_ratio_GPP),
           y = max(df_combined$ln_ratio_NEP) - 0.05,
           label = label_r2_ln_GPP, hjust = 0, size = 5) +
  labs(
    title = "ln(NEP Dist / Control) vs ln(GPP Dist / Control)",
    x = "ln(GPPd / GPPc)",
    y = "ln(NEPd / NEPc)"
  ) +
  theme_minimal(base_size = 14)

plot_ln_NEP_vs_ln_RECO <- ggplot(df_combined, aes(x = ln_ratio_RECO, y = ln_ratio_NEP)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", label.x = min(df_combined$ln_ratio_RECO),
           label.y = max(df_combined$ln_ratio_NEP),
           size = 5, color = "black") +
  annotate("text", x = min(df_combined$ln_ratio_RECO),
           y = max(df_combined$ln_ratio_NEP) - 0.05,
           label = label_r2_ln_RECO, hjust = 0, size = 5) +
  labs(
    title = "ln(NEP Dist / Control) vs ln(RECO Dist / Control)",
    x = "ln(RECOd / RECOc)",
    y = "ln(NEPd / NEPc)"
  ) +
  theme_minimal(base_size = 14)



correlation_panel <- plot_ln_NEP_vs_ln_GPP + plot_ln_NEP_vs_ln_RECO

plot_residuals_NEP_vs_GPP <- ggplot(df_residuals, aes(x = res_GPP, y = res_NEP)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", label.x = min(df_residuals$res_GPP), 
           label.y = max(df_residuals$res_NEP), size = 5) +
  annotate("text", x = min(df_residuals$res_GPP), 
           y = max(df_residuals$res_NEP) - 0.05,
           label = label_r2_res_GPP, hjust = 0, size = 5) +
  labs(
    title = "Residuals: ln(NEP Dist/Control) vs ln(GPP Dist/Control)",
    x = "Residual ln(GPPd/GPPc)",
    y = "Residual ln(NEPd/NEPc)"
  ) +
  theme_minimal(base_size = 14)


plot_residuals_NEP_vs_RECO <- ggplot(df_residuals, aes(x = res_RECO, y = res_NEP)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  stat_cor(method = "pearson", label.x = min(df_residuals$res_RECO), 
           label.y = max(df_residuals$res_NEP), size = 5) +
  annotate("text", x = min(df_residuals$res_RECO), 
           y = max(df_residuals$res_NEP) - 0.05,
           label = label_r2_res_RECO, hjust = 0, size = 5) +
  labs(
    title = "Residuals: ln(NEP Dist/Control) vs ln(RECO Dist/Control)",
    x = "Residual ln(RECOd/RECOc)",
    y = "Residual ln(NEPd/NEPc)"
  ) +
  theme_minimal(base_size = 14)


residuals_correlation_panel <- plot_residuals_NEP_vs_GPP + plot_residuals_NEP_vs_RECO

# Combine panels side by side with spacing and margins
combined_panels <- correlation_panel | residuals_correlation_panel + 
  plot_layout(widths = c(1, 1), guides = "collect") & 
  theme(plot.margin = margin(15, 15, 15, 15))

print(correlation_panel/residuals_correlation_panel)
