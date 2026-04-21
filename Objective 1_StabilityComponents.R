library(dplyr)
library(ggplot2)
library(readr)
library(patchwork)
library(car)
library(lmtest)  # for bptest

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

# Print OLS summaries for slope, SE, p-value (same style as GLS)
print_ols_summary <- function(model, var_name) {
  coefs <- summary(model)$coefficients
  slope <- coefs["t", "Estimate"]
  se <- coefs["t", "Std. Error"]
  pval <- coefs["t", "Pr(>|t|)"]
  lower_ci <- slope - 1.96 * se
  upper_ci <- slope + 1.96 * se
  cat(paste0(var_name, " OLS slope (t): ", round(slope, 4), "\n"))
  cat(paste0("95% CI: ", round(lower_ci, 4), " to ", round(upper_ci, 4), "\n"))
  cat(paste0("p-value: ", signif(pval, 4), "\n\n"))
}

print_ols_summary(ols_NEP, "NEP")
print_ols_summary(ols_RECO, "RECO")
print_ols_summary(ols_GPP, "GPP")

#TEMPORAL STABILITY
calculate_ols_stability <- function(model) {
  residuals <- resid(model)
  stability <- 1 / sd(residuals)
  return(stability)
}

stability_NEP_ols <- calculate_ols_stability(ols_NEP)
stability_RECO_ols <- calculate_ols_stability(ols_RECO)
stability_GPP_ols <- calculate_ols_stability(ols_GPP)

print(stability_NEP_ols)
print(stability_RECO_ols)
print(stability_GPP_ols)

# Plot residuals vs year to visualize temporal stability
plot_residuals_vs_year <- function(model, year_vector, variable_name) {
  data <- data.frame(
    year = year_vector,
    residuals = resid(model)
  )
  
  ggplot(data, aes(x = year, y = residuals)) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "solid") +
    labs(
      title = paste("Residuals over Time:", variable_name),
      x = "Year",
      y = "Residuals"
    ) +
    theme_minimal()
}

# Use TIMESTAMP as the year vector
year_vector <- df_combined$TIMESTAMP

# Generate plots
p1 <- plot_residuals_vs_year(ols_NEP, year_vector, "NEP")
p2 <- plot_residuals_vs_year(ols_RECO, year_vector, "RECO")
p3 <- plot_residuals_vs_year(ols_GPP, year_vector, "GPP")

# Display all three side by side using patchwork
p1 + p2 + p3 + plot_layout(ncol = 1)


# Resilience plots for NEP, GPP, and RECO
library(ggplot2)

plot_resilience <- function(df, y_var, fitted_model, color, title_text) {
  ggplot(df, aes(x = t, y = .data[[y_var]])) +
    geom_point(color = color) +
    geom_smooth(method = "lm", se = FALSE, color = color, size = 1) +
    labs(
      title = title_text,
      x = "Years Since Disturbance",
      y = "ln(Fd/Fc)"
    ) +
    theme_minimal(base_size = 14)
}

plot_NEP <- plot_resilience(df_combined, "ln_ratio_NEP", ols_NEP, "blue", "Resilience of NEP")
plot_RECO <- plot_resilience(df_combined, "ln_ratio_RECO", ols_RECO, "red", "Resilience of RECO")
plot_GPP <- plot_resilience(df_combined, "ln_ratio_GPP", ols_GPP, "darkgreen", "Resilience of GPP")

# Combine them vertically using patchwork
library(patchwork)
final_resilience_plot <- plot_NEP / plot_RECO / plot_GPP

# Prepare temporal stability dataframe for plotting
temporal_stability_df <- data.frame(
  Variable = c("NEP", "RECO", "GPP"),
  Temporal_Stability = c(stability_NEP_ols, stability_RECO_ols, stability_GPP_ols)
)

# Temporal stability bar plot (standalone)
plot_stability <- ggplot(temporal_stability_df, aes(x = Variable, y = Temporal_Stability, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = round(Temporal_Stability, 2)), vjust = -0.5, size = 5, fontface = "bold") +
  labs(
    title = "Temporal Stability of Flux Variables",
    x = "Flux Variable",
    y = "Temporal Stability"
  ) +
  scale_fill_manual(values = c("NEP" = "blue", "RECO" = "red", "GPP" = "darkgreen")) +
  theme_minimal(base_size = 16) +
  ylim(0, max(temporal_stability_df$Temporal_Stability) * 1.2)

# Print the combined plot
  print(final_resilience_plot/plot_stability)



