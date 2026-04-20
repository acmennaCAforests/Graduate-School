#Objective 0 - making a time series from YY data into a yearly time series and MM error bars for NEE, GPP and Reco

# THIS GRAPH IS FOR NEP IN THE CONTROL FOREST

library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(zoo)
library(patchwork)

file_path <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"
df_annual <- read_csv(file_path)

file_path1 <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_MM_2007-2021_3-5.csv"
df_monthly <- read_csv(file_path1, col_types = cols(
  TIMESTAMP = col_datetime(format = "%Y%m")
))

df_monthly <- df_monthly %>%
  mutate(Year = year(TIMESTAMP), Month = month(TIMESTAMP))

df_monthly <- df_monthly %>%
  filter(NEE_VUT_REF != -9999)


# Create df_monthly by grouping by Year and Month
df_monthly <- df_monthly %>%
  group_by(Year) %>%
  mutate(
    NEE_VUT_REF_SD = sd(NEE_VUT_REF, na.rm = TRUE),  # Calculate SD of monthly values for each year
    NEE_VUT_SE_calculated = NEE_VUT_REF_SD / sqrt(12)  # Calculate SE from SD
  ) %>%
  ungroup()

df_monthly <- df_monthly %>%
  mutate(NEE_VUT_SE_yearly = NEE_VUT_SE_calculated * (365 / 30))  # Adjust to yearly

# Join with the correct column for SE
df_annual <- df_annual %>%
  left_join(df_monthly %>% select(Year, NEE_VUT_SE_yearly), 
            by = c("TIMESTAMP" = "Year"))

# Filter df_annual for years >= 2008
df_annual_filtered <- df_annual %>% filter(TIMESTAMP >= 2008)
df_annual_filtered$NEE_VUT_REF <- -df_annual_filtered$NEE_VUT_REF


# Create the plot for NEE in the control forest with monthly SE
plotNEPcontrol <- ggplot(df_annual_filtered, aes(x = TIMESTAMP, y = NEE_VUT_REF)) +
  geom_line(color = "blue") +  
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = NEE_VUT_REF - NEE_VUT_SE_yearly, 
                    ymax = NEE_VUT_REF + NEE_VUT_SE_yearly), 
                width = 0.3, color = "black") +
  geom_ribbon(aes(ymin = NEE_VUT_REF - NEE_VUT_SE_yearly, 
                  ymax = NEE_VUT_REF + NEE_VUT_SE_yearly), 
              fill = "grey80", alpha = 0.4) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 1)) +
  theme_minimal() +
  labs(title = "Annual NEP Control Time Series",
       x = "Year",
       y = "NEP (g C m⁻² yr⁻¹)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# THIS GRAPH IS FOR NEP IN THE DISTURBED FOREST
file_path <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"
df_annual <- read_csv(file_path)

file_path1 <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_MM_2007-2021_3-5.csv"
df_monthly <- read_csv(file_path1, col_types = cols(
  TIMESTAMP = col_datetime(format = "%Y%m")
))

df_monthly <- df_monthly %>%
  mutate(Year = year(TIMESTAMP), Month = month(TIMESTAMP))

df_monthly <- df_monthly %>%
  filter(NEE_VUT_REF != -9999)

# Create df_monthly by grouping by Year and Month
df_monthly <- df_monthly %>%
  group_by(Year) %>%
  mutate(
    NEE_VUT_REF_SD = sd(NEE_VUT_REF, na.rm = TRUE),  # Calculate SD of monthly values for each year
    NEE_VUT_SE_calculated = NEE_VUT_REF_SD / sqrt(12)  # Calculate SE from SD
  ) %>%
  ungroup()

df_monthly <- df_monthly %>%
  mutate(NEE_VUT_SE_yearly = NEE_VUT_SE_calculated * (365 / 30))  # Adjust to yearly

# Join with the correct column for SE
df_annual <- df_annual %>%
  left_join(df_monthly %>% select(Year, NEE_VUT_SE_yearly), 
            by = c("TIMESTAMP" = "Year"))

# Filter df_annual for years >= 2008
df_annual_filtered <- df_annual %>% filter(TIMESTAMP >= 2008)
df_annual_filtered$NEE_VUT_REF <- -df_annual_filtered$NEE_VUT_REF

#making a time series from hh data into a yearly time series and hh error bars for NEE, GPP and Reco


# Create the plot for NEE in the disturbed forest with monthly SE
plotNEPdisturbed <- ggplot(df_annual_filtered, aes(x = TIMESTAMP, y = NEE_VUT_REF)) +
  geom_line(color = "blue") +  
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = NEE_VUT_REF - NEE_VUT_SE_yearly, 
                    ymax = NEE_VUT_REF + NEE_VUT_SE_yearly), 
                width = 0.3, color = "black") +
  geom_ribbon(aes(ymin = NEE_VUT_REF - NEE_VUT_SE_yearly, 
                  ymax = NEE_VUT_REF + NEE_VUT_SE_yearly), 
              fill = "grey80", alpha = 0.4) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 1)) +
  theme_minimal() +
  labs(title = "Annual NEP Disturbed Time Series",
       x = "Year",
       y = "NEP (g C m⁻² yr⁻¹)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# THIS GRAPH IS FOR RECO IN THE CONTROL FOREST


library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(zoo)
library(patchwork)

file_path <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"
df_annual <- read_csv(file_path)

file_path1 <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_MM_2007-2021_3-5.csv"
df_monthly <- read_csv(file_path1, col_types = cols(
  TIMESTAMP = col_datetime(format = "%Y%m")
))

df_monthly <- df_monthly %>%
  mutate(Year = year(TIMESTAMP), Month = month(TIMESTAMP))

df_monthly <- df_monthly %>%
  filter(RECO_NT_VUT_REF != -9999)

# Create df_monthly by grouping by Year and Month
df_monthly <- df_monthly %>%
  group_by(Year) %>%
  mutate(
    RECO_NT_VUT_REF_SD = sd(RECO_NT_VUT_REF, na.rm = TRUE),  # Calculate SD of monthly values for each year
    RECO_NT_VUT_REF_calculated = RECO_NT_VUT_REF_SD / sqrt(12)  # Calculate SE from SD
  ) %>%
  ungroup()

df_monthly <- df_monthly %>%
  mutate(RECO_NT_VUT_REF_yearly = RECO_NT_VUT_REF_calculated * (365 / 30))  # Adjust to yearly

# Join with the correct column for SE
df_annual <- df_annual %>%
  left_join(df_monthly %>% select(Year, RECO_NT_VUT_REF_yearly), 
            by = c("TIMESTAMP" = "Year"))

# Filter df_annual for years >= 2008
df_annual_filtered <- df_annual %>% filter(TIMESTAMP >= 2008)

# Create the plot for RECO in the control forest with monthly SE
plotRECOcontrol <- ggplot(df_annual_filtered, aes(x = TIMESTAMP, y = RECO_NT_VUT_REF)) +
  geom_line(color = "red") +  
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = RECO_NT_VUT_REF - RECO_NT_VUT_REF_yearly, 
                    ymax = RECO_NT_VUT_REF + RECO_NT_VUT_REF_yearly), 
                width = 0.3, color = "black") +
  geom_ribbon(aes(ymin = RECO_NT_VUT_REF - RECO_NT_VUT_REF_yearly, 
                  ymax = RECO_NT_VUT_REF + RECO_NT_VUT_REF_yearly), 
              fill = "grey80", alpha = 0.4) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 1)) +
  theme_minimal() +
  labs(title = "Annual RECO Control Time Series",
       x = "Year",
       y = "RECO (g C m⁻² yr⁻¹)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#TIME SERIES FOR RECO DISTURBED FOREST

file_path <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"
df_annual <- read_csv(file_path)

file_path1 <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_MM_2007-2021_3-5.csv"
df_monthly <- read_csv(file_path1, col_types = cols(
  TIMESTAMP = col_datetime(format = "%Y%m")
))

df_monthly <- df_monthly %>%
  mutate(Year = year(TIMESTAMP), Month = month(TIMESTAMP))

df_monthly <- df_monthly %>%
  filter(RECO_NT_VUT_REF != -9999)
  

# Create df_monthly by grouping by Year and Month
df_monthly <- df_monthly %>%
  group_by(Year) %>%
  mutate(
    RECO_NT_VUT_REF_SD = sd(RECO_NT_VUT_REF, na.rm = TRUE),  # Calculate SD of monthly values for each year
    RECO_NT_VUT_REF_calculated = RECO_NT_VUT_REF_SD / sqrt(12)  # Calculate SE from SD
  ) %>%
  ungroup()

df_monthly <- df_monthly %>%
  mutate(RECO_NT_VUT_REF_yearly = RECO_NT_VUT_REF_calculated * (365 / 30))  # Adjust to yearly

# Join with the correct column for SE
df_annual <- df_annual %>%
  left_join(df_monthly %>% select(Year, RECO_NT_VUT_REF_yearly), 
            by = c("TIMESTAMP" = "Year"))

# Filter df_annual for years >= 2008
df_annual_filtered <- df_annual %>% filter(TIMESTAMP >= 2008)

# Create the plot for RECO in the disturbed forest with monthly SE
plotRECOdisturbed <- ggplot(df_annual_filtered, aes(x = TIMESTAMP, y = RECO_NT_VUT_REF)) +
  geom_line(color = "red") +  
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = RECO_NT_VUT_REF - RECO_NT_VUT_REF_yearly, 
                    ymax = RECO_NT_VUT_REF + RECO_NT_VUT_REF_yearly), 
                width = 0.3, color = "black") +
  geom_ribbon(aes(ymin = RECO_NT_VUT_REF - RECO_NT_VUT_REF_yearly, 
                  ymax = RECO_NT_VUT_REF + RECO_NT_VUT_REF_yearly), 
              fill = "grey80", alpha = 0.4) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 1)) +
  theme_minimal() +
  labs(title = "Annual RECO Disturbed Time Series",
       x = "Year",
       y = "RECO (g C m⁻² yr⁻¹)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# PLOTTING GPP CONTROL FOREST
library(ggplot2)
library(readr)
library(lubridate)
library(dplyr)
library(zoo)
library(patchwork)

file_path <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"
df_annual <- read_csv(file_path)

file_path1 <- "../Tower Data/Control_US-UMB_2007-2021/AMF_US-UMB_FLUXNET_FULLSET_MM_2007-2021_3-5.csv"
df_monthly <- read_csv(file_path1, col_types = cols(
  TIMESTAMP = col_datetime(format = "%Y%m")
))

df_monthly <- df_monthly %>%
  mutate(Year = year(TIMESTAMP), Month = month(TIMESTAMP))

df_monthly <- df_monthly %>%
  filter(GPP_NT_VUT_REF != -9999)

# Create df_monthly by grouping by Year and Month
df_monthly <- df_monthly %>%
  group_by(Year) %>%
  mutate(
    GPP_NT_VUT_REF_SD = sd(GPP_NT_VUT_REF, na.rm = TRUE),  # Calculate SD of monthly values for each year
    GPP_NT_VUT_REF_calculated = GPP_NT_VUT_REF_SD / sqrt(12)  # Calculate SE from SD
  ) %>%
  ungroup()

df_monthly <- df_monthly %>%
  mutate(GPP_NT_VUT_REF_yearly = GPP_NT_VUT_REF_calculated * (365 / 30))  # Adjust to yearly

# Join with the correct column for SE
df_annual <- df_annual %>%
  left_join(df_monthly %>% select(Year, GPP_NT_VUT_REF_yearly), 
            by = c("TIMESTAMP" = "Year"))

# Filter df_annual for years >= 2008
df_annual_filtered <- df_annual %>% filter(TIMESTAMP >= 2008)

# Create the plot for GPP in the control forest with monthly SE
plotGPPcontrol <- ggplot(df_annual_filtered, aes(x = TIMESTAMP, y = GPP_NT_VUT_REF)) +
  geom_line(color = "green") +  
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = GPP_NT_VUT_REF - GPP_NT_VUT_REF_yearly, 
                    ymax = GPP_NT_VUT_REF + GPP_NT_VUT_REF_yearly), 
                width = 0.3, color = "black") +
  geom_ribbon(aes(ymin = GPP_NT_VUT_REF - GPP_NT_VUT_REF_yearly, 
                  ymax = GPP_NT_VUT_REF + GPP_NT_VUT_REF_yearly), 
              fill = "grey80", alpha = 0.4) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 1)) +
  theme_minimal() +
  labs(title = "Annual GPP Control Time Series",
       x = "Year",
       y = "GPP (g C m⁻² yr⁻¹)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#TIME SERIES FOR GPP DISTURBED FOREST

file_path <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_YY_2007-2021_3-5.csv"
df_annual <- read_csv(file_path)

file_path1 <- "../Tower Data/Disturbed_US-UMd_2007-2021/AMF_US-UMd_FLUXNET_FULLSET_MM_2007-2021_3-5.csv"
df_monthly <- read_csv(file_path1, col_types = cols(
  TIMESTAMP = col_datetime(format = "%Y%m")
))

df_monthly <- df_monthly %>%
  mutate(Year = year(TIMESTAMP), Month = month(TIMESTAMP))

df_monthly <- df_monthly %>%
  filter(GPP_NT_VUT_REF != -9999)


# Create df_monthly by grouping by Year and Month
df_monthly <- df_monthly %>%
  group_by(Year) %>%
  mutate(
    GPP_NT_VUT_REF_SD = sd(GPP_NT_VUT_REF, na.rm = TRUE),  # Calculate SD of monthly values for each year
    GPP_NT_VUT_REF_calculated = GPP_NT_VUT_REF_SD / sqrt(12)  # Calculate SE from SD
  ) %>%
  ungroup()

df_monthly <- df_monthly %>%
  mutate(GPP_NT_VUT_REF_yearly = GPP_NT_VUT_REF_calculated * (365 / 30))  # Adjust to yearly

# Join with the correct column for SE
df_annual <- df_annual %>%
  left_join(df_monthly %>% select(Year, GPP_NT_VUT_REF_yearly), 
            by = c("TIMESTAMP" = "Year"))

# Filter df_annual for years >= 2008
df_annual_filtered <- df_annual %>% filter(TIMESTAMP >= 2008)

# Create the plot for GPP in the disturbed forest with monthly SE
plotGPPdisturbed <- ggplot(df_annual_filtered, aes(x = TIMESTAMP, y = GPP_NT_VUT_REF)) +
  geom_line(color = "green") +  
  geom_point(size = 0.5) + 
  geom_errorbar(aes(ymin = GPP_NT_VUT_REF - GPP_NT_VUT_REF_yearly, 
                    ymax = GPP_NT_VUT_REF + GPP_NT_VUT_REF_yearly), 
                width = 0.3, color = "black") +
  geom_ribbon(aes(ymin = GPP_NT_VUT_REF - GPP_NT_VUT_REF_yearly, 
                  ymax = GPP_NT_VUT_REF + GPP_NT_VUT_REF_yearly), 
              fill = "grey80", alpha = 0.4) +
  scale_x_continuous(breaks = seq(2008, 2021, by = 1)) +
  theme_minimal() +
  labs(title = "Annual GPP Disturbed Time Series",
       x = "Year",
       y = "GPP (g C m⁻² yr⁻¹)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


(plotNEPcontrol | plotNEPdisturbed) /
(plotRECOcontrol | plotRECOdisturbed) /
(plotGPPcontrol | plotGPPdisturbed)


