#----------------------Tile Experiment Plots-----------------------------

#----------------------------Load packages-----------------------------

library(tidyverse)
library(ggthemes)
library(viridis)
library(tidyr)
library(dplyr)

#------------------------Read data and clear NAs-----------------------------

data <- read.csv("Tile_exp_gametophyte_data.csv")
data$Gametophyte_area_mm2 <- ifelse(is.na(data$Gametophyte_area_mm2), 0, data$Gametophyte_area_mm2)

#---------------------------Reorder the data-----------------------------

workable <- data %>%
  select(Date, Group, Temp, Tile, FOV, Gametophyte, Gametophyte_area_mm2)

#Dataframe with average gametophyte size for each tile across temps/groups
wide_avg <- workable %>%
  group_by(Date, Group, Temp, Tile) %>%
  summarise(avg_mm2 = mean(Gametophyte_area_mm2, na.rm = TRUE), .groups = "drop")

#Create group periods of pre, post, and recovered size for comparing
wide_data <- wide_avg %>%
  pivot_wider(
    names_from = Date,
    values_from = avg_mm2
  ) %>%
  rename(
    avg_size_before = Pre,
    avg_size_after = Post,
    avg_size_recovered = Recovered
  )

#--------------------Calculate avg growth rate per tile-------------------------

#Set number of days between photo measurements for Long/Short HWs
wide_data <- wide_data %>%
  mutate(days = if_else(Group == "Long", 18, 11)
  )

#Divide size differences by days
wide_data <- wide_data %>%
  mutate(
    hw_growth_rate = (avg_size_after - avg_size_before) / days,   # growth hw
    rec_growth_rate = (avg_size_recovered - avg_size_after) / 7 # recovery growth
  )

#Temp and HW as factors for easier plotting
wide_data <- wide_data %>%
  mutate(Temp = factor(Temp, levels = c(12, 18, 20, 22)))

wide_data <- wide_data %>%
  mutate(Group = factor(Group, levels = c("Short", "Long"))) %>%
  arrange(Group)

# ------------------Boxplots of GR by temp and HW length------------------------

#Heatwave period - facet wrap for HWL (1 vs 2 week)
ggplot(wide_data, aes(x = Temp, y = hw_growth_rate, fill = Group)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  facet_wrap(~ Group) +
  theme_minimal() +
  labs(
    title = "Average Heatwave Growth Rate by Temperature, Separated by HW Length",
    x = "Temperature (°C)",
    y = "Heatwave Growth Rate (mm²/day)",
    color = "Group"
  ) +
  scale_fill_manual(values = c("Long" = "#045275", "Short" = "#7ccba2")) 


#Recovery period - facet wrap for HWL (1 vs 2 week)
ggplot(wide_data, aes(x = Temp, y = rec_growth_rate, fill = Group)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  facet_wrap(~ Group) +
  theme_minimal() +
  labs(
    title = "Average Recovery Growth Rate by Temperature, Separated by HW Length",
    x = "Temperature (°C)",
    y = "Recovery Growth Rate (mm²/day)",
    color = "Group"
  ) +
  scale_fill_manual(values = c("Long" = "#045275", "Short" = "#7ccba2"))  

