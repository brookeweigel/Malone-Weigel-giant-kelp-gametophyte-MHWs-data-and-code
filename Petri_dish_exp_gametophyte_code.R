#------------------------Petri experiment plots--------------------------------

#-----------------------------Load packages------------------------------------

library(tidyverse)
library(ggthemes)
library(viridis)
library(tidyr)
library(dplyr)

#------------------------Read data and reorder------------------------------
#Read in data
data <- read.csv("Petri_dish_exp_gametophyte_data.csv")

#Rename Long and Short HWs by  length
new_data <- data %>% rename(HW_Length = Group)

#Reorder long/short
new_data$HW_Length <- factor(new_data$HW_Length, levels = c("Short", "Long"))
clean_data <- new_data[order(new_data$HW_Length), ]

#Remove NA values 
clean_data <- clean_data[clean_data$Gametophyte_area_mm2 != 0, ]

plain_data <- clean_data %>%
  select(Date, HW_Length, Temp, Dish, FOV, Gametophyte, Gametophyte_area_mm2)

#------------------------Calculate average sizes per dish----------------------------

data_avgs <- plain_data %>%
  group_by(Date, HW_Length, Temp, Dish) %>%
  summarise(avg_mm2 = mean(Gametophyte_area_mm2, na.rm = TRUE), .groups = "drop")

#Reconfigure to get an average size of each individual dish at each time period
wide_avgs <- data_avgs %>%
  pivot_wider(
    names_from = Date,
    values_from = avg_mm2
  ) %>%
  rename(
    avg_size_before = Pre,
    avg_size_after = Post,
    avg_size_recovered = Recovered
  )

#Reorder sizes to go from before to after to recovered
wide_avgs <- wide_avgs %>%
  select(HW_Length, Temp, Dish, avg_size_before, avg_size_after, avg_size_recovered)


#----------------------------Calculate growth rates------------------------
#Setting the length of experiment in days, based on heatwave length
wide_avgs <- wide_avgs %>%
  mutate(days = if_else(HW_Length == "Long", 16, 9)
  )

#Convert avg. size per dish to avg. GR per dish using days
wide_avgs <- wide_avgs %>%
  mutate(
    hw_growth_rate = (avg_size_after - avg_size_before) / days,   # growth hw
    rec_growth_rate = (avg_size_recovered - avg_size_after) / 10 # recovery growth
  )

#Temp as factor
wide_avgs <- wide_avgs %>%
  mutate(Temp = factor(Temp, levels = c(12, 18, 20, 22)))

#-------------------------Faceted boxplots of GRs------------------------

#Heatwave - growth rates with facet wrap for HWL (1 and 2 week)
ggplot(wide_avgs, aes(x = Temp, y = hw_growth_rate, fill = HW_Length)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  facet_wrap(~ HW_Length) +
  theme_minimal() +
  labs(
    title = "Average Heatwave Growth Rate by Temperature, by HW Length",
    x = "Temperature (°C)",
    y = "Heatwave Growth Rate (mm²/day)",
    color = "HW_Length"
  ) +
  scale_fill_manual(values = c("Long" = "#045275", "Short" = "#7ccba2")) 

#Recovery - GRs faceted by length
ggplot(wide_avgs, aes(x = Temp, y = rec_growth_rate, fill = HW_Length)) +
  geom_boxplot() + 
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  facet_wrap(~ HW_Length) +
  theme_minimal() +
  labs(
    title = "Average Recovery Growth Rate by Temperature, by HW Length",
    x = "Temperature (°C)",
    y = "Recovery Growth Rate (mm²/day)",
    color = "HW_Length"
  ) +
  scale_fill_manual(values = c("Long" = "#045275", "Short" = "#7ccba2")) 

