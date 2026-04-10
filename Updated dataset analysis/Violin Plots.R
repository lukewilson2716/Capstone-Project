read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
library(dplyr)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(ggpubr)


#Prepare Data
myData_class_long <- myData %>%
  mutate(Demographics_class = as.numeric(Demographics_class)) %>%
  mutate(Year_Group = case_when(
    Demographics_class %in% c(1, 2) ~ "Underclassmen (N=88)",
    Demographics_class %in% c(3, 4) ~ "Upperclassmen (N=243)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Year_Group)) %>%
  mutate(Year_Group = factor(Year_Group, levels = c("Upperclassmen (N=243)", "Underclassmen (N=88)"))) %>%
  pivot_longer(cols = starts_with("Total_"), names_to = "Wellness_Dimension", values_to = "Score") %>%
  mutate(Wellness_Dimension = str_remove_all(Wellness_Dimension, "Total_|_Wellbeing")) %>%
  filter(!is.na(Score))

#Plot
ggplot(myData_class_long, aes(x = Wellness_Dimension, y = Score, fill = Year_Group)) +
  geom_violin(position = "identity", alpha = 0.6, scale = "count") +
  stat_summary(geom = "point", position = position_identity()) +
  stat_compare_means(method = "t.test", label = "p.signif") +
  scale_fill_manual(values = c("Underclassmen (N=88)" = "#56B4E9", "Upperclassmen (N=243)" = "#E69F00")) +
  scale_y_continuous(breaks = 1:7) +
  labs(title = "Wellness Scores: Class Comparison", fill = "Group") +
  coord_cartesian(ylim = c(1, 8)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1), legend.position = "bottom") 
#top line to make plot, second to make violin, third for mean dots, fourth for stat sig comp, everything else aesthetics


#Prepare Data for sexual orientation analysis - no results of note and pretty much the same process as class analysis above
myData_sex_long <- myData %>%
  mutate(Demographics_sex = as.numeric(Demographics_sex)) %>%
  mutate(Sex_Group = case_when(
    Demographics_sex == 1 ~ "Men (N=93)",
    Demographics_sex == 2 ~ "Women (N=241)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Sex_Group)) %>%
  # REORDER: This ensures Men (smaller group) is drawn second/on top
  mutate(Sex_Group = factor(Sex_Group, levels = c("Women (N=241)", "Men (N=93)"))) %>%
  pivot_longer(cols = starts_with("Total_"), names_to = "Wellness_Dimension", values_to = "Score") %>%
  mutate(Wellness_Dimension = str_remove_all(Wellness_Dimension, "Total_|_Wellbeing")) %>%
  filter(!is.na(Score))

# Plot
ggplot(myData_sex_long, aes(x = Wellness_Dimension, y = Score, fill = Sex_Group)) +
  geom_violin(position = "identity", alpha = 0.6, scale = "count") +
  stat_summary(geom = "point", position = position_identity()) +
  stat_compare_means(method = "t.test", label = "p.signif") +
  scale_fill_manual(values = c("Men (N=93)" = "#0072B2", "Women (N=241)" = "#4b0082")) +
  scale_y_continuous(breaks = 1:7) +
  labs(title = "Wellness Scores: Sex Comparison", fill = "Group") +
  coord_cartesian(ylim = c(1, 8)) + 
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1), legend.position = "bottom")


