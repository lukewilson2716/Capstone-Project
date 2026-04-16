read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data


library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

# 1. Combined Data Prep (Keeping both sets of logic for your dataset)
myData_final <- myData %>%
  # Religion Clustering
  mutate(Religion_Cluster = case_when(
    Demographics_relig == "1" ~ "Christian",
    Demographics_relig == "6" ~ "Atheist",
    TRUE ~ "Other" 
  )) %>%
  filter(!is.na(Demographics_rlgsty), !is.na(Total_Spirit_Wellbeing)) %>%
  
  # Class/Year Grouping
  mutate(Demographics_class = as.numeric(as.character(Demographics_class))) %>%
  mutate(Year_Group = case_when(
    Demographics_class %in% c(1, 2) ~ "Underclassmen (N=88)",
    Demographics_class %in% c(3, 4) ~ "Upperclassmen (N=243)",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Year_Group)) %>%
  mutate(Year_Group = factor(Year_Group, levels = c("Upperclassmen (N=243)", "Underclassmen (N=88)")))

# 2. Visualization: Underclassmen vs. Upperclassmen
a1 <- ggplot(myData_final, aes(x = Demographics_rlgsty, fill = Year_Group)) +
  # Using density with transparency for the overlay
  geom_density(alpha = 0.5) +
  # Manually setting your preferred colors
  scale_fill_manual(values = c(
    "Underclassmen (N=88)" = "#0072B2", 
    "Upperclassmen (N=243)" = "#D55E00"
  )) +
  theme_minimal() +
  labs(
    title = "Distribution of Religiosity Scores by Academic Class",
    x = "Religiosity Score",
    y = "Density",
    fill = "Class Group"
  )
a1


myData_final <- myData_final %>%
  mutate(
    Total_Spirit_Wellbeing = as.numeric(as.character(Total_Spirit_Wellbeing)),
    Demographics_rlgsty = as.numeric(as.character(Demographics_rlgsty))
  )

# 2. Run the analysis for each group
# We use 'group_by' and 'do' or 'nest' to run a model per cluster
library(purrr)
library(broom)

group_analysis <- myData_final %>%
  # Filter only for the Spiritual dimension since we pivoted the data
  filter(Wellness_Dimension == "Spirit") %>% 
  group_by(Religion_Cluster) %>%
  group_modify(~ {
    # Use 'Score' instead of the original column name
    model <- lm(Score ~ Demographics_rlgsty, data = .x)
    tidy(model)
  })

print(group_analysis)


myData_overall <- myData_final %>%
  filter(Wellness_Dimension == "Spirit") %>%
  mutate(
    Score = as.numeric(as.character(Score)),
    Demographics_rlgsty = as.numeric(as.character(Demographics_rlgsty))
  )

# 2. Run the overall Linear Model
overall_model <- lm(Score ~ Demographics_rlgsty, data = myData_overall)

# 3. View the summary
summary(overall_model)

# 4. Optional: View in a clean table format (using broom)
library(broom)
tidy(overall_model)



