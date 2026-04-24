read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(tidyverse)




library(tidyverse)

# 1. Clean and Prepare Data for the Christian Subset
myData_christian <- myData %>%
  # Standardize types
  mutate(
    Demographics_relig = as.character(Demographics_relig),
    Demographics_class = as.numeric(as.character(Demographics_class))
  ) %>%
  # Filter for Christians and valid responses only
  filter(
    Demographics_relig == "1", 
    !is.na(Demographics_rlgsty), 
    !is.na(Total_Spirit_Wellbeing),
    Demographics_class %in% 1:4
  ) %>%
  # Assign Class Labels
  mutate(Class_Label = case_when(
    Demographics_class %in% c(1, 2) ~ "Underclassmen",
    Demographics_class %in% c(3, 4) ~ "Upperclassmen"
  )) %>%
  # Dynamic N-count for Christians ONLY
  group_by(Class_Label) %>%
  mutate(n_count = n()) %>%
  ungroup() %>%
  mutate(Year_Group = paste0(Class_Label, " (n=", n_count, ")")) %>%
  # Ensure factor order (Upperclassmen on top)
  mutate(Year_Group = fct_reorder(Year_Group, Demographics_class, .desc = TRUE))

# 2. Extract specific labels for color mapping consistency
labels_vec <- unique(myData_christian$Year_Group)
upper_label <- labels_vec[str_detect(labels_vec, "Upper")]
under_label <- labels_vec[str_detect(labels_vec, "Under")]

labels_vec <- unique(myData_christian$Year_Group)
upper_label <- labels_vec[str_detect(labels_vec, "Upper")]
under_label <- labels_vec[str_detect(labels_vec, "Under")]

# 3. Create the Proportional Plot
christian_plot_proportional <- ggplot(myData_christian, 
                                      aes(x = Demographics_rlgsty, 
                                          fill = Year_Group, 
                                          # This line makes the heights proportional to sample size
                                          y = after_stat(count))) +
  # Using position = "identity" so they overlap without stacking
  geom_density(alpha = 0.5, position = "identity", color = NA) +
  
  # Locked-in colors
  scale_fill_manual(values = setNames(c("#D55E00", "#0072B2"), c(upper_label, under_label))) +
  
  # Locked-in X-axis range
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  
  theme_minimal() +
  labs(
    title = "Christianity: Proportional Religiosity Distribution",
    subtitle = "Heights reflect actual sample sizes (Upperclassmen outnumber Underclassmen)",
    x = "Religiosity Score (1-7)",
    y = "Frequency (Count)", # Changed label to match the new scale
    fill = "Group"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

christian_plot_proportional










a1




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


