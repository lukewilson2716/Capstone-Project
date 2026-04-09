read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
#needed for this script

myData_cleaned <- myData %>%
  mutate(Religion_Cluster = case_when(
    Demographics_relig == "1" ~ "Christian",
    TRUE ~ "Other"  # This catches "1,6", other numbers, and NAs
  ))


a1 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title1,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
a1
#try heatmap
#smooth the bins, violin plots