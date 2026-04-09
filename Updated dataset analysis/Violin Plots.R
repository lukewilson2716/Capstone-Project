read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)

myData_long <- myData_cleaned %>%
  select(Total_Psych_Wellbeing, Total_Spirit_Wellbeing, Total_Emot_Wellbeing, 
         Total_Social_Wellbeing, Total_Physical_Wellbeing, Total_Int_Wellbeing) %>%
  pivot_longer(cols = everything(), 
               names_to = "Wellness_Dimension", 
               values_to = "Score") %>%
  # Clean up the names (optional: removes the "Total_" and "_Wellness" for cleaner labels)
  mutate(Wellness_Dimension = str_remove_all(Wellness_Dimension, "Total_|_Wellness"))

# 2. Create the Violin Plot
ggplot(myData_long, aes(x = Wellness_Dimension, y = Score, fill = Wellness_Dimension)) +
  # Draw the violin
  geom_violin(trim = FALSE, alpha = 0.7) +
  # Add a small boxplot inside to show the median and quartiles
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  # Add a point for the Mean score (since you asked for Average Score)
  stat_summary(fun = "mean", geom = "point", color = "red", size = 2) +
  labs(
    title = "Comparison of Wellness Dimensions",
    x = "Wellness Dimension",
    y = "Average Score",
    caption = "Red dots indicate the mean score"
  ) +
  theme_minimal() +
  theme(legend.position = "none", # Hide legend since X-axis labels are sufficient
        axis.text.x = element_text(angle = 45, hjust = 1)) # Tilt labels for readability
#violin plot of scores