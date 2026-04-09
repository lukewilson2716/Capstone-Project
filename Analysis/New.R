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


myData_cleaned <- myData %>%
  mutate(Religion_Cluster = case_when(
    Demographics_relig == "1" ~ "Christian",
    Demographics_relig == "6" ~ "Atheist",
    TRUE ~ "Other" 
  )) %>%
  # Filter out rows with missing values in your key variables
  filter(!is.na(Demographics_rlgsty), !is.na(Total_Spirit_Wellbeing))

# 3. Create the Smoothed Heatmap
# stat_density_2d creates the "smooth" matrix effect
ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  # Fill the area with a smooth density gradient
  stat_density_2d(aes(fill = after_stat(density)), 
                  geom = "raster", 
                  contour = FALSE, 
                  n = 200) + 
  # Use facet_wrap to create one graph per cluster
  facet_wrap(~Religion_Cluster) +
  # Color palette - 'magma' or 'viridis' work well for heatmaps
  scale_fill_viridis_c(option = "magma") +
  # Ensure the axes reflect your 1-7 scale
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(
    title = "Religiosity vs. Wellbeing Heatmap",
    subtitle = "Separated by Religion Cluster (Smoothed Density)",
    x = "Religiosity (1-7)",
    y = "Spiritual Wellbeing (1-7)",
    fill = "Density"
  ) +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        strip.text = element_text(face = "bold", size = 12))

