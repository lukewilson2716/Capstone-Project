
library(dplyr)
library(ggplot2)
library(patchwork)

# 1. Prepare the data
myData_cleaned <- myData %>%
  mutate(Religion_Cluster = case_when(
    Demographics_relig == "1" ~ "Christian",
    Demographics_relig == "6" ~ "Atheist",
    TRUE ~ "Other" 
  )) %>%
  filter(!is.na(Demographics_rlgsty), !is.na(Total_Spirit_Wellbeing))

# 2. Create the individual plots

# Plot 1: Christian (Red Scale)
p1 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Christian"), 
             aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "#FF0000") + # Red
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Christian", x = "Religiosity", y = "Spiritual Wellbeing") +
  theme_minimal() + theme(legend.position = "none")

# Plot 2: Atheist (Purple Scale)
p2 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Atheist"), 
             aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "#6A0DAD") + # Purple
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Atheist", x = "Religiosity", y = "") + # Removed Y label for middle plot
  theme_minimal() + theme(legend.position = "none")

# Plot 3: Other (Blue/Steel Scale)
p3 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Other"), 
             aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "steelblue") + 
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Other", x = "Religiosity", y = "") + # Removed Y label for clean look
  theme_minimal() + theme(legend.position = "none")

# 3. Combine them using patchwork
(p1 | p2 | p3) + 
  plot_annotation(
    title = "Religiosity vs. Spiritual Wellbeing by Group",
    subtitle = "Darker colors indicate higher density of participants"
  )





# 2. Create the individual plots

# Plot 1: Christian (Red Scale)
p4 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Christian"), 
             aes(x = Demographics_rlgsty, y = Total_Social_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "#FF0000") + # Red
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Christian", x = "Religiosity", y = "Social Wellbeing") +
  theme_minimal() + theme(legend.position = "none")

# Plot 2: Atheist (Purple Scale)
p5 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Atheist"), 
             aes(x = Demographics_rlgsty, y = Total_Social_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "#6A0DAD") + # Purple
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Atheist", x = "Religiosity", y = "") + # Removed Y label for middle plot
  theme_minimal() + theme(legend.position = "none")

# Plot 3: Other (Blue/Steel Scale)
p6 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Other"), 
             aes(x = Demographics_rlgsty, y = Total_Social_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "steelblue") + 
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Other", x = "Religiosity", y = "") + # Removed Y label for clean look
  theme_minimal() + theme(legend.position = "none")

# 3. Combine them using patchwork
(p4 | p5 | p6) + 
  plot_annotation(
    title = "Religiosity vs. Social Wellbeing by Group",
    subtitle = "Darker colors indicate higher density of participants"
  )








# 2. Create the individual plots

# Plot 1: Christian (Red Scale)
p7 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Christian"), 
             aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "#FF0000") + # Red
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Christian", x = "Religiosity", y = "Psychological Wellbeing") +
  theme_minimal() + theme(legend.position = "none")

# Plot 2: Atheist (Purple Scale)
p8 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Atheist"), 
             aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "#6A0DAD") + # Purple
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Atheist", x = "Religiosity", y = "") + # Removed Y label for middle plot
  theme_minimal() + theme(legend.position = "none")

# Plot 3: Other (Blue/Steel Scale)
p9 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Other"), 
             aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  scale_fill_gradient(low = "white", high = "steelblue") + 
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Other", x = "Religiosity", y = "") + # Removed Y label for clean look
  theme_minimal() + theme(legend.position = "none")

# 3. Combine them using patchwork
(p7 | p8 | p9) + 
  plot_annotation(
    title = "Religiosity vs. Psychological Wellbeing by Group",
    subtitle = "Darker colors indicate higher density of participants"
  )

