
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
  geom_smooth(method = "lm", color = "black", linewidth = 0.5, 
              se = FALSE, fullrange = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "#FF0000") + # Red
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Christian", x = "Religiosity", y = "Spiritual Wellbeing") +
  theme_minimal() + theme(legend.position = "none")

# Plot 2: Atheist (Purple Scale)
p2 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Atheist"), 
             aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.5, 
              se = FALSE, fullrange = TRUE, na.rm = TRUE) +
  scale_fill_gradient(low = "white", high = "#6A0DAD") + # Purple
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(title = "Atheist", x = "Religiosity", y = "") + # Removed Y label for middle plot
  theme_minimal() + theme(legend.position = "none")

# Plot 3: Other (Blue/Steel Scale)
p3 <- ggplot(filter(myData_cleaned, Religion_Cluster == "Other"), 
             aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE, n = 200) +
  geom_smooth(method = "lm", color = "black", linewidth = 0.5, 
              se = FALSE, fullrange = TRUE, na.rm = TRUE) +
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







p1 <- ggplot(filter(myData_density_compare, Religion_Cluster == "Christian"), 
             aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  
  # 1. Upperclassmen Heat (Red)
  # Added 'lims' to force the density calculation to cover the full 1-7 range
  stat_density_2d(data = filter(myData_density_compare, Religion_Cluster == "Christian", Year_Group == "Upperclassmen"),
                  aes(fill = after_stat(count)), geom = "raster", contour = FALSE, 
                  n = 200, na.rm = TRUE, show.legend = FALSE,
                  lims = c(1, 7, 1, 7)) + # Sets calculation limits for [x_min, x_max, y_min, y_max]
  scale_fill_gradient(low = "white", high = "#FF0000") + 
  
  # 2. Underclassmen Heat (Blue)
  stat_density_2d(data = filter(myData_density_compare, Religion_Cluster == "Christian", Year_Group == "Underclassmen"),
                  aes(alpha = after_stat(count)), fill = "blue", geom = "raster", contour = FALSE, 
                  n = 200, na.rm = TRUE, show.legend = FALSE,
                  lims = c(1, 7, 1, 7)) + # Sets calculation limits here too
  
  scale_alpha_continuous(range = c(0, 0.5)) + 
  
  # 3. Two Trendlines
  geom_smooth(data = filter(myData_density_compare, Religion_Cluster == "Christian", Year_Group == "Upperclassmen"),
              method = "lm", color = "black", linewidth = 0.7, se = FALSE, fullrange = TRUE, na.rm = TRUE) +
  geom_smooth(data = filter(myData_density_compare, Religion_Cluster == "Christian", Year_Group == "Underclassmen"),
              method = "lm", color = "black", linewidth = 0.4, linetype = "dashed", se = FALSE, fullrange = TRUE, na.rm = TRUE) +
  
  # 4. Custom Key
  annotate("segment", x = 1.2, xend = 1.7, y = 6.8, yend = 6.8, color = "black", linewidth = 0.8) +
  annotate("text", x = 1.8, y = 6.8, label = "Upperclassmen (n = 243)", hjust = 0, size = 3.5, fontface = "italic") +
  annotate("segment", x = 1.2, xend = 1.7, y = 6.4, yend = 6.4, color = "black", linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = 1.8, y = 6.4, label = "Underclassmen (n = 88)", hjust = 0, size = 3.5, fontface = "italic") +
  
  # 5. Lock the View
  coord_cartesian(xlim = c(1, 7), ylim = c(1, 7), expand = FALSE) + # expand = FALSE makes the heat touch the axis lines
  scale_x_continuous(breaks = 1:7) +
  scale_y_continuous(breaks = 1:7) +
  
  labs(title = "Christian Participants", 
       subtitle = "Proportional density: Red (N=243) vs Blue (N=88)",
       x = "Religiosity Score (1-7)", y = "Spiritual Wellbeing Score (1-7)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14), 
        legend.position = "none",
        panel.grid.minor = element_blank()) # Cleans up the background grid

p1
