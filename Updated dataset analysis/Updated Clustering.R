read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggpubr)
#needed for this script


boxplot(Total_Spirit_Wellbeing ~ Demographics_class, data = myData, 
        main="Outcome by Religion", col="lightblue")
myData %>% count(Demographics_class)
# First graph, looks at college students n wellbeing

myData_cleaned <- myData %>%
  # Convert to numeric just in case they were read as characters
  mutate(Demographics_relig = as.numeric(Demographics_relig)) %>% 
  mutate(Religion_Cluster = case_when(
    Demographics_relig == 1 ~ "Christian",
    Demographics_relig == 6 ~ "Atheist",
    TRUE ~ "Other"
  )) %>%
  # This ensures we don't plot rows where the cluster or variables are missing
  filter(!is.na(Religion_Cluster), 
         !is.na(Demographics_rlgsty), 
         !is.na(Total_Spirit_Wellbeing))

# Convert to factor so the plot labels stay organized
myData_cleaned$Religion_Cluster <- factor(myData_cleaned$Religion_Cluster, 
                                          levels = c("Christian", "Atheist", "Other"))

model1 <- aov(Total_Int_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model1)

model2 <- aov(Total_Spirit_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model2)
TukeyHSD(model2)
#Sig result

model3 <- aov(Total_Emot_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model3)

model4 <- aov(Total_Psych_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model4)
TukeyHSD(model4)
#Sig, not as much

model5 <- aov(Total_Physical_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model5)

model6 <- aov(Total_Social_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model6)
TukeyHSD(model6)

covModel1 <- aov(Total_Int_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel1)
#nothing

covModel2 <- aov(Total_Spirit_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel2)
#very cool results, both cluster and religiosity have sig results here

covModel3 <- aov(Total_Psych_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel3)
#Clustered Anova for psych wellbeing, Religion had effect, cluster didnt

covModel4 <- aov(Total_Emot_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel4)
#nothing

covModel5 <- aov(Total_Social_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel5)
#Religion had effect

covModel6 <- aov(Total_Physical_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel6)
#nothing

title1 <- str_wrap("Overall Wellbeing", width = 30)
title2 <- str_wrap("Social Wellbeing", width = 30)
title3 <- str_wrap("Spiritual Wellbeing", width = 30)

 a1 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title1,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#Psych wellbeing graph

a2 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Social_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title2,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#emotional wellbeing graph

a3 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title3,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#int wellbeing graph
a4 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title3,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#int wellbeing graph
a4

my_comparisons <- list( c("Christian", "Atheist"), c("Atheist", "Other"), c("Christian", "Other") )
ggplot(myData_cleaned, aes(x = Religion_Cluster, y = Total_Spirit_Wellbeing, fill = Religion_Cluster)) +
  geom_boxplot() +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif") +
  coord_flip() +
  labs(title = "Spiritual Well-Being Across Religious Clusters",
       x = "Cluster Group",
       y = "Total Spirit Wellbeing") +
  theme_minimal() + theme(legend.position = "bottom")
#boxplot of new results
#run atheism vs everything else


a1 + a2 + a3 + plot_layout(guides = "collect") + plot_annotation(title = "Clustered Analysis of Religion, Religiosity, and their Relationship with Well-Being")
#visualization for the poster maybe

myData_cleaned$Religion_Cluster <- factor(myData_cleaned$Religion_Cluster, 
                                          levels = c("Christian", "Atheist", "Other"))

ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  stat_density_2d(aes(fill = after_stat(density)), 
                  geom = "raster", 
                  contour = FALSE, 
                  n = 200) + 
  # Use facet_wrap to create one graph per cluster
  facet_wrap(~Religion_Cluster) +
  scale_fill_viridis_c(option = "magma") +
  scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
  scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
  labs(
    title = "Religiosity vs. Spiritual Wellbeing Heatmap",
    subtitle = "Separated by Religion Cluster (Smoothed Density)",
    x = "Religiosity (1-7)",
    y = "Spiritual Wellbeing (1-7)",
    fill = "Density"
  ) +
  theme_minimal() +
  theme(panel.spacing = unit(2, "lines"),
        strip.text = element_text(face = "bold", size = 12))
