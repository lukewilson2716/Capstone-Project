read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
library(dplyr)
library(ggplots2)
library(patchwork)
#needed for this script

myData_cleaned <- myData %>%
  mutate(Religion_Cluster = case_when(
    Demographics_relig == "1" ~ "Christian",
    Demographics_relig == "6" ~ "Atheist",
    TRUE ~ "Other"  # This catches "1,6", other numbers, and NAs
  ))

# Convert to factor so the plot labels stay organized
myData_cleaned$Religion_Cluster <- factor(myData_cleaned$Religion_Cluster, 
                                          levels = c("Christian", "Atheist", "Other"))

model1 <- aov(Total_Int_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model1)

model2 <- aov(Total_Spirit_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model2)
#Sig result

model3 <- aov(Total_Emot_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model3)

model4 <- aov(Total_Psych_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model4)
#Sig, not as much

model5 <- aov(Total_Physical_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model5)

model6 <- aov(Total_Spirit_Wellbeing ~ Religion_Cluster, data = myData_cleaned)
summary(model2)

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
a4 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title3,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#int wellbeing graph
a4
ggplot(myData_cleaned, aes(x = Religion_Cluster, y = Total_Spirit_Wellbeing, fill = Religion_Cluster)) +
  geom_boxplot() +
  labs(title = "Spirit Wellbeing Across Clusters",
       x = "Cluster Group",
       y = "Total Spirit Wellbeing") +
  theme_minimal() + theme(legend.position = "bottom")
#boxplot of new results


a1 + a2 + a3 + plot_layout(guides = "collect") + plot_annotation(title = "Clustered Analysis of Religion, Religiosity, and their Relationship with Well-Being")
#visualization for the poster maybe
                                                                 