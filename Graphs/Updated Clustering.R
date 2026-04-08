
read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
library(dplyr)

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

covModel2 <- aov(Total_Spirit_Wellbeing ~ Religion_Cluster * Demographics_rlgsty, data = myData_cleaned)
summary(covModel2)
#very cool results, both cluster and religiosity have sig results here



title1 <- str_wrap("Overall Wellbeing", width = 30)
title2 <- str_wrap("Emotional Wellbeing", width = 30)
title3 <- str_wrap("Intellectual Wellbeing", width = 30)

 a1 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title1,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#Psych wellbeing graph

a2 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Emot_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title2,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#emotional wellbeing graph

a3 <- ggplot(myData_cleaned, aes(x = Demographics_rlgsty, y = Total_Int_Wellbeing, color = Religion_Cluster)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "lm", formula = y ~ x) + # Adds a linear regression line for each group
  labs(title = title3,
       subtitle = "By Religious Cluster",
       x = "Religiosity Score",
       y = "Wellbeing Score") +
  theme_minimal()
#int wellbeing graph

a1 + a2 + a3 + plot_layout(guides = "collect") + plot_annotation(title = "Clustered Anaylsis of Religion, Religiosity, and their Relationship with Well-Being")
#visualization for the poster maybe
                                                                 