read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

#1/3 of data (2023) missing


model1 <- lm(Total_Psych_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model1)
#linear regression for psych wellness and religiosity - nothin

model2 <- lm(Total_Spirit_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model2)
#linear regression for spiritual wellness and religiosity -  STAT SIG result! Although 1/3 of data not there.
#Due to bonferronis if we are counting 6 groups our p should be 0.05/6=0.008, still stat sig but if we looking for more groups it wont be.

ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Spiritual Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Plot of religiosity & Spiritual wellbeing - VERY STRONG for poster prob


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Psych Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()

