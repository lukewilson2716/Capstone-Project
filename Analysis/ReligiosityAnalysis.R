read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data


model1 <- lm(Psychological.Wellness ~ myData$Demographics_rlgsty, data = myData)
#linear regression for psych wellness and religiosity
summary(model1)

model2 <- lm(Total_Spirit_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model2)
#linear regression for spiritual wellness and religiosity - stat sig result! Although 1/3 of data not there.


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Int_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Intellectual Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Plot of religiosity & Intellectual wellbeing

ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Spiritual Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Plot of religiosity & Spiritual wellbeing - very strong for poster prob
