read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data



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
#Plot of religiosity & Spiritual wellbeing - VERY STRONG for poster prob


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Social_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Social Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Plot of religiosity & Social wellbeing


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Emot_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Emotional Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Plot of religiosity & Emotional wellbeing - Decent connection


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Psych Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Plot of religiosity & Psych wellbeing
