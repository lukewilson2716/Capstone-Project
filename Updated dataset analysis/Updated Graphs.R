read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Spiritual Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Box & Whiskers Plot of religiosity & Spiritual wellbeing


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Psych Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()

boxplot(Total_Int_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")
#Religion and int wellbeing graph, strongest result i think

ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing, 
                   color = as.factor(Demographics_relig))) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + # Trend lines for each group
  labs(title = "Impact of Religiosity on Wellbeing across Religions",
       color = "Religion Group") +
  theme_light()


ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing)) +
  geom_jitter(alpha = 0.4) + # Adds jitter so points don't overlap
  geom_smooth(method = "lm", color = "red") + # Adds the trend line
  facet_wrap(~ as.factor(Demographics_relig)) + # Splits by religion
  labs(title = "Religiosity and Spiritual Wellbeing by Religious Group",
       x = "Religiosity Score (1-7)",
       y = "Spiritual Wellbeing") +
  theme_minimal()

ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Spirit_Wellbeing, 
                   color = as.factor(Demographics_relig))) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) + # Trend lines for each group
  labs(title = "Impact of Religiosity on Spiritual Wellbeing across Religions",
       color = "Religion Group") +
  theme_light()

