read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

#Which looks better/interesting?
ggplot(myData, aes(x = Demographics_rlgsty, y = Total_Psych_Wellbeing)) +
  geom_jitter(alpha = 0.4) + # Adds jitter so points don't overlap
  geom_smooth(method = "lm", color = "red") + # Adds the trend line
  facet_wrap(~ as.factor(Demographics_relig)) + # Splits by religion
  labs(title = "Religiosity and Psych Wellbeing by Religious Group",
       x = "Religiosity Score (1-7)",
       y = "Psychological Wellbeing") +
  theme_minimal()

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
