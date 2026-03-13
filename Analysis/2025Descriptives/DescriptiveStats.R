read.csv("Data/2025Data - 1-2 (1).csv")
# Read the file
myData <- read.csv("Data/2025Data - 1-2 (1).csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data
print(summary(myData$Psychological.Wellness))
# This Gives Mean + Quartile info
print(sd(myData$Psychological.Wellness, na.rm = TRUE))
# St Dev, na.rm to ignore NA responses
#hi

myData$Demographics_relig <- as.factor(myData$Demographics_relig)
#Turn Religion Into Groups
model1 <- aov(Psychological.Wellness ~ myData$Demographics_relig, data = myData)
TukeyHSD(model1)
#Anova + Tukey for Post-Hoc
summary(model1)
# Psychological Wellness Mean = 4.631, St. Dev = 0.7435167
# P .675 > 0.05, No Stat Sig Diff For Psych Wellness by Religion

model2 <- lm(Psychological.Wellness ~ myData$Demographics_rlgsty, data = myData)
#linear regression for psych wellness and religiosity
summary(model2)
#P .212 > 0.05, no stat sig relationship between Psych Wellness and Religiosity

model2 <- lm(Spiritual.Wellness ~ myData$Demographics_rlgsty, data = myData)
summary(model2)
ggplot(myData, aes(x = Demographics_rlgsty, y = Spiritual.Wellness)) +
  geom_jitter(alpha = 0.5, color = "darkblue", width = 0.2, height = 0.2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Adds a trend line
  labs(title = "Religiosity vs. Spiritual Well-being",
       x = "Religiosity Score (1-7)",
       y = "Total Well-being Score (1-7)") +
  theme_minimal()
#Oh our datas goated