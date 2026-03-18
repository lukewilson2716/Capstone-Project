read.csv("Data/Complete_Data - 1.csv", header = TRUE)
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)

myData$Demographics_relig <- as.factor(myData$Demographics_relig)

library(car)

# ANOVA
model <- aov(Total_Spirit_Wellbeing ~ Demographics_relig, data = myData)

# Shapiro-Wilk test
shapiro.test(residuals(model))

# Q-Q plot
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Levene's test
leveneTest(Total_Spirit_Wellbeing ~ Demographics_relig, data = myData)
