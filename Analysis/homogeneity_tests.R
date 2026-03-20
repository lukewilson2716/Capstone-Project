read.csv("Data/Complete_Data - 1.csv", header = TRUE)
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)

myData$Demographics_relig <- as.factor(myData$Demographics_relig)

library(car)

#Spiritual 
model_spirit <- aov(Total_Spirit_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_spirit))
qqnorm(residuals(model_spirit))
qqline(residuals(model_spirit), col = "red")
leveneTest(Total_Spirit_Wellbeing ~ Demographics_relig, data = myData)

#Psychological 
model_psych <- aov(Total_Psych_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_psych))
qqnorm(residuals(model_psych))
qqline(residuals(model_psych), col = "red")
leveneTest(Total_Psych_Wellbeing ~ Demographics_relig, data = myData)

#Social 
model_social <- aov(Total_Social_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_social))
qqnorm(residuals(model_social))
qqline(residuals(model_social), col = "red")
leveneTest(Total_Social_Wellbeing ~ Demographics_relig, data = myData)

#Emotional 
model_emot <- aov(Total_Emot_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_emot))
qqnorm(residuals(model_emot))
qqline(residuals(model_emot), col = "red")
leveneTest(Total_Emot_Wellbeing ~ Demographics_relig, data = myData)

#Intellectual
model_int <- aov(Total_Int_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_int))
qqnorm(residuals(model_int))
qqline(residuals(model_int), col = "red")
leveneTest(Total_Int_Wellbeing ~ Demographics_relig, data = myData)

#Physical 
model_phys <- aov(Total_Physical_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_phys))
qqnorm(residuals(model_phys))
qqline(residuals(model_phys), col = "red")
leveneTest(Total_Physical_Wellbeing ~ Demographics_relig, data = myData)


