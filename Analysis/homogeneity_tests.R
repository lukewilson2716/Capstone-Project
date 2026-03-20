read.csv("Data/Complete_Data - 1.csv", header = TRUE)
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)

myData$Demographics_relig <- as.factor(myData$Demographics_relig)

library(car)
library(ggplot2)

#Spiritual 
model_spirit <- aov(Total_Spirit_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_spirit))
spirit_mod <- ggplot(data = data.frame(resid = residuals(model_spirit)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  ggtitle("Q-Q Plot: Spiritual Wellbeing Residuals")
print(spirit_mod)

#Psychological 
model_psych <- aov(Total_Psych_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_psych))
psych_mod<- ggplot(data = data.frame(resid = residuals(model_psych)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  ggtitle("Q-Q Plot: Psychological Wellbeing Residuals")
print(psych_mod)


#Social 
model_social <- aov(Total_Social_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_social))
social_mod<- ggplot(data = data.frame(resid = residuals(model_social)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "darkorchid") +
  ggtitle("Q-Q Plot: Social Wellbeing Residuals")
print(social_mod)


#Emotional 
model_emot <- aov(Total_Emot_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_emot))
emot_mod<- ggplot(data = data.frame(resid = residuals(model_emot)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "darkolivegreen4") +
  ggtitle("Q-Q Plot: Emotional Wellbeing Residuals")
print(emot_mod)

#Intellectual
model_int <- aov(Total_Int_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_int))
int_mod<- ggplot(data = data.frame(resid = residuals(model_int)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "orange") +
  ggtitle("Q-Q Plot: Intellectual Wellbeing Residuals")
print(int_mod)

#Physical 
model_phys <- aov(Total_Physical_Wellbeing ~ Demographics_relig, data = myData)
shapiro.test(residuals(model_phys))
physical_mod<- ggplot(data = data.frame(resid = residuals(model_phys)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "deeppink3") +
  ggtitle("Q-Q Plot: Physical Wellbeing Residuals")
print(physical_mod)



