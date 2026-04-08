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


model1 <- aov(Total_Int_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model1)
TukeyHSD(model1)
#Anova + Tukey for Post-Hoc - not sig 

model2 <- aov(Total_Emot_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model2)
TukeyHSD(model2)
#Anova + Tukey for Post-Hoc - Not sig

model3 <- aov(Total_Social_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model3)
TukeyHSD(model3)
# Social - Sig result

model4 <- aov(Total_Spirit_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model4)
TukeyHSD(model4)
# Also sig
#Check 6 vs everyone maybe?

model5 <- aov(Total_Psych_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model5)
TukeyHSD(model5)
# Not sig
#Check 6 vs everyone maybe - interesting boxplot, almost sig

model6 <- aov(Total_Physical_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model6)
TukeyHSD(model6)
# Not sig

