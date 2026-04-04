read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

#1/3 of data (2023) missing


model1 <- lm(Total_Psych_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model1)
#linear regression for psych wellness and religiosity - nothin

model2 <- lm(Total_Spirit_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model2)
#linear regression for spiritual wellness and religiosity -  STAT SIG result! Although 1/3 of data not there.
#Due to bonferronis if we are counting 6 groups our p should be 0.05/6=0.008, still stat sig but if we looking for more groups it wont be.

model3 <- lm(Total_Social_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model3)
#linear regression for spiritual wellness and religiosity - nothin

model4 <- lm(Total_Emot_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model4)
#linear regression for Emotional wellness and religiosity - Almost

model5 <- lm(Total_Int_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model5)
#linear regression for Int wellness and religiosity - trash lol

model6 <- lm(Total_Physical_Wellbeing ~ myData$Demographics_rlgsty, data = myData)
summary(model6)
#linear regression for physical wellness and religiosity -nothin
