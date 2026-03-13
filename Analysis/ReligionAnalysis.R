read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data


myData$Demographics_relig <- as.factor(myData$Demographics_relig)
#Turn Religion Into Groups

model1 <- aov(Total_Int_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model1)
TukeyHSD(model1)
#Anova + Tukey for Post-Hoc - Significant???

model1 <- aov(Total_Emot_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model1)
TukeyHSD(model1)
#Anova + Tukey for Post-Hoc -




