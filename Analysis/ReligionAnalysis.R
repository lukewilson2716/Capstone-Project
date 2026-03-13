read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data


myData$Demographics_relig <- as.factor(myData$Demographics_relig)
#Turn Religion Into Groups

model1 <- aov(Total_Int_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model1)
TukeyHSD(model1)
boxplot(Total_Int_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")
#Anova + Tukey for Post-Hoc - Significant??? No Tukey tho 8 and 2 most diff but 8 only has like 2 people

model2 <- aov(Total_Emot_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model2)
TukeyHSD(model2)
#Anova + Tukey for Post-Hoc -

model3 <- aov(Total_Social_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model3)
TukeyHSD(model3)
# Social Also sig? I might have done sum wrong

model4 <- aov(Total_Spirit_Wellbeing ~ myData$Demographics_relig, data = myData)
summary(model4)
TukeyHSD(model4)
# Also sig I def did something wrong



