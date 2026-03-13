read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

#Demographic Descriptives
print(summary(myData$Demographics_age))
print(sd(myData$Demographics_age, na.rm = TRUE))
#Demographics for Age


#Wellbeing Descriptives
print(summary(myData$Total_Emot_Wellbeing))
print(sd(myData$Total_Emot_Wellbeing))
#Emotional Wellbeing Descriptives

print(summary(myData$Total_Int_Wellbeing))
print(sd(myData$DTotal_Int_Wellbeing, na.rm = TRUE))
#Int Wellbeing Descriptives

