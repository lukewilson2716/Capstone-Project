read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data

#Demographic Descriptives
print(summary(myData$Demographics_age))
print(sd(myData$Demographics_age, na.rm = TRUE))
#Demographics for Age - Mean 20.59 SD 1.029448

print(summary(myData$Demographics_rlgsty))
print(sd(myData$Demographics_rlgsty, na.rm = TRUE))
print(table(myData$Demographics_relig))
#Mean Religiosty 3.008, SD 1.5275
#90 in 1(christian), 14 in 2(Islam), 4 in 3(Judaism), 18 in 4(Hindusim), 61 in 6(athiest), 6 in 7(other), 8 in 8(questioning)

print(table(myData$Demographics_sex))
print(table(myData$Demographics_ethnic))
print(table(myData$Demographics_class))
#42 Male, 166 Female
#96 White, 33 Black, 9 Hispanic, 52 Asian, 5 Biracial, 10 Other
#3 Fresh, 14 Soph, 72, Junior, 119 Senior





#Wellbeing Descriptives
#Total_Psych_Wellbeing	Total_Physical_Wellbeing	Total_Emot_Wellbeing	Total_Spirit_Wellbeing	Total_Social_Wellbeing	Total_Int_Wellbeing

print(summary(myData$Total_Psych_Wellbeing))
print(sd(myData$Total_Psych_Wellbeing, na.rm = TRUE))
#Psych Wellbeing Descriptives - Mean 4.606 SD .949958

print(summary(myData$Total_Physical_Wellbeing))
print(sd(myData$Total_Physical_Wellbeing, na.rm = TRUE))
#Physical Wellbeing Descriptives - Mean 4.548 SD 1.1766

print(summary(myData$Total_Spirit_Wellbeing))
print(sd(myData$Total_Spirit_Wellbeing, na.rm = TRUE))
#Spirit Wellbeing Descriptives - Mean 4.967 SD .974

print(summary(myData$Total_Social_Wellbeing))
print(sd(myData$Total_Social_Wellbeing, na.rm = TRUE))
#Social Wellbeing Descriptives - Mean 5.174 SD .9743

print(summary(myData$Total_Emot_Wellbeing))
print(sd(myData$Total_Emot_Wellbeing, na.rm = TRUE))
#Emotional Wellbeing Descriptives - Mean 4.438 SD 1.1296

print(summary(myData$Total_Int_Wellbeing))
print(sd(myData$Total_Int_Wellbeing, na.rm = TRUE))
#Int Wellbeing Descriptives - Mean 5.018 SD .9301
