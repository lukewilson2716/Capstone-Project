read.csv("Data/new_complete_data - Sheet1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/new_complete_data - Sheet1.csv", header = TRUE)

print(myData)

library(dplyr)

# 1. Underclassmen Percentage (Class 1 or 2)
underclassmen_christian_count <- myData %>%
  filter(Demographics_class %in% c(1, 2)) %>%
  filter(Demographics_relig == 1) %>%
  nrow()

underclassmen_percent <- (underclassmen_christian_count / 88) * 100

# 2. Upperclassmen Percentage (Class 3 or 4)
upperclassmen_christian_count <- myData %>%
  filter(Demographics_class %in% c(3, 4)) %>%
  filter(Demographics_relig == 1) %>%
  nrow()

upperclassmen_percent <- (upperclassmen_christian_count / 243) * 100

# Print Results
cat("Underclassmen Christians (Class 1-2):", underclassmen_christian_count, "out of 88 (", round(underclassmen_percent, 2), "%)\n")
cat("Upperclassmen Christians (Class 3-4):", upperclassmen_christian_count, "out of 243 (", round(upperclassmen_percent, 2), "%)\n")

#Demographic Descriptives
print(summary(myData$Demographics_age))
print(sd(myData$Demographics_age, na.rm = TRUE))
#Demographics for Age - Mean 20.59 SD 1.029448 - NOT UPDATED

print(summary(myData$Demographics_rlgsty))
print(sd(myData$Demographics_rlgsty, na.rm = TRUE))
print(table(myData$Demographics_relig))
#Mean Religiosty 2.73, SD 1.492
#131 in 1(christian), 16 in 2(Islam), 7 in 3(Judaism), 37 in 4(Hindusim), 94 in 6(athiest), 10 in 7(other), 14 in 8(questioning)

print(table(myData$Demographics_sex))
print(table(myData$Demographics_ethnic))
print(table(myData$Demographics_class))
#93 Male, 241 Female
#96 White, 33 Black, 9 Hispanic, 52 Asian, 5 Biracial, 10 Other - NOT UPDATED
#55 Fresh, 33 Soph, 107, Junior, 136 Senior, 4 other





#Wellbeing Descriptives
#Total_Psych_Wellbeing	Total_Physical_Wellbeing	Total_Emot_Wellbeing	Total_Spirit_Wellbeing	Total_Social_Wellbeing	Total_Int_Wellbeing

print(summary(myData$Total_Psych_Wellbeing))
print(sd(myData$Total_Psych_Wellbeing, na.rm = TRUE))
#Psych Wellbeing Descriptives - Mean 4.251 SD .9398 - Drop off

print(summary(myData$Total_Physical_Wellbeing))
print(sd(myData$Total_Physical_Wellbeing, na.rm = TRUE))
#Physical Wellbeing Descriptives - Mean 4.393 SD 1.023 - drop off

print(summary(myData$Total_Spirit_Wellbeing))
print(sd(myData$Total_Spirit_Wellbeing, na.rm = TRUE))
#Spirit Wellbeing Descriptives - Mean 4.666 SD .9861

print(summary(myData$Total_Social_Wellbeing))
print(sd(myData$Total_Social_Wellbeing, na.rm = TRUE))
#Social Wellbeing Descriptives - Mean 5.119 SD .8389

print(summary(myData$Total_Emot_Wellbeing))
print(sd(myData$Total_Emot_Wellbeing, na.rm = TRUE))
#Emotional Wellbeing Descriptives - Mean 4.378 SD 9.497

print(summary(myData$Total_Int_Wellbeing))
print(sd(myData$Total_Int_Wellbeing, na.rm = TRUE))
#Int Wellbeing Descriptives - Mean 4.777 SD .8525
