read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data


boxplot(Total_Spirit_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")

boxplot(Total_Emot_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")

boxplot(Total_Int_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")

boxplot(Total_Social_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")

boxplot(Total_Psych_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")

boxplot(Total_Physical_Wellbeing ~ Demographics_relig, data = myData, 
        main="Outcome by Religion", col="lightblue")


