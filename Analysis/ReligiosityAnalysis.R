read.csv("Data/Complete_Data - 1.csv")
# Read the file, Data/ to access it in the folder
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)
# Assign File to something, header true needed to count first row as header not data


model2 <- lm(Psychological.Wellness ~ myData$Demographics_rlgsty, data = myData)
#linear regression for psych wellness and religiosity
summary(model2)
#