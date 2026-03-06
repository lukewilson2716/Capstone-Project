read.csv("2025Data - 1-2 (1).csv")
myData <- read.csv("2025Data - 1-2 (1).csv", skip = 1, header = TRUE)
myData$Demographics_age <- as.numeric(myData$Demographics_age)

summary(myData$Demographics_age)
summary(myData)

