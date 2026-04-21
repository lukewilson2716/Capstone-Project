read.csv("Data/Complete_Data - 1.csv", header = TRUE)
myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)

myData$Demographics_relig <- as.factor(myData$Demographics_relig)

library(ggplot2)
library(DiagrammeR)

myData <- read.csv("Data/Complete_Data - 1.csv", header = TRUE)

wellbeing_vars <- c(
  "Total_Spirit_Wellbeing",
  "Total_Psych_Wellbeing",
  "Total_Social_Wellbeing",
  "Total_Emot_Wellbeing",
  "Total_Int_Wellbeing",
  "Total_Physical_Wellbeing"
)

plot_data <- na.omit(myData[, wellbeing_vars])

pca <- prcomp(plot_data, scale. = TRUE)

var_exp <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 1)

grViz(paste0("
digraph {
  graph [layout = dot, rankdir = LR]

  s [label='Spiritual']
  p [label='Psychological']
  so [label='Social']
  e [label='Emotional']
  i [label='Intellectual']
  ph [label='Physical']

  pc1 [label='PC1\\n", var_exp[1], "%']
  pc2 [label='PC2\\n", var_exp[2], "%']

  s -> pc1; p -> pc1; so -> pc1; e -> pc1; i -> pc1; ph -> pc1;
  s -> pc2; p -> pc2; so -> pc2; e -> pc2; i -> pc2; ph -> pc2;
}
"))
