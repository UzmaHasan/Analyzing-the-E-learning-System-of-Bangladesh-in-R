# Load libraries
library(mlbench)
require(gbm)
library(caret)
library(caretEnsemble)

dataset <- read.csv("F:\\data.csv", header = FALSE)

# set the column names in the dataset
colnames(dataset) <- c("Study.Level","Age","Device.Usage","Result.Improved","Knowledge.Increased","Institute.Area", "Internet.Availability", "Internet.Type", "StudyHours.Before", "StudyHours.After","Performance.Increased", "Institute.Type", "Current.Location", "Gender", "Issue.Faced", "Preferred.Device", "Happy")

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
# C5.0
set.seed(seed)
fit.c50 <- train(Happy~., data=dataset, method="C5.0", metric=metric, trControl=control)
# Stochastic Gradient Boosting
set.seed(seed)
fit.gbm <- train(Happy~., data=dataset, method="gbm", metric=metric, trControl=control, verbose=FALSE)
# summarize results
boosting_results <- resamples(list(c5.0=fit.c50, gbm=fit.gbm))
summary(boosting_results)
dotplot(boosting_results)

