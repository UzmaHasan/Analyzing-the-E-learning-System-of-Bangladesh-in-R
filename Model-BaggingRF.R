# Load libraries
library(ipred)
library(caret)

dataset <- read.csv("F:\\datum.csv", header = FALSE)

# set the column names in the dataset
colnames(dataset) <- c("Study.Level","Age","Device.Usage","Result.Improved","Knowledge.Increased","Institute.Area", "Internet.Availability", "Internet.Type", "StudyHours.Before", "StudyHours.After","Performance.Increased", "Institute.Type", "Current.Location", "Gender", "Issue.Faced", "Preferred.Device", "Happy")

set.seed(12)

indexes <- createDataPartition(dataset$Happy, p = .9, list = F)
train <- dataset[indexes, ]
test <- dataset[-indexes, ]

fit <- bagging(Happy~ Knowledge.Increased + Result.Improved, data = train, coob = T, nbagg = 100)

print(fit)

pred <- predict(fit, test)

result <- data.frame(original = test$Happy, predicted = pred)
print(result)