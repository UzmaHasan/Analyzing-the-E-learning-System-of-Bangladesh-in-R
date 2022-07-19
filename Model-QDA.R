dataset <- read.csv("F:\\data.csv", header = FALSE)

# set the column names in the dataset
colnames(dataset) <- c("Study.Level","Age","Device.Usage","Result.Improved","Knowledge.Increased","Institute.Area", "Internet.Availability", "Internet.Type", "StudyHours.Before", "StudyHours.After","Performance.Increased", "Institute.Type", "Current.Location", "Gender", "Issue.Faced", "Preferred.Device", "Happy")

##Generate a random number that is 75% of the total number of rows in dataset.
ran <- sample(1:nrow(dataset), 0.75 * nrow(dataset)) 

# use the 75% of data for training & remaining 25% data for testing
train_set <- dataset[ran,]
test_set <- dataset[-ran,]

library(MASS)
library(caret)

#Training the data with a qda() function
model_qda = qda(Happy~., data=train_set)
coef(model_qda)

#predict test data 
pred_qda = predict(model_qda,test_set[,-17])
data.frame(original=test_set$Happy, pred=pred_qda$class)

Happy <- as.factor(test_set$Happy)
confusionMatrix(Happy, pred_qda$class)

