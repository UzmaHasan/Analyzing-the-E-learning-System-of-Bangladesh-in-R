dataset <- read.csv("F:\project804\\data.csv", header = FALSE)
dataset

# set the column names in the dataset
colnames(dataset) <- c("Study.Level","Age","Device.Usage","Result.Improved","Knowledge.Increased","Institute.Area", "Internet.Availability", "Internet.Type", "StudyHours.Before", "StudyHours.After","Performance.Increased", "Institute.Type", "Current.Location", "Gender", "Issue.Faced", "Preferred.Device", "Happy")

##Generate a random number that is 75% of the total number of rows in dataset.
ran <- sample(1:nrow(dataset), 0.75 * nrow(dataset)) 

# use the 75% of data for training
train_set <- dataset[ran,]

# use the remaining 25% of data for testing
test_set <- dataset[-ran,]

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = Happy ~ .,data = train_set, scale = TRUE,
                 type = 'C-classification',kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set)

# Making the Confusion Matrix
conf = table(test_set[, 17], y_pred)
conf

##Find the accuracy of the model. 
#This function divides the correct predictions by total number of predictions.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf)
