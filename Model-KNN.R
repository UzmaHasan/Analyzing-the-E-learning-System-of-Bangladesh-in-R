dataset <- read.csv("F:\project804\\data.csv", header = FALSE)
dataset

# set the column names in the dataset
colnames(dataset) <- c("Study.Level","Age","Device.Usage","Result.Improved","Knowledge.Increased","Institute.Area", "Internet.Availability", "Internet.Type", "StudyHours.Before", "StudyHours.After","Performance.Increased", "Institute.Type", "Current.Location", "Gender", "Issue.Faced", "Preferred.Device", "Happy")

head(dataset)

##Generate a random number that is 70% of the total number of rows in dataset.
ran <- sample(1:nrow(dataset), 0.7 * nrow(dataset)) 

# use the 70% of data for training
train_set <- dataset[ran,]

# use the remaining 30% of data for testing
test_set <- dataset[-ran,]

##extract 17th column of train dataset because it will be used as 'cl' argument in knn function.
target_category <- dataset[ran,17]

##extract 17th column if test dataset to measure the accuracy
test_category <- dataset[-ran,17]

##load the package class
library(class)

##run the knn function
classifier <- knn(train_set,test_set,cl=target_category,k=10)

##create the confusion matrix
conf <- table(classifier,test_category)
conf

##Find the accuracy of the model. 
#This function divides the correct predictions by total number of predictions.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf)

