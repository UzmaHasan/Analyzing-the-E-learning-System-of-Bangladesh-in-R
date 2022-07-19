dataset <- read.csv("F:\\data.csv", header = FALSE)

# set the column names in the dataset
colnames(dataset) <- c("Study.Level","Age","Device.Usage","Result.Improved","Knowledge.Increased","Institute.Area", "Internet.Availability", "Internet.Type", "StudyHours.Before", "StudyHours.After","Performance.Increased", "Institute.Type", "Current.Location", "Gender", "Issue.Faced", "Preferred.Device", "Happy")

#upsampling: to increase the size of the minority class
set.seed(111)
Happy <- as.factor(dataset$Happy)
dataset<-upSample(x=dataset[,-ncol(dataset)],
                  y=Happy)
table(dataset$Class)

# create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Happy, p=0.70, list=FALSE)

# select 20% of the data for validation
validation <- dataset[-validation_index,]

# use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# split input and output
x <- dataset[,1:16]
y <- dataset[,17]

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

library(caret)

# LDA
set.seed(7)
fit.lda <- train(Happy~., data=dataset, method="lda", metric=metric, trControl=control)
# CART
set.seed(7)
fit.cart <- train(Happy~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Happy~., data=dataset, method="knn", metric=metric, trControl=control)
# SVM
set.seed(7)
fit.svm <- train(Happy~., data=dataset, method="svmRadial", metric=metric, trControl=control)

#summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm))
summary(results)

# compare accuracy of models
dotplot(results)






# summarize Best Model
print(fit.lda)

Happy <- as.factor(validation$Happy)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
#confusionMatrix(predictions, validation$Happy)
confusionMatrix(predictions, Happy)
