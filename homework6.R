predictions <- read.csv("predictions.csv")
actualClasses <- predictions$Actual
predictionsA <- predictions$A
predictionsB <- predictions$B
actual0 <- which(actualClasses==0)
actual1 <- which(actualClasses==1)
predictA_0 <- which(predictionsA==0)
predictA_1 <- which(predictionsA==1)
predictB_0 <- which(predictionsB==0)
predictB_1 <- which(predictionsB==1)
total <- length(actualClasses)

true_positive_A <- length(intersect(actual1,predictA_1))
true_negative_A <- length(intersect(actual0,predictA_0))
false_positive_A <- length(intersect(actual0,predictA_1))
false_negative_A <- length(intersect(actual1,predictA_0))

accuracy_A <- (true_positive_A+true_negative_A) / total 
precision_A <- true_positive_A / length(predictA_1)
recall_A <- true_positive_A / length(actual1)
f_measure_A <- 2 / ((1/precision_A)+(1/recall_A))

true_positive_B <- length(intersect(actual1,predictB_1))
true_negative_B <- length(intersect(actual0,predictB_0))
false_positive_B <- length(intersect(actual0,predictB_1))
false_negative_B <- length(intersect(actual1,predictB_0))

accuracy_B <- (true_positive_B+true_negative_B) / total 
precision_B <- true_positive_B / length(predictB_1)
recall_B <- true_positive_B / length(actual1)
f_measure_B <- 2 / ((1/precision_B)+(1/recall_B))


data = data.frame(
  Actual_Predicted = c("Disease (1)","No Disease (0)","Total"),
  Disease_1 = c(paste("TP: ",true_positive_A),paste("FN: ",false_negative_A),length(predictA_1)),
  No_Disease_0 = c(paste("FP: ",false_positive_A),paste("TN: ",true_negative_A),length(predictA_0)),
  Total = c(length(actual1),length(actual0),total))

colnames(data) <- c("ACTUAL\\PREDICTED","DISEASE (1)","NO DISEASE (0)","TOTAL")
data$`ACTUAL\\PREDICTED` <- c("DISEASE (1)","NO DISEASE (0)","TOTAL")

########################Task 2#######
library(caret)
training <- read.csv("training.csv")
testing1 <- read.csv("testing_1.csv")
testing2 <- read.csv("testing_2.csv")

ctrl <- trainControl(method="none", number = 1, repeats = 1)
(rf_fit <- train(as.factor(y)~., data = training, method = 'rf', trControl = ctrl))
predicted_y1 <- predict(rf_fit, testing1)
predicted_y2 <- predict(rf_fit, testing2)
actual_y1 <- testing1$y
actual_y2 <- testing2$y
sum(predicted_y1 == actual_y1)/length(actual_y1)


confMatrix <- confusionMatrix(predicted_y1, actual_y1)
confMatrix$table
confMatrix <- confusionMatrix(predicted_y2, actual_y2)
confMatrix$table[2,2]

training.balanced.over <- ovun.sample(as.factor(y)~., data=training, N=1900, method = "over")
training.balanced.under <- ovun.sample(as.factor(y)~., data=training, N=100, method = "under")
training.balanced <- ovun.sample(as.factor(y)~., data=training, N=1000, p=0.5, method = "both")
overSampleTraining <- training.balanced.over$data
underSampleTraining <- training.balanced.under$data
bothOverUnderTraining <- training.balanced$data

(rf_fit <- train(as.factor(y)~., data = overSampleTraining, method = 'rf', trControl = ctrl))
predicted_y1 <- predict(rf_fit, testing1)
predicted_y2 <- predict(rf_fit, testing2)
confusionMatrix(predicted_y1, actual_y1)
confusionMatrix(predicted_y2, actual_y2)

(rf_fit <- train(as.factor(y)~., data = underSampleTraining, method = 'rf', trControl = ctrl))
predicted_y1 <- predict(rf_fit, testing1)
predicted_y2 <- predict(rf_fit, testing2)
confusionMatrix(predicted_y1, actual_y1)
confusionMatrix(predicted_y2, actual_y2)

(rf_fit <- train(as.factor(y)~., data = bothOverUnderTraining, method = 'rf', trControl = ctrl))
predicted_y1 <- predict(rf_fit, testing1)
predicted_y2 <- predict(rf_fit, testing2)
confusionMatrix(predicted_y1, actual_y1)
confusionMatrix(predicted_y2, actual_y2)

###################Task 3##################
set.seed(1111)
x_20 <- runif(20,min=0, max=10)
y_20 <- runif(20,min=0, max=10)
class20 <- c(rep(1,20))
for (index in 1:20) {
  x <- x_20[index]
  y <- y_20[index]
  if ( ((x-5)^2 + (y-5)^2 - 9) > 0 ){
    class20[index] <- 0
  }
}

data20 <- cbind(x_20,y_20,class20)
colnames(data20) <- c("x_coord","y_coord","label")
dataFrame20 = as.data.frame(data20)
dataFrame20$la
ggplot(dataFrame20,aes(x=x_coord,y=y_coord,col = Label)) + coord_fixed() + 
  geom_point(aes(colour = factor(label)))

x_100 <- runif(100,min=0, max=10)
y_100 <- runif(100,min=0, max=10)
class100 <- c(rep(1,100))
for (index in 1:100) {
  x <- x_100[index]
  y <- y_100[index]
  if ( ((x-5)^2 + (y-5)^2 - 9) > 0 ){
    class100[index] <- 0
  }
}

data100 <- cbind(x_100,y_100,class100)
colnames(data100) <- c("x_coord","y_coord","label")
dataFrame100 = as.data.frame(data100)
ggplot(dataFrame100,aes(x=x_coord,y=y_coord,col = Label)) + coord_fixed() + 
  geom_point(aes(colour = factor(label)))

xx <- seq(0,10,by=0.25)
yy <- seq(0,10,by=0.25)
points <- expand.grid(x=xx, y=yy)
testingDataset <- points
colnames(testingDataset) <- c("x_coord","y_coord")
ctrl <- trainControl(method="none", number = 1, repeats = 1)
(knn_fit20 <- train(y = as.factor(dataFrame20$label), x = dataFrame20[,-3], method = "knn", trControl = ctrl, tuneGrid = data.frame(k = 10)))
(knn_fit100 <- train(y = as.factor(dataFrame100$label), x = dataFrame100[,-3], method = "knn", trControl = ctrl, tuneGrid = data.frame(k = 10)))
(rf_fit20 <- train(as.factor(label)~., data = dataFrame20, method = 'rf', trControl = ctrl))
(rf_fit100 <- train(as.factor(label)~., data = dataFrame100, method = 'rf', trControl = ctrl))
(svmL_fit20 <- train(as.factor(label)~., data = dataFrame20, method = 'svmLinear', trControl = ctrl))
(svmL_fit100 <- train(as.factor(label)~., data = dataFrame100, method = 'svmLinear', trControl = ctrl))
(svmR_fit20 <- train(as.factor(label)~., data = dataFrame20, method = 'svmRadial', trControl = ctrl))
(svmR_fit100 <- train(as.factor(label)~., data = dataFrame100, method = 'svmRadial', trControl = ctrl))

predictedLabels <- predict(svmL_fit20, testingDataset)
testingDataset[,3] <- predictedLabels
colnames(testingDataset) <- c("x_coord","y_coord","label")
ggplot(testingDataset,aes(x=x_coord,y=y_coord,col = label)) + coord_fixed() + 
  geom_point(aes(colour = factor(label)))
predictedLabels <- predict(svmR_fit20, testingDataset)
testingDataset[,3] <- predictedLabels
colnames(testingDataset) <- c("x_coord","y_coord","label")
ggplot(testingDataset,aes(x=x_coord,y=y_coord,col = label)) + coord_fixed() + 
  geom_point(aes(colour = factor(label)))

predictedLabels <- predict(svmR_fit20, testingDataset)
testingDataset[,3] <- predictedLabels
colnames(testingDataset) <- c("x_coord","y_coord","label")
ggplot(testingDataset,aes(x=x_coord,y=y_coord,col = label)) + coord_fixed() + 
  geom_point(aes(colour = factor(label)))