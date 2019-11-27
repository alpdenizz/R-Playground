trainData <- read.csv("train.csv")
valData <- read.csv("val.csv")
testData <- read.csv("test.csv")

library(ggplot2)
ggplot(trainData, aes(x=V1,y=V2,color=y)) + geom_point(alpha = 0.5, size = 2) + theme_bw()

library(caret)
set.seed(10) # important for correct behaviour!
ctrl <- trainControl(method="none", number = 1, repeats = 1, classProbs = TRUE)
rf_fit <- train(make.names(y)~., data = trainData, method = 'rf', trControl = ctrl)

prediction <- predict(rf_fit, valData)
prediction <- as.character(prediction)
actual <- valData$y
actual[actual==0] <- "X0"
actual[actual==1] <- "X1"
probs <- predict(rf_fit, newdata = valData, type = 'prob')[,2]
cutoffs <- sort(unique(probs))

tpr <- sapply(cutoffs, function(cut) sum((probs >= cut) == TRUE & valData$y == 1)/(sum(valData$y == 1)))
fpr <- sapply(cutoffs, function(cut) sum((probs >= cut) == TRUE & valData$y == 0)/(sum(valData$y == 0)))
stats <- data.frame(cutoffs, tpr, fpr)
stats[6,]

roc_plot <- ggplot(stats, aes(x = fpr, y = tpr)) + geom_path() + theme_bw()
roc_plot

rf_dt <- data.frame(class = predict(rf_fit, newdata = valData, type = 'raw'), probs = probs)
rf_dt = rf_dt[order(probs),]

accuracy <- sum(prediction == actual)/length(actual)

roc_plot <- roc_plot +
  geom_point(data=stats[which(stats$cutoffs == 0.5), ], aes(x=fpr, y=tpr), colour="red", size=3) +
  geom_label(data = stats[which(stats$cutoffs == 0.5), ], label = 'default_cutoff', nudge_y = 0.05)
roc_plot

fp_count <- sapply(cutoffs, function(cut) sum((probs >= cut) == TRUE & valData$y == 0))
fn_count <- sapply(cutoffs, function(cut) sum((probs >= cut) == FALSE & valData$y == 1))
acc <- sapply(cutoffs, function(cut) (sum((probs >= cut) == FALSE & valData$y == 0) + sum((probs >= cut) == TRUE & valData$y == 1))/length(probs))
stats <- data.frame(stats, fp_count, fn_count, acc)
################
stats$cost <- stats$fp_count + stats$fn_count
stats[which.min(stats$cost),]
cutoffA <- stats[which.min(stats$cost),1]
accA <- stats[which.min(stats$cost),6]

roc_plot <- roc_plot + geom_point(data=stats[which.min(stats$cost),], aes(x=fpr, y=tpr), colour="blue", size=3) +
  geom_label(data = stats[which.min(stats$cost),], label = stats$cutoffs[which.min(stats$cost)], nudge_y = 0.05)
roc_plot
###############
stats$cost <- 3*(stats$fp_count) + (stats$fn_count)
cutoffB <- stats[which.min(stats$cost),1]
accB <- stats[which.min(stats$cost),6]

roc_plot <- roc_plot + geom_point(data=stats[which.min(stats$cost),], aes(x=fpr, y=tpr), colour="yellow", size=3) +
  geom_label(data = stats[which.min(stats$cost),], label = stats$cutoffs[which.min(stats$cost)], nudge_y = 0.05)
roc_plot
##################
stats$cost <- (stats$fp_count) + (3*stats$fn_count)
cutoffC <- stats[which.min(stats$cost),1]
accC <- stats[which.min(stats$cost),6]

roc_plot <- roc_plot + geom_point(data=stats[which.min(stats$cost),], aes(x=fpr, y=tpr), colour="green", size=3) +
  geom_label(data = stats[which.min(stats$cost),], label = stats$cutoffs[which.min(stats$cost)], nudge_y = 0.05)
roc_plot
###################
prediction <- predict(rf_fit, testData)
prediction <- as.character(prediction)
actual <- testData$y
actual[actual==0] <- "X0"
actual[actual==1] <- "X1"
accuracy <- sum(prediction == actual)/length(actual)
probs <- predict(rf_fit, newdata = testData, type = 'prob')[,2]
rf_dt <- data.frame(class = predict(rf_fit, newdata = testData, type = 'raw'), probs = probs)
rf_dt = rf_dt[order(probs),]
accDefCutoff <- (sum((probs >= 0.5) == FALSE & testData$y == 0) + sum((probs >= 0.5) == TRUE & testData$y == 1))/length(probs)
accOptCutoff <- (sum((probs >= cutoffA) == FALSE & testData$y == 0) + sum((probs >= cutoffA) == TRUE & testData$y == 1))/length(probs)
################Task 2####
regData <- read.csv("ex2.csv")
regPlot <- ggplot(regData, aes(x=feature1,y=label)) + geom_point(alpha = 0.5, size = 2) + theme_bw()
f1 <- function(x) {
  return(500)
}
f2 <- function(x) {
  return(40*x-300)
}
f3 <- function(x) {
  return(x^2 + 2*x)
}
regPlot + stat_function(fun=f3, colour="red")
xx <- regData$feature1
yy <- regData$label
mse1 <- mean((yy-500)^2)
mse2 <- mean((yy-f2(xx))^2)
mse3 <- mean((yy-f3(xx))^2)
rmse1 <- sqrt(mse1)
rmse2 <- sqrt(mse2)
rmse3 <- sqrt(mse3)
#####################################
mpgData <- read.csv("auto_mpg_cleaned.csv",sep=";")
d_train <- mpgData[1:196,]
d_test <- mpgData[197:392,]
MSE = function(model,data) mean((data$mpg-predict(model,newdata=data))^2)
empty_model = lm(formula = mpg ~ 1,data = d_train)
full_model = lm(formula = mpg ~ .,data = d_train[,-9])
add_to_report = function(report,model,d_train,d_test) {
  formula_string = as.character(formula(model)[3])
  n_features = (length(strsplit(formula_string,split=" ")[[1]])+1)/2
  df = data.frame(formula=formula_string,n_features=n_features,train_mse=MSE(model,d_train),test_mse=MSE(model,d_test))
  report = rbind(report,df)
  return(report)
}
report = data.frame()
report = add_to_report(report,empty_model,d_train[,-9],d_test[,-9])
report = add_to_report(report,full_model,d_train[,-9],d_test[,-9])
print(report)

report = data.frame()
report = add_to_report(report,empty_model,d_train[,-9],d_test[,-9])
report = add_to_report(report,full_model,d_train[,-9],d_test[,-9])
print(report)
summary(full_model)
report = add_to_report(report,model,d_train[,-9],d_test[,-9])
model = step(empty_model,direction='forward',scope=formula(full_model),steps=6)