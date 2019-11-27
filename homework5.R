######################Task1##################
id = matrix(c(1,2,3,4,5,6,7,8,9,10,11))
x_axis = matrix(c(3,2,3,1,4,9,5,6,6,3,3))
y_axis = matrix(c(9,4,3,6,1,3,6,4,2,7,5))
class = matrix(c(1,1,1,1,0,0,0,0,0,0,NA))
data = cbind(id,x_axis,y_axis,class)
colnames(data) <- c("id","x_coord","y_coord","class")
dataFrame = as.data.frame(data)
dataFrame$class = (c(" +"," +"," +"," +"," -"," -"," -"," -"," -"," -","class=?"))
ggplot(dataFrame,aes(x=x_coord,y=y_coord,label = paste(class,id,sep = "\n"))) + geom_text() + coord_fixed() + 
  scale_x_continuous(minor_breaks = seq(0 , 10, 1), breaks = seq(0, 10, 1)) +
  scale_y_continuous(minor_breaks = seq(0 , 10, 1), breaks = seq(0, 10, 1)) + geom_point()
################################Task2##################
mnist <- readRDS("MNIST.RDS")
str(mnist$y)
table(mnist$y)[10]
table(mnist$x)

colors <- c('black', 'white')
cus_col <- colorRampPalette(colors=colors)
index <- 1
img <- array(mnist$x[index,],dim=c(28,28))
img <- img[,28:1]
image(img, col=cus_col(256))

head(mnist$y)
print(paste("Correct label of the first image is:", mnist$y[1]))
ggplot(as.data.frame(mnist),aes(y)) + geom_bar() + 
  scale_x_continuous(minor_breaks = seq(0 , 9, 1), breaks = seq(0, 9, 1)) +
  scale_y_continuous(breaks = seq(0, 7000, 1000))

pixel <- data.frame('value' = mnist$x[,400], 'classes' = mnist$y)

g <- ggplot(pixel, aes(x = value)) +
     geom_histogram(binwidth = 0.1) +
     theme_bw()

g + facet_wrap(~ classes) 


################################Task 3############
set.seed(1111)
new_indx <- sample(c(1:nrow(mnist$x)), size = 4000, replace = FALSE)

sample_img <- mnist$x[new_indx, ]
sample_labels <- mnist$y[new_indx]

train_img <- sample_img[1:3000,]
train_labels <- sample_labels[1:3000]

test_img <- sample_img[3001:4000,]
test_labels <- sample_labels[3001:4000]

str(train_img) # Make sure you have 3000 rows here
str(test_img) # Make sure you have 1000 rows here

dist <- function(img1, img2) {
  # TODO
  # calculate the euclidean distance between img1 and img2
  return(sqrt(sum((img1-img2)^2)))
}

unknown_img <- test_img[1,]
true_label <- test_labels[1]
all_distances <- apply(train_img, 1, function(img) dist(unknown_img, img))
closest_index <- which(all_distances==min(all_distances))
predicted_label <- train_labels[closest_index]
(predicted_label == true_label)
print(paste("Predicted class for the first image is", predicted_label ,"and the true label is",  true_label))

unknown_img <- train_img[15,]
index <- 0
for (ind in 1:3000) {
  if (identical(train_img[ind,],unknown_img)) {
    index <- ind
  }
}

img <- array(unknown_img,dim=c(28,28))
img <- img[,28:1]
image(img, col=cus_col(256))
classify <- function(unknown_img) {
  all_distances <- apply(train_img, 1, function(img) dist(unknown_img, img))
  closest_index <- which(all_distances==min(all_distances))
  return(train_labels[closest_index]) # REPORT A LABEL OF THE CLOSEST NEIGHBOUR
}
print(paste("Predicted class for the first image is", classify(unknown_img),"and the true label is",  true_label))

classify_knn <- function(unknown_img, k) {
  # This step we already know from the previous exercises
  all_distances <- apply(train_img, 1, function(img) dist(unknown_img, img))
    
    # We need to get indexes of K smallest distances
    # (hint: use functions order() and head())
    knn = head(order(all_distances),k)
    
    # you can print potential predictions
    #print(train_labels[knn])
    #print(names(sort(table(train_labels[knn]), decreasing = TRUE)))
    
    # Very small step is left, return the most frequently predicted label
    return(names(sort(table(train_labels[knn]), decreasing = TRUE))[1])
}


print(paste("Predicted class for the first image is", classify_knn(unknown_img, k = 1),"and the true label is",  true_label))
#####################Task 4#############
test_predicted <- apply(train_img, 1, function(img) classify_knn(img, k= 5))
predictedClasses = as.integer(test_predicted)
actualClasses = train_labels
knn_accuracy = length(which((predictedClasses-actualClasses)==0)) / length(actualClasses)
print(paste("Final accuracy of our nearest neighbor classifier is", knn_accuracy,"- not bad!"))

# Set an index of missclassified instance you want to examine
index = 10

miss_ind = which(test_predicted != test_labels)[index] # remember function `which` in R?

colors<-c('black','white')
cus_col<-colorRampPalette(colors=colors)

img <- array(test_img[miss_ind,],dim=c(28,28))
img <- img[,28:1]
image(img, col=cus_col(256))
print(paste("This image has a class", test_labels[miss_ind],"was incorrectly predicted as",  test_predicted[miss_ind]))

# We will discuss this line further in more details,
# we need it now as without it, caret tries to be very smart
# and training takes too much time...
ctrl <- trainControl(method="none", number = 1, repeats = 1)

# we should use train_labels as factors, otherwise caret thinks that
# this is a regression problem
(knn_fit <- train(y = as.factor(train_labels), x = data.frame(train_img), method = "knn", trControl = ctrl, tuneGrid = data.frame(k = 5)))
test_predicted = predict(knn_fit, data.frame(train_img))
print(paste("Accuracy of caret nearest neighbor classifier is", sum(test_predicted == train_labels)/length(train_labels)))

confusionMatrix(test_predicted, train_labels)

grid <- expand.grid( .winnow = c(FALSE), .trials=1, .model="tree" )
(knn_fit <- train(y = as.factor(train_labels), x = data.frame(train_img), method = "C5.0", trControl = ctrl, 
                  tuneGrid = grid))
test_predicted = predict(knn_fit, data.frame(test_img))
print(paste("Accuracy of caret nearest neighbor classifier is", sum(test_predicted == test_labels)/length(test_labels)))

confusionMatrix(test_predicted, test_labels)

listem
if (is.element(5,listem)) {
  listem[-(which(listem==5))]
}
identical(c(1:5),c(1:4))