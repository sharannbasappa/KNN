
# drop the id feature
wbcd <- wbcd[-1]

# Exploratory Data Analysis

# table of diagnosis
table(wbcd$diagnosis)

str(wbcd$diagnosis)
# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 2)

# summarize any three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(0.01, 0.02, 0.03, 0.04, 0.05))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

# confirm that normalization worked
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_train_labels <- wbcd_train_labels[["diagnosis"]] 

wbcd_test_labels <- wbcd[470:569, 1]
wbcd_test_labels <- wbcd_test_labels[["diagnosis"]]
#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k = 21)


## ---- Evaluating model performance ---- ##
confusion_test <- table(x = wbcd_test_labels, y = wbcd_test_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy 

# Training Accuracy to compare against test accuracy
wbcd_train_pred <- knn(train = wbcd_train, test = wbcd_train, cl = wbcd_train_labels, k=21)

confusion_train <- table(x = wbcd_train_labels, y = wbcd_train_pred)
confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

## Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)


# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)


########################################################
pred.train <- NULL
pred.val <- NULL
error_rate.train <- NULL
error_rate.val <- NULL
accu_rate.train <- NULL
accu_rate.val <- NULL
accu.diff <- NULL
error.diff <- NULL

for (i in 1:39) {
     pred.train <- knn(train = wbcd_train, test = wbcd_train, cl = wbcd_train_labels, k = i)
     pred.val <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = i)
     error_rate.train[i] <- mean(pred.train!=wbcd_train_labels)
     error_rate.val[i] <- mean(pred.val != wbcd_test_labels)
     accu_rate.train[i] <- mean(pred.train == wbcd_train_labels)
     accu_rate.val[i] <- mean(pred.val == wbcd_test_labels)  
     accu.diff[i] = accu_rate.train[i] - accu_rate.val[i]
     error.diff[i] = error_rate.val[i] - error_rate.train[i]
}

knn.error <- as.data.frame(cbind(k = 1:39, error.train = error_rate.train, error.val = error_rate.val, error.diff = error.diff))
knn.accu <- as.data.frame(cbind(k = 1:39, accu.train = accu_rate.train, accu.val = accu_rate.val, accu.diff = accu.diff))

library(ggplot2)
errorPlot = ggplot() + 
     geom_line(data = knn.error[, -c(3,4)], aes(x = k, y = error.train), color = "blue") +
     geom_line(data = knn.error[, -c(2,4)], aes(x = k, y = error.val), color = "red") +
     geom_line(data = knn.error[, -c(2,3)], aes(x = k, y = error.diff), color = "black") +
     xlab('knn') +
     ylab('ErrorRate')
accuPlot = ggplot() + 
     geom_line(data = knn.accu[,-c(3,4)], aes(x = k, y = accu.train), color = "blue") +
     geom_line(data = knn.accu[,-c(2,4)], aes(x = k, y = accu.val), color = "red") +
     geom_line(data = knn.accu[,-c(2,3)], aes(x = k, y = accu.diff), color = "black") +
     xlab('knn') +
     ylab('AccuracyRate')

# Plot for Accuracy
plot(knn.accu[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInAccu") 

# Plot for Error
plot(knn.error[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInError") 


