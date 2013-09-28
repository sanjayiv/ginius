
require('randomForest')
data.orig <- read.csv(file="dev_sample.csv", header=T)
data <- na.roughfix(data.orig)
data$default_flag_factor <- as.factor(data$default_flag)
set.seed(111)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.5, 0.25, 0.25))
tr.data.x <- data[ind==1,4:409]
tr.data.y <- as.factor(data[ind==1,410])
cv.data.x <- data[ind==2,4:409]
cv.data.y <- as.factor(data[ind==2,410])
ts.data.x <- data[ind==3,4:409]
ts.data.y <- as.factor(data[ind==3,410])
## rf <- randomForest(data[ind==1,-1],ylabels[ind==1], importance=T, proximity=T, na.action=na.omit)
## rf <- randomForest(default_flag_factor ~ ., data=tr.data, importance=TRUE, proximity=TRUE, na.action=na.omit)
rf <- randomForest(tr.data.x, tr.data.y, importance=TRUE, proximity=TRUE, na.action=na.roughfix)
pred <- predict(rf, cv.data.x)
table(observed = cv.data.y, predicted = pred)
## Get prediction for all trees.randomForest 17
## predict(rf, data[ind==2,], predict.all=TRUE)
## Proximities.
## predict(rf, data[ind==2,], proximity=TRUE)
## Nodes matrix.
## str(attr(predict(rf, data[ind==2,], nodes=TRUE), "nodes"))
