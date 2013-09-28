
require('randomForest')
data <- read.csv(file="out_dev_sample.csv", header=T)
set.seed(111)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.6, 0.2, 0.2))
tr.data.x <- data[ind==1,-1]
tr.data.y <- as.factor(data[ind==1,1])
cv.data.x <- data[ind==2,-1]
cv.data.y <- as.factor(data[ind==2,1])
ts.data.x <- data[ind==3,-1]
ts.data.y <- as.factor(data[ind==3,1])
print("Checkpoint#1")
tr.num <- 5000
ntree <- ceiling(ncol(tr.data.x)/2)
rf <- randomForest(tr.data.x[1:tr.num,], tr.data.y[1:tr.num], ntree=ntree, importance=TRUE, proximity=TRUE)
cv.pred <- predict(rf, cv.data.x)
ts.pred <- predict(rf, ts.data.x)
cv.table <- table(observed = cv.data.y, predicted = cv.pred)
print(cv.table)
print("Checkpoint#2")
ts.table <- table(observed = ts.data.y, predicted = ts.pred)
print(ts.table)
print("Checkpoint#3")
## Get prediction for all trees.randomForest 17
## predict(rf, data[ind==2,], predict.all=TRUE)
## Proximities.
## predict(rf, data[ind==2,], proximity=TRUE)
## Nodes matrix.
## str(attr(predict(rf, data[ind==2,], nodes=TRUE), "nodes"))
