
require('randomForest')
data <- read.csv(file="out_dev_sample.csv", header=T)
set.seed(111)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.6, 0.2, 0.2))
data$defaultflagfactor <- as.factor(data$default_flag)
data$default_flag <- NULL
tr.data <- data[ind==1,]
cv.data <- data[ind==2,]
ts.data <- data[ind==3,]
print("Checkpoint#1")
tr.num <- 10000
num.features <- ncol(tr.data)
ntree <- ceiling(num.features/2)
#rf <- randomForest(tr.data.x[1:tr.num,], tr.data.y[1:tr.num], classwt=c(0.2,0.8), ntree=ntree, importance=TRUE)
#rf <- randomForest(tr.data[1:tr.num,2:22], tr.data[1:tr.num,"defaultflagfactor"], classwt=c(5,5), ntree=ntree, importance=TRUE)
rf <- randomForest(defaultflagfactor ~ ., data=tr.data[1:tr.num,], classwt=c(5,5), ntree=ntree, importance=TRUE)
cv.pred <- predict(rf, cv.data[,-(num.features)])
ts.pred <- predict(rf, ts.data[,-(num.features)])
cv.table <- table(observed = cv.data$defaultflagfactor, predicted = cv.pred)
print(cv.table)
print("Checkpoint#2")
ts.table <- table(observed = ts.data$defaultflagfactor, predicted = ts.pred)
print(ts.table)
print("Checkpoint#3")
## Get prediction for all trees.randomForest 17
## predict(rf, data[ind==2,], predict.all=TRUE)
## Proximities.
## predict(rf, data[ind==2,], proximity=TRUE)
## Nodes matrix.
## str(attr(predict(rf, data[ind==2,], nodes=TRUE), "nodes"))
