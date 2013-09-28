
require('randomForest')
data <- read.csv("out_dev_sample.csv", header=T)
## data <- read.csv("dev_sample.csv", header=T)
ylabels <- factor(data[,1])
set.seed(111)
ind <- sample(2, nrow(data), replace = TRUE, prob=c(0.7, 0.3))
rf <- randomForest(data[ind==1,-1],ylabels[ind==1], na.action=na.omit)
pred <- predict(rf, data[ind==2,])
table(observed = ylabels[ind==2], predicted = pred)
## Get prediction for all trees.randomForest 17
## predict(rf, data[ind==2,], predict.all=TRUE)
## Proximities.
## predict(rf, data[ind==2,], proximity=TRUE)
## Nodes matrix.
## str(attr(predict(rf, data[ind==2,], nodes=TRUE), "nodes"))
