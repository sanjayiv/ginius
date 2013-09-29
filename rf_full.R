
require('randomForest')
data.all <- read.csv(file="dev_sample.csv", header=T)
data <- data.all
set.seed(111)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.6, 0.2, 0.2))
data$defaultflagfactor <- as.factor(data$default_flag)
#drop few columns
print(dim(data))
data$default_flag <- NULL
data$date <- NULL
max.allowed.num.na <- nrow(data)*0.1
drops <- lapply(colnames(data), function(ft) { if ( sum(is.na(data[ft])) > max.allowed.num.na ) { return (ft) } else { return("UniqueID") } } )
drops <- unique(drops)
#sapply(unique(drops), function(ft) { data$ft <- NULL } )
data <- data[,!(names(data) %in% drops)]
data.post.drops <- data
print(dim(data))
#keep columns which are either numeric or has less than 32 levels for factor
keeps <- lapply(colnames(data), function(ft) { if ( is.numeric(data[ft]) || nrow(unique(data[ft])) < 32) { return (ft) } else { return("defaultflagfactor") } } )
keeps <- unique(keeps)
#data <- data[,(names(data) %in% keeps)]
print(dim(data))
tr.data <- data[ind==1,]
cv.data <- data[ind==2,]
ts.data <- data[ind==3,]
print("Checkpoint#1")
tr.num <- 500
num.features <- ncol(tr.data)
ntree <- ceiling(num.features/2)
#rf <- randomForest(tr.data.x[1:tr.num,], tr.data.y[1:tr.num], classwt=c(0.2,0.8), ntree=ntree, importance=TRUE)
#rf <- randomForest(tr.data[1:tr.num,2:22], tr.data[1:tr.num,"defaultflagfactor"], classwt=c(5,5), ntree=ntree, importance=TRUE)
rf <- randomForest(defaultflagfactor ~ attributecredibility + character + matchcount + matchquality + sum_haspresence + sum_verified_aaeep + sum_verifiedall, data=tr.data, ntree=ntree, importance=TRUE, na.action=na.roughfix)
cv.pred <- predict(rf, cv.data)
ts.pred <- predict(rf, ts.data)
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
