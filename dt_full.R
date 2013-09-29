
require('rpart')
data.all <- read.csv(file="dev_sample.csv", header=T)
data <- data.all
set.seed(111)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.6, 0.2, 0.2))
data$date <- NULL
data$UniqueID <- NULL
tr.data <- data[ind==1,]
cv.data <- data[ind==2,]
ts.data <- data[ind==3,]
print("Checkpoint#1")
#dt <- rpart(default_flag ~ attributecredibility + character + matchcount + matchquality + sum_haspresence + sum_verified_aaeep + sum_verifiedall, data=tr.data, method="class")
rpart.control(maxdepth=20)
dt <- rpart(default_flag ~ ., data=tr.data, method="class", parms = list(prior = c(.45,.55), split = "information"))
cv.pred <- predict(dt, cv.data, type="class")
ts.pred <- predict(dt, ts.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred)
print(cv.table)
print("Checkpoint#2")
ts.table <- table(observed = ts.data$default_flag, predicted = ts.pred)
print(ts.table)
print("Checkpoint#3")
