
require('rpart')
data.all <- read.csv(file="dev_sample.csv", header=T)
data <- data.all
set.seed(92434)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.66, 0.34, 0.0))
data$date <- NULL
data$UniqueID <- NULL
print("Computing na.count")
data$na.count <- sapply(1:nrow(data), function(r) { sum(is.na(data[r,])) })
print("Computing na.consistency.count")
c1 <- which(names(data)=="consistency")
c2 <- which(names(data)=="consistencyzip")
#data$na.consistency.count <- sapply(1:nrow(data), function(r) { sum(is.na(data[r,c1:c2])) })
c1 <- which(names(data)=="hasaboutme")
c2 <- which(names(data)=="haszynga")
c3 <- which(names(data)=="haspinterest")
c4 <- which(names(data)=="hasposterous")
#data$na.has.count <- sapply(1:nrow(data), function(r) { sum(is.na(data[r,c(c1:c2,c3,c4)])) })
tr.data <- data[ind==1,]
cv.data <- data[ind==2,]
ts.data <- data[ind==3,]

parms_list <- list(prior = c(.50,.50), split = "information")
rpart.control(maxdepth=30)

print("Running with all features")
dt1 <- rpart(default_flag ~ ., data=tr.data, method="class", parms=parms_list)
cv.pred1 <- predict(dt1, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred1)
print(paste("Accuracy: ", sum(cv.pred1==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all index features")
dt2 <- rpart(default_flag ~ consistency + attributecredibility + character + matchcount + matchquality + sum_haspresence + sum_verified_aaeep + sum_verifiedall, data=tr.data, method="class", parms=parms_list)
cv.pred2 <- predict(dt2, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred2)
print(paste("Accuracy: ", sum(cv.pred2==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all other features")
#dt3 <- rpart(default_flag ~ age_numeric + employment_type + jobtitle + likelybusinessowner + householdincome + lengthofresidence + homemarketvalue + homeownerstatus, data=tr.data, method="class", parms=parms_list)
dt3 <- rpart(default_flag ~ householdincome + lengthofresidence + homemarketvalue + homeownerstatus, data=tr.data, method="class", parms=parms_list)
cv.pred3 <- predict(dt3, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred3)
print(paste("Accuracy: ", sum(cv.pred3==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all mean features")
dt4 <- rpart(default_flag ~ mean_consistencybio + mean_consistency_haspresence + mean_cred_haspresence + mean_consistencyall, data=tr.data, method="class", parms=parms_list)
cv.pred4 <- predict(dt4, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred4)
print(paste("Accuracy: ", sum(cv.pred4==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all consistency features")
dt5 <- rpart(default_flag ~ consistencyaddress + consistencyaddressdistance + consistencyaddressquality + consistencyage + consistencycheckinscount + consistencychildren + consistencycity + consistencycountry + consistencycounty + consistencyemailprovidercountry + consistencyemailproviderdomain + consistencyfirstname + consistencyfullname + consistencygender + consistencyhomemarketvalue + consistencyhomeownerstatus + consistencyhouseholdincome + consistencyincome + consistencyindustry + consistencyjobtitle + consistencylastname + consistencylengthofresidence + consistencylinkedinbio + consistencylocality + consistencylocation + consistencynearestvenue + consistencynearestvenuegroup + consistencynearestvenuetype + consistencynegativesentiment + consistencypositivesentiment + consistencypostalcode + consistencyreviewcount + consistencyschool + consistencyschoolend + consistencyschoollikelihood + consistencystate + consistencystreet + consistencystreetnumber + consistencyvalidphone + consistencyvenuecount + consistencyverifiedemailaccount + consistencyzip, data=tr.data, method="class", parms=parms_list)
cv.pred5 <- predict(dt5, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred5)
print(paste("Accuracy: ", sum(cv.pred5==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)
#consistencyhasaboutme + consistencyhasbadoo + consistencyhasbebo + consistencyhasblogger + consistencyhasclassmates + consistencyhasdelicious + consistencyhasdigg + consistencyhasdisqus + consistencyhasfacebook + consistencyhasflickr + consistencyhasflixster + consistencyhasfoursquare + consistencyhasfriendfeed + consistencyhasgithub + consistencyhasgoogleplus + consistencyhasgoogleprofile + consistencyhasgravatar + consistencyhashi + consistencyhasklout + consistencyhaslanyrd + consistencyhaslastfm + consistencyhaslinkedin + consistencyhaslivejournal + consistencyhasmeetup + consistencyhasmyspace + consistencyhasmyworldebay + consistencyhasning + consistencyhasorkut + consistencyhasother + consistencyhaspicasa + consistencyhaspinterest + consistencyhasplancast + consistencyhasposterous + consistencyhasquora + consistencyhasrenren + consistencyhasslideshare + consistencyhassoundcloud + consistencyhastagged + consistencyhastripadvisor + consistencyhastripit + consistencyhastumblr + consistencyhastwitter + consistencyhasvimeo + consistencyhasvk + consistencyhasweibo + consistencyhaswordpress + consistencyhasxbox + consistencyhasyahoo + consistencyhasyelp + consistencyhasyoutube + consistencyhaszynga + 

print("Running with all credibility features")
dt6 <- rpart(default_flag ~ X_checkinscount + X_children + X_emailhit + X_employerlikelihood + X_fullnamehit + X_gender + X_hasfacebook + X_haslinkedin + X_homemarketvalue + X_homeownerstatus + X_householdincome + X_iphit + X_jobtitle + X_lengthofresidence + X_maritalstatus + X_nearestvenuegroup + X_nearestvenuetype + X_positivesentiment + X_postalcode + X_reviewage + X_reviewcount + X_userscount + X_venuecount + X_verifiedemailaccount + X_addressdistance + X_addresshit + X_age, data=tr.data, method="class", parms=parms_list)
cv.pred6 <- predict(dt6, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred6)
print(paste("Accuracy: ", sum(cv.pred6==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all has features")
dt7 <- rpart(default_flag ~ hasaboutme + hasbadoo + hasbebo + hasblogger + hasclassmates + hasdelicious + hasdigg + hasdisqus + hasfacebook + hasflickr + hasflixster + hasfoursquare + hasfriendfeed + hasgithub + hasgoogleplus + hasgoogleprofile + hasgravatar + hashi5 + hasklout + haslanyrd + haslastfm + haslinkedin + haslivejournal + hasmeetup + hasmyspace + hasmyworldebay + hasning + hasorkut + hasother + haspicasa + hasplancast + hasquora + hasrenren + hasslideshare + hassoundcloud + hastagged + hastripadvisor + hastripit + hastumblr + hastwitter + hasvimeo + hasvk + hasweibo + haswordpress + hasxbox + hasyahoo + hasyelp + hasyoutube + haszynga + haspinterest + hasposterous, data=tr.data, method="class", parms=parms_list)
cv.pred7 <- predict(dt7, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred7)
print(paste("Accuracy: ", sum(cv.pred7==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all verified features")
dt8 <- rpart(default_flag ~ active_linkedin + verifiedemailaccount + verifiedmerchant + verifiedaddress + verifiedadult + verifiedadult19 + verifiedage + verifiedemail + verifiedemployer + verifiedfacebook + verifiedlinkedin + verifiedminor + verifiedminor19 + verifiedphone + verifiedschool + verifiedtwitter, data=tr.data, method="class", parms=parms_list)
cv.pred8 <- predict(dt8, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred8)
print(paste("Accuracy: ", sum(cv.pred8==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all NA.count features")
dt9 <- rpart(default_flag ~ na.count, data=tr.data, method="class", parms=parms_list)
cv.pred9 <- predict(dt9, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred9)
print(paste("Accuracy: ", sum(cv.pred9==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

cv.pred <- (cbind(cv.pred1, cv.pred2, cv.pred3, cv.pred4, cv.pred5, cv.pred6, cv.pred7, cv.pred8, cv.pred9, cv.data$default_flag))

print("Mean based")
pred.table.mean <- table(predicted= sapply(1:nrow(cv.pred), function(r) { -1+mean(cv.pred[r,1:9]) }) , observed=cv.pred[,10])
print(pred.table.mean)
print("Median based")
pred.table.median <- table(predicted= sapply(1:nrow(cv.pred), function(r) { -1+median(cv.pred[r,1:9]) }) , observed=cv.pred[,10])
print(pred.table.median)
