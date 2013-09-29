
require('rpart')
data.all <- read.csv(file="dev_sample.csv", header=T)
data <- data.all
set.seed(1234)
ind <- sample(3, nrow(data), replace = TRUE, prob=c(0.7, 0.3, 0.0))
data$date <- NULL
data$UniqueID <- NULL
tr.data <- data[ind==1,]
cv.data <- data[ind==2,]
ts.data <- data[ind==3,]

parms_list <- list(prior = c(.49,.51), split = "information")
rpart.control(maxdepth=30)

print("Running with all features")
dt1 <- rpart(default_flag ~ ., data=tr.data, method="class", parms=parms_list)
cv.pred1 <- predict(dt1, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred1)
print(paste("Accuracy: ", sum(cv.pred1==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all index features")
dt2 <- rpart(default_flag ~ attributecredibility + character + matchcount + matchquality + sum_haspresence + sum_verified_aaeep + sum_verifiedall, data=tr.data, method="class", parms=parms_list)
cv.pred2 <- predict(dt2, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred2)
print(paste("Accuracy: ", sum(cv.pred2==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all status features")
dt3 <- rpart(default_flag ~ age_numeric + businessage + householdincome + lengthofresidence + homemarketvalue + homeownerstatus, data=tr.data, method="class", parms=parms_list)
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
dt5 <- rpart(default_flag ~ consistencyaddress + consistencyaddressdistance + consistencyaddressquality + consistencyage + consistencyagerange + consistencyalumnigroups + consistencyavgoccupationlength + consistencybio + consistencybusinessage + consistencycheckinscount + consistencychildren + consistencycity + consistencyconnections + consistencycountry + consistencycounty + consistencydegree + consistencydomainnameage + consistencyeducation + consistencyeducations + consistencyemail + consistencyemailprovidercountry + consistencyemailproviderdomain + consistencyemployer + consistencyemployerdomain + consistencyemployerlikelihood + consistencyfirstname + consistencyfullname + consistencygender + consistencygoogleplusbio + consistencygoogleprofilebio + consistencygravatarbio + consistencygroups + consistencyhasaboutme + consistencyhasbadoo + consistencyhasbebo + consistencyhasblogger + consistencyhasclassmates + consistencyhasdelicious + consistencyhasdigg + consistencyhasdisqus + consistencyhasfacebook + consistencyhasflickr + consistencyhasflixster + consistencyhasfoursquare + consistencyhasfriendfeed + consistencyhasgithub + consistencyhasgoogleplus + consistencyhasgoogleprofile + consistencyhasgravatar + consistencyhashi + consistencyhasklout + consistencyhaslanyrd + consistencyhaslastfm + consistencyhaslinkedin + consistencyhaslivejournal + consistencyhasmeetup + consistencyhasmyspace + consistencyhasmyworldebay + consistencyhasning + consistencyhasorkut + consistencyhasother + consistencyhaspicasa + consistencyhaspinterest + consistencyhasplancast + consistencyhasposterous + consistencyhasquora + consistencyhasrenren + consistencyhasslideshare + consistencyhassoundcloud + consistencyhastagged + consistencyhastripadvisor + consistencyhastripit + consistencyhastumblr + consistencyhastwitter + consistencyhasvimeo + consistencyhasvk + consistencyhasweibo + consistencyhaswordpress + consistencyhasxbox + consistencyhasyahoo + consistencyhasyelp + consistencyhasyoutube + consistencyhaszynga + consistencyhighestdegree + consistencyhighnetworth + consistencyhomemarketvalue + consistencyhomeownerstatus + consistencyhouseholdincome + consistencyincome + consistencyindustry + consistencyjobtitle + consistencylanguages + consistencylanyrdbio + consistencylastname + consistencylat + consistencylatesttweet + consistencylatlon + consistencylengthofresidence + consistencylinkedinbio + consistencylocality + consistencylocation + consistencylon + consistencymajor + consistencymaritalstatus + consistencymarkertweet + consistencymentions + consistencymerchant + consistencynearestvenue + consistencynearestvenuegroup + consistencynearestvenuetype + consistencynegativesentiment + consistencyneighborhood + consistencyoccupation + consistencyoccupationend + consistencyoccupationlength + consistencyoccupations + consistencyoccupationstart + consistencyownername + consistencyphone + consistencyplancastbio + consistencypopulation + consistencypositivesentiment + consistencypostalcode + consistencyrecommendations + consistencyregistrationnumber + consistencyreviewcount + consistencyreviewrating + consistencyschool + consistencyschoolend + consistencyschoollikelihood + consistencyschoolstart + consistencystate + consistencystreet + consistencystreetnumber + consistencytweetsperday + consistencytweetspersecond + consistencytwitterbio + consistencytwitterfollowers + consistencytwitterfollowing + consistencyvalidphone + consistencyvenuecount + consistencyverifiedemailaccount + consistencyvimeobio + consistencyzip, data=tr.data, method="class", parms=parms_list)
cv.pred5 <- predict(dt5, cv.data, type="class")
cv.table <- table(observed = cv.data$default_flag, predicted = cv.pred5)
print(paste("Accuracy: ", sum(cv.pred5==cv.data$default_flag)/nrow(cv.data)))
print(cv.table)

print("Running with all credibility features")
dt6 <- rpart(default_flag ~ X_agerange + X_alumnigroups + X_androiduser + X_averageoccupationlength + X_bio + X_businessage + X_checkinscount + X_children + X_connections + X_degree + X_domainnameage + X_education + X_educations + X_emailhit + X_employerlikelihood + X_fullnamehit + X_gender + X_groups + X_hasaboutme + X_hasbadoo + X_hasbebo + X_hasblogger + X_hasclassmates + X_hasdelicious + X_hasdigg + X_hasdisqus + X_hasfacebook + X_hasflickr + X_hasflixster + X_hasfoursquare + X_hasfriendfeed + X_hasgithub + X_hasgoogleplus + X_hasgoogleprofile + X_hasgravatar + X_hashi5 + X_hasklout + X_haslanyrd + X_haslastfm + X_haslinkedin + X_haslivejournal + X_hasmeetup + X_hasmyspace + X_hasmyworldebay + X_hasning + X_hasorkut + X_hasother + X_haspicasa + X_haspinterest + X_hasplancast + X_hasposterous + X_hasquora + X_hasrenren + X_hasslideshare + X_hassoundcloud + X_hastagged + X_hastripadvisor + X_hastripit + X_hastumblr + X_hastwitter + X_hasvimeo + X_hasvk + X_hasweibo + X_haswordpress + X_hasxbox + X_hasyahoo + X_hasyelp + X_hasyoutube + X_haszynga + X_highestdegree + X_highnetworth + X_homemarketvalue + X_homeownerstatus + X_householdincome + X_industry + X_iphit + X_jobtitle + X_languages + X_lat + X_latitude + X_lengthofresidence + X_maritalstatus + X_matchedbusinessownername + X_mentions + X_nearestvenuegroup + X_nearestvenuerating + X_nearestvenuetype + X_nearestvenuetypes + X_negativesentiment3 + X_occupation + X_occupationend + X_occupationlength + X_occupations + X_occupationstart + X_onlinetime + X_population + X_positivesentiment + X_postalcode + X_recommendations + X_registrationnumber + X_reviewage + X_reviewcount + X_reviewrating + X_tweetsperday + X_twitterbio + X_twitterfollowers + X_twitterfollowing + X_userscount + X_validphone + X_venuecount + X_verifiedemailaccount + X_verifiedmerchant + X_addressdistance + X_addresshit + X_age, data=tr.data, method="class", parms=parms_list)
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

cv.pred <- (cbind(cv.pred1, cv.pred2, cv.pred3, cv.pred4, cv.pred5, cv.pred6, cv.pred7))
