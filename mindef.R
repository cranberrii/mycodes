library(GGally)
library(randomForest)


#read & massage data 
train <- read.csv("20150803115609-HR_Retention_2013_training.csv")
train$RESIGN_DATE <- NULL
train$RESIGNATION_MTH <- NULL
train$RESIGNATION_QTR <- NULL
train$RESIGNATION_YEAR <- NULL
train$STATUS <- NULL

test <- read.csv("20150803115608-HR_Retention_2013_to_be_predicted.csv")
test <- as.data.frame(append(test, list(RESIGNED = NA), after = 4))

combi <- rbind(train, test)

combi$NATIONALITY <- as.character(combi$NATIONALITY)
combi$NATIONALITY[combi$NATIONALITY %in% c('Chinese', 'Filipino', 'Indonesian', 'Malaysian', 'Burmese')] <- 'Others'
combi$NATIONALITY <- as.factor(combi$NATIONALITY)
combi$EMPLOYEE_GROUP[2956] <- 'MILITARY REGULARS'
combi$NO_OF_KIDS[c(is.na(combi$NO_OF_KIDS))] <- median(combi$NO_OF_KIDS, na.rm=TRUE)
combi$MIN_CHILD_AGE[c(is.na(combi$MIN_CHILD_AGE))] <- median(combi$MIN_CHILD_AGE, na.rm=TRUE)
combi$AVE_CHILD_AGE[c(is.na(combi$AVE_CHILD_AGE))] <- median(combi$AVE_CHILD_AGE, na.rm=TRUE)
combi$HSP_CERT_RANK[c(is.na(combi$HSP_CERT_RANK))] <- median(combi$HSP_CERT_RANK, na.rm=TRUE)
combi$PROMO_LAST_5_YRS[c(is.na(combi$PROMO_LAST_5_YRS))] <- median(combi$PROMO_LAST_5_YRS, na.rm=TRUE)
combi$PROMO_LAST_4_YRS[c(is.na(combi$PROMO_LAST_4_YRS))] <- median(combi$PROMO_LAST_4_YRS, na.rm=TRUE)
combi$PROMO_LAST_3_YRS[c(is.na(combi$PROMO_LAST_3_YRS))] <- median(combi$PROMO_LAST_3_YRS, na.rm=TRUE)
combi$PROMO_LAST_2_YRS[c(is.na(combi$PROMO_LAST_2_YRS))] <- median(combi$PROMO_LAST_2_YRS, na.rm=TRUE)
combi$PROMO_LAST_1_YR[c(is.na(combi$PROMO_LAST_1_YR))] <- median(combi$PROMO_LAST_1_YR, na.rm=TRUE)
combi$AWARDS_RECEIVED[c(is.na(combi$AWARDS_RECEIVED))] <- median(combi$AWARDS_RECEIVED, na.rm=TRUE)
combi$HOUSING_RANK[c(is.na(combi$HOUSING_RANK))] <- median(combi$HOUSING_RANK, na.rm=TRUE)
combi$HOUSE_UPG_DGRD[c(is.na(combi$HOUSE_UPG_DGRD))] <- median(combi$HOUSE_UPG_DGRD, na.rm=TRUE)
combi$IPPT_SCORE[c(is.na(combi$IPPT_SCORE))] <- median(combi$IPPT_SCORE, na.rm=TRUE)
combi$PES_SCORE[c(is.na(combi$PES_SCORE))] <- median(combi$PES_SCORE, na.rm=TRUE)
combi$HOMETOWORKDIST[c(is.na(combi$HOMETOWORKDIST))] <- median(combi$HOMETOWORKDIST, na.rm=TRUE)
combi$SVC_INJURY_TYPE[c(is.na(combi$SVC_INJURY_TYPE))] <- 0
combi$TOT_PERC_INC_LAST_1_YR[c(is.na(combi$TOT_PERC_INC_LAST_1_YR))] <- median(combi$TOT_PERC_INC_LAST_1_YR, na.rm=TRUE)
combi$BAS_PERC_INC_LAST_1_YR[c(is.na(combi$BAS_PERC_INC_LAST_1_YR))] <- median(combi$BAS_PERC_INC_LAST_1_YR, na.rm=TRUE)

combi$VOC <- NULL
combi$UNIT <- NULL
combi$HSP_CERT_DESC <- NULL
combi$UPGRADED_CERT_DESC_3_YRS <- NULL

summary(combi)

train2 <- combi[1:15019,]
test2 <- combi[15020:23107,]


#randomForest
set.seed(28)

'''
#obtain best mtry
bestmtry <- tuneRF(train2[,-5], train2[,5], ntreeTry = 100, 
                   stepFactor = 1.5, improve = 0.01, trace = TRUE, plot = TRUE, dobest = FALSE)
'''

mindef_rf <- randomForest(as.factor(RESIGNED) ~ .,  data = train2, ntree = 1000, mtry = 15, keep.forest = TRUE, 
                          importance = TRUE, na.action = na.omit)

varImpPlot(mindef_rf)

Prediction <- predict(mindef_rf, newdata = test2, type = "prob")


#save submission			
submit_file = data.frame(ID = test2$PERID, ACTION = Prediction[,2])
write.table(submit_file, file="mindef_soln_RF.csv", row.names = FALSE, col.names = TRUE, sep = ",")



'''
References

summary(combi$EMPLOYEE_GROUP)
hist(combi$PROMO_LAST_1_YR)
table(combi$HSP_CERT_DESC)
str(combi)
which(is.na(combi$EMPLOYEE_GROUP))
which(train$HOUSING_TYPE == '')

#plot ggpairs to determine correlation 
ggpairs(train2[,c(5,12,47,48)]) 
        #upper = list(continuous = "density", combo = "box"), lower = list(continuous = "points", combo = "dot"))

http://trevorstephens.com/post/73461351896/titanic-getting-started-with-r-part-4-feature
http://scg.sdsu.edu/rf_r/
http://stackoverflow.com/questions/23021057/randomforest-machine-learning-in-r
http://stackoverflow.com/questions/20370827/plot-learning-curves-with-caret-package-and-r
'''