#UNIT 3

frh <- read.csv("framingham.csv")
library(caTools)
library(ROCR)

set.seed(1000)
split <- sample.split(frh$TenYearCHD, 0.65)
Train <- subset(frh, split == TRUE)
Test <- subset(frh, split == FALSE)

frhLog = glm(TenYearCHD ~ . , data = Train, family = binomial)
predictTrain <- predict(frhLog, newdata = Train)
predictTest <- predict(frhLog, type = "response", newdata = Test)

#confusion matrix
table(Test$TenYearCHD, predictTest > 0.5)

ROCRPred <- prediction(predictTest, Test$TenYearCHD)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))

auc = as.numeric(performance(ROCRPred, "auc")@y.values)
auc

###

poll <- read.csv("PollingData.csv")
library(mice)

simple <- poll[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")]
set.seed(144)

imputed <- complete(mice(simple)) 
poll$Rasmussen <- imputed$Rasmussen
poll$SurveyUSA <- imputed$SurveyUSA

train <- subset(poll, Year == 2004 | Year == 2008)
test <- subset(poll, Year == 2012)
table(sign(train$Rasmussen))
#Baseline model
table(train$Republican , sign(train$Rasmussen))
cor(train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount","Republican")])

mod1 = glm(Republican ~ PropR , data = train, family = binomial)
pred1 <- predict(mod1, type = "response")
table(train$Republican, pred1 >= 0.5)

mod2 = glm(Republican ~ SurveyUSA + DiffCount , data = train, family = "binomial")
pred2 <- predict(mod2, type = "response")
table(train$Republican, pred2 >= 0.5)

#Test Set
table(test$Republican , sign(test$Rasmussen))

testpred <- predict(mod2, type = "response", newdata = test)
table(test$Republican , testpred >=0.5)
subset(test, testpred >= 0.5 & Republican == 0)
