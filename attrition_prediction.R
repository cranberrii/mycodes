library(ggplot2)
library(scales)
library(GGally)
library(plyr)
library(tidyr)
library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(ROCR)
library(randomForest)
library(gbm)
library(caret)
library(pROC)
library(e1071)
library(tibble)
library(readr)

dev.off()

df <- read.csv("FULL TOM 2015.csv")
###prep data###
df$LOCAL.REGIONAL <- as.factor(tolower(df$LOCAL.REGIONAL))
df$Type.of.Contract <- as.factor(tolower(df$Type.of.Contract))
df$JOB.GRADE <- as.factor(df$JOB.GRADE)
df$ANNUAL.BASE.SALARY...LOCAL <- as.numeric(gsub("[[:punct:]]", "", df$ANNUAL.BASE.SALARY...LOCAL))
df$ANNUAL.BASE.SALARY...USD <- sub("USD ","",df$ANNUAL.BASE.SALARY...USD)
df$ANNUAL.BASE.SALARY...USD <- as.numeric(gsub("[[:punct:]]", "", df$ANNUAL.BASE.SALARY...USD))
df$MIN <- as.numeric(gsub("[[:punct:]]", "", df$MIN))
df$MID <- as.numeric(gsub("[[:punct:]]", "", df$MID))
df$MAX <- as.numeric(gsub("[[:punct:]]", "", df$MAX))
df$RANGE.PENETRATION <- as.numeric(sub("%","",df$RANGE.PENETRATION))
df$RANGE.PENETRATION[df$RANGE.PENETRATION >400] <- 399
df$RANGE.PENETRATION.2 <- cut(df$RANGE.PENETRATION, 
                       breaks = c(-Inf, 0, 33, 66, 100, Inf), 
                       labels = c("< 0%", "0% to 33%", "34% to 66%", "67% to 100%", "> 100%"), 
                       right = FALSE)
df$Length.of.Service[df$Length.of.Service <1] <- 1
df$Age.LOS <- df$EmployeeAge/df$Length.of.Service # new feature: Age / LOS
df$RP.Age <- df$RANGE.PENETRATION/df$EmployeeAge  # new feature: RP / Age
df$EducationLevel <- as.factor(tolower(df$EducationLevel))
df$MaritalStatus <- as.factor(tolower(df$MaritalStatus))
df$Male.Female <- sub(" ","",df$Male.Female)
df$Male.Female <- as.factor(tolower(df$Male.Female))
df$Leaver.2 <- cut(df$Leaver, breaks = c(-Inf, 1, Inf), 
                              labels = c(0, 1), right = FALSE)
df$Leaver.2 <- as.character(df$Leaver.2) # change to char for gbm
df$SalaryIncrementin1Year.2 <- cut(df$SalaryIncrementin1Year, breaks = c(-Inf, 1, Inf), 
                                   labels = c(0, 1), right = FALSE)
df$PROMO.1.YEAR.2 <- cut(df$PROMO.1.YEAR, breaks = c(-Inf, 1, Inf), 
                                   labels = c(0, 1), right = FALSE)
df <- df %>% 
  separate(col = COST.CENTER, into = c("CC1", "CC2","CC3"), sep = "/") %>%
  filter(JOB.GRADE != "10" & Type.of.Contract == "localcontract" ) %>%
  filter(Transfer == 0 & Leaverin3months == 0 & MID != "NA")
df$CC1 <- as.factor(tolower(gsub(" ", "", df$CC1)))
df$CC2 <- as.factor(tolower(gsub(" ", "", df$CC2)))
df$CC3 <- as.factor(tolower(gsub(" ", "", df$CC3)))
df <- df %>% 
  filter(CC1 == 'technology')

# SELECT only variables for prediction
df <- df %>% 
  select(Length.of.Service, JOB.GRADE, 
         PROMO.1.YEAR.2, SalaryIncrementin1Year.2, EmployeeAge, 
         EducationLevel, MaritalStatus, No.ofDeps, 
         RANGE.PENETRATION.2, Age.LOS, RP.Age, 
         LOCAL.REGIONAL, Local.Foreigner, Male.Female, 
         Leaver.2) 
  # low importance: MainLoc


set.seed(345)

split = sample.split(df$Leaver.2, SplitRatio = 0.70)
df.train = subset(df, split==TRUE)
df.test = subset(df, split==FALSE)


###explore data###

prop.table(table(df$Leaver.2))

prop.table(table(df$CC1, df$Leaver.2),1)
prop.table(table(df$RANGE.PENETRATION.2, df$Leaver.2),1)

(chisq.test(df$CC1, df$Leaver.2)) # higher better
(chisq.test(df$EmployeeAge, df$Leaver.2)) # higher better

df %>% 
  group_by(RANGE.PENETRATION.2, Leaver.2) %>%
  count(RANGE.PENETRATION.2) %>%
  mutate(freq = n/sum(n)) %>%
  ggplot(aes(x = RANGE.PENETRATION.2, fill = Leaver.2)) + geom_bar()

df %>% 
  group_by(RANGE.PENETRATION.2, Leaver.2) %>%
  count(RANGE.PENETRATION.2) %>%
  ggplot(aes(x = RANGE.PENETRATION.2, fill = Leaver.2)) + geom_bar()


ggplot(data = df, aes(x = Length.of.Service, fill = Leaver.2)) + geom_density(alpha=0.33) + 
  theme(axis.title.y=element_text(size=16), axis.title.x=element_text(size=16), axis.text.y  = element_text(size=16), axis.text.x  = element_text(size=16))
ggplot(data = df, aes(x = Length.of.Service, fill = Leaver.2)) + geom_histogram(alpha=0.5, binwidth=1)

ggplot(data = df, aes(x = EmployeeAge, fill = Leaver.2)) + geom_density(alpha=0.33) + 
  theme(axis.title.y=element_text(size=16), axis.title.x=element_text(size=16), axis.text.y  = element_text(size=16), axis.text.x  = element_text(size=16))


ggplot(data = df, aes(x = RANGE.PENETRATION, fill = PROMO.1.YEAR.2)) + geom_density(alpha=0.33)

ggplot(data = df, aes(x = RANGE.PENETRATION.2)) + geom_bar(aes(fill = Leaver.2), position = 'fill') + 
  #facet_wrap(~CC1) + 
  theme(axis.title.y=element_text(size=13), axis.title.x=element_text(size=13), 
        axis.text.y  = element_text(size=13), axis.text.x  = element_text(size=11),
        strip.text.x = element_text(size=13),strip.text.y = element_text(size=13))


###cart###
tree = rpart(Leaver.2 ~ . , data=df.train, minbucket=50)
fancyRpartPlot(tree)

tree.pred = predict(tree, newdata=df.test, type = "class")
#MSE for regression
#tree.sse = sum((tree.pred - df.test$Leaver.2)^2)
#tree.sse

#confusion matrix
table(df.test$Leaver.2, tree.pred)
#(38+75)/(38+75+39+18)

PredictROC <- predict(tree, newdata=df.test)
pred <- prediction(PredictROC[,2], df.test$Leaver.2)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))
abline(a=0, b= 1)
auc = as.numeric(performance(pred, "auc")@y.values)
auc


###randomforest###

for(mtry in 1:13) { 
  rforest <- randomForest(Leaver.2 ~ ., data=df.train, mtry=3, ntree=800) 
  rforest.pred <- predict(rforest, type="prob", newdata=df.test)[ ,2]
  pred2 <- prediction(rforest.pred, df.test$Leaver.2) 
  perf2 <- performance(pred2, "tpr", "fpr") 
  plot(perf2, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7)) 
  abline(a=0, b= 1) 
  auc <- as.numeric(performance(pred2, "auc")@y.values) 
  cat(mtry, "). ")
  cat(auc, " ")
  #best mtry = 3, ntree = 800, auc=0.8982015
}
varImpPlot(rforest)


###boosting###

booster <- gbm(Leaver.2 ~ ., data=df.train,
               distribution = "bernoulli", n.tree = 8000, 
               shrinkage = 0.005, interaction.depth = 4, cv.folds = 5, 
               keep.data = FALSE, verbose = FALSE,
               class.stratify.cv=TRUE, n.cores = 4) 

summary(booster)
iterations_optimal <- gbm.perf(object = booster ,plot.it = TRUE,oobag.curve = TRUE,overlay = TRUE,method="cv")
print(iterations_optimal)

for (i in 1:length(booster$var.names)) {
  plot(booster, i.var = i, col="blue", 
       ntrees = gbm.perf(booster, plot.it = FALSE), #optimal # trees
       type = "response") # to get fitted probs
}

ntrees = gbm.perf(booster, plot.it = TRUE)
pred.gbm <- predict.gbm(booster, df.test, n.trees = ntrees, type = 'response')
head(pred.gbm)

pred.gbm2 <- prediction(pred.gbm, df.test$Leaver.2) 
perf.gbm <- performance(pred.gbm2, "tpr", "fpr") 
plot(perf.gbm, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7)) 
abline(a=0, b= 1) 
(auc <- as.numeric(performance(pred.gbm2, "auc")@y.values)) 
# best auc: 0.91469

pred.gbm <- ifelse(pred.gbm >= 0.5, 1, 0)
confusionMatrix(pred.gbm, df.test$Leaver.2)


###logistic regression###

log.reg <- glm(Leaver.2 ~ . , family=binomial(link='logit'), data=df.train)
summary(log.reg)

log.reg2 <- glm(Leaver.2 ~ Length.of.Service + JOB.GRADE + 
                  PROMO.1.YEAR.2 + SalaryIncrementin1Year.2 + EmployeeAge + 
                  EducationLevel + MaritalStatus + No.ofDeps + LOCAL.REGIONAL + 
                  RANGE.PENETRATION.2, family=binomial(link='logit'), data=df.train)
varImp(log.reg2)

pred.log = predict(log.reg2, newdata = df.test, type = "response")

pred.log <- ifelse(pred.log >= 0.5, 1, 0)
confusionMatrix(data=pred.log, df.test$Leaver.2)


###
#random codes
###
predictors <- names(df)[1:13]
outcome <- 'Leaver.2'

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

booster <- train(df.train[,predictors], df.train[,outcome], 
                 method='gbm', 
                 trControl=objControl, 
                 metric = "ROC", 
                 preProc = c("center", "scale"))

ggpairs(df[,c("EmployeeAge","Length.of.Service")], axisLabels="show")


oob.error <- double(13)
test.error <- double(13)
for(mtry in 1:13) {
  forest...
  oob.error[mtry] <- forest$MSE[400]
  forest.pred <- predict(forest, df.test)
  test.error[mtry] <- with(df.test, mean((as.numeric(Leaver.2-forest.pred)^2)))
  cat(mtry, " ")
}
matplot(1:mtry, cbind(test.error,oob.error), pch=19, col = c("red","blue"), type = "b", ylab = "Mean Squared Error")
legend("topright", legend = c("OOB","Test"), pch=19, col = c("red","blue"))
