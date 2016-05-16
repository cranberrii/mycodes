#UNIT 4

stv <- read.csv("stevens.csv")

library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(randomForest)
#for cp / CV
library(caret)
library(e1071)


set.seed(3000)
set.seed(100)

split <- sample.split(stv$Reverse, 0.7)
Train <- subset(stv, split == TRUE)
Test <- subset(stv, split == FALSE)


stvTree <- rpart(Reverse ~ Circuit + Issue + Petitioner +Respondent+LowerCourt+Unconst, data=Train, method="class", minbucket=100)
prp(stvTree)
PredictCART <- predict(stvTree, newdata = Test, type="class")

#confusion matrix
table(Test$Reverse, PredictCART)
(38+75)/(38+75+39+18)

PredictROC <- predict(stvTree, newdata=Test)
pred <- prediction(PredictROC[,2], Test$Reverse)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))

auc = as.numeric(performance(pred, "auc")@y.values)
auc

Train$Reverse <- as.factor(Train$Reverse)
Test$Reverse <- as.factor(Test$Reverse)

stvForest <- randomForest(Reverse ~ Circuit + Issue + Petitioner +Respondent+LowerCourt+Unconst, data=Train, ntree=200, nodesize=25)
PredictForest <- predict(stvForest, newdata = Test)

table(Test$Reverse, PredictForest)
(42+76)/(42+35+17+76)

#CrossValidation
#complexity parameter

numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01, 0.5, 0.01))
train(Reverse ~ Circuit + Issue + Petitioner +Respondent+LowerCourt+Unconst, data=Train, method="rpart", trControl=numFolds, tuneGrid=cpGrid)

stvTreeCV <- rpart(Reverse ~ Circuit + Issue + Petitioner +Respondent+LowerCourt+Unconst, data=Train, method="class", cp=0.2) 
PredictCV <- predict(stvTreeCV, newdata = Test, type = "class")

table(Test$Reverse, PredictCV)
(49+67)/(49+28+26+67)

prp(stvTreeCV)


###


claims <- read.csv("ClaimsData.csv")
table(claims$bucket2008)/nrow(claims)

set.seed(88)

split <- sample.split(claims$bucket2009, 0.6)
claimsTrain <- subset(claims, split == TRUE)
claimsTest <- subset(claims, split == FALSE)

table(claimsTest$bucket2009, claimsTest$bucket2008)
(110051+10798+2726+1575+99)/nrow(claimsTest)
PeneltyMatrix <- matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow = TRUE,nrow = 5)
#penelty error
sum(as.matrix(table(claimsTest$bucket2009, claimsTest$bucket2008))*PeneltyMatrix)/nrow(claimsTest)

ClaimsTree <- rpart(bucket2009 ~ age+arthritis+alzheimers+cancer+copd+depression+diabetes+ heart.failure+ihd+kidney+stroke+osteoporosis+bucket2008+reimbursement2008, data = claimsTrain, method = "class", cp=0.00005, parms = list(loss=PeneltyMatrix))
prp(ClaimsTree)
PredictTest <- predict(ClaimsTree, newdata = claimsTest, type = "class")
table(claimsTest$bucket2009, PredictTest)

sum(as.matrix(table(claimsTest$bucket2009, PredictTest))*PeneltyMatrix)/nrow(claimsTest)

###

b <- read.csv("boston.csv")
str(b)
plot(b$LON, b$LAT)
points(b$LON[b$CHAS==1], b$LAT[b$CHAS==1], col="blue",pch=19)
points(b$LON[b$TRACT==3531], b$LAT[b$TRACT==3531], col="red",pch=19)
points(b$LON[b$NOX>=0.55], b$LAT[b$NOX>=0.55], col="green",pch=19)
plot(b$LON, b$LAT)
points(b$LON[b$MEDV>=21.2], b$LAT[b$MEDV>=21.2], col="red",pch=19)

latlonlm <- lm(MEDV ~ LAT + LON, data = b)
summary(latlonlm)
points(b$LON[latlonlm$fitted.values>=21.2], b$LAT[latlonlm$fitted.values>=21.2], col="blue",pch="$")

latlontree <- rpart(MEDV ~ LAT + LON, data = b, minbucket=50)
prp(latlontree)
plot(b$LON, b$LAT)
points(b$LON[b$MEDV>=21.2], b$LAT[b$MEDV>=21.2], col="red",pch=19)
fittedvalues <- predict(latlontree)
points(b$LON[fittedvalues>=21.2], b$LAT[fittedvalues >=21.2], col="blue",pch="$")
plot(latlontree)
text(latlontree)
abline(v = -71)
abline(h = 42.28)
abline(h = 42.17)

set.seed(123)
split <- sample.split(b$MEDV, 0.7)
train <- subset(b, split == TRUE)
test <- subset(b, split == FALSE)
linreg <- lm(MEDV ~ LAT + LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data = train)
linreg.pred <- predict(linreg, newdata = test)
linreg.sse <- sum((linreg.pred - test$MEDV)^2)
linreg.sse
tree <- rpart(MEDV ~ LAT + LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data = train)
prp(tree)
tree.pred <- predict(tree, newdata = test)
tree.sse <- sum((tree.pred - test$MEDV)^2)
tree.sse

#small cp encourage large trees
#CV
tr.control <- trainControl(method = "cv", number = 10)
cp.grid <- expand.grid(.cp = (0:10)*0.001)
tr <- train(MEDV ~ LAT + LON+CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid=cp.grid)
best.tree <- tr$finalModel
prp(best.tree)
best.tree.pred <- predict(best.tree, newdata = test)
best.tree.sse <- sum((best.tree.pred - test$MEDV)^2)
