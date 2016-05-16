bb <- read.csv("baseball.csv")

NBA <- read.csv("NBA_train.csv")
NBA_test <- read.csv("NBA_test.csv")

#UNIT 3

q <- read.csv("quality.csv")
library(caTools)
library(ROCR)

set.seed(88)
split <- sample.split(q$PoorCare, 0.75)
qTrain <- subset(q, split == TRUE)
qTest <- subset(q, split == FALSE)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qTrain, family=binomial)
predictTrain <- predict(QualityLog, newdata = qTrain)
predictTest <- predict(QualityLog, type="response", newdata=qTest)

#confusion matrix
table(qTrain$PoorCare, predictTrain > 0.5)

ROCRPred <- prediction(predictTrain, qTrain$PoorCare)
ROCRPerf <- performance(ROCRPred, "tpr", "fpr")
plot(ROCRPerf, colorize = TRUE, print.cutoffs.at=seq(0, 1, 0.1), text.adj=c(-0.2, 1.7))

ROCRpredTest = prediction(predictTest, qTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
