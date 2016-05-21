library(tibble)
library(tidyr)
library(readr)
library(ggplot2)
library(dplyr)

library(xgboost)
library(DiagrammeR)

options(tibble.width = Inf)

### http://dmlc.ml/rstats/2016/03/10/xgboost.html
data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

xgb.model <- xgb.cv(data = train$data, label = train$label, nrounds = 200, objective = "binary:logistic",
                     nfold = 5, early.stop.round = 3, maximize = FALSE)

xgb.pred <- predict(xgb.model, test$data)


bst <- xgboost(data = train$data, label = train$label, max.depth = 2,
               eta = 1, nthread = 2, nround = 3, objective = "binary:logistic")
xgb.plot.tree(feature_names = agaricus.train$data@Dimnames[[2]], model = bst)

# use this
bst <- xgb.cv(data = train$data, label = train$label, max.depth = 5,
               eta = 0.5, nthread = 2, nround = 30, objective = "binary:logistic",
               min_child_weight = 50, nfold = 5, early.stop.round = 3, maximize = FALSE)
, eval_metric = "auc")

model <- xgb.dump(bst, with.stats = T)


xgb.plot.multi.trees(model = bst, feature_names = agaricus.train$data@Dimnames[[2]], features.keep = 3)

xgb.pred <- predict(bst, data.matrix(test$data))

importance_matrix <- xgb.importance(agaricus.train$data@Dimnames[[2]], model = bst)
xgb.plot.importance(importance_matrix[1:5])


### http://xgboost.readthedocs.io/en/latest/R-package/discoverYourData.html

require(Matrix)
require(data.table)
if (!require('vcd')) install.packages('vcd')
library(vcd)

data(Arthritis)
df <- data.table(Arthritis, keep.rownames = F)
df[,ID:=NULL]
head(df[,AgeDiscret := as.factor(round(Age/10,0))])

# one hot encoding
sparse_matrix <- sparse.model.matrix(Improved ~ . -1, data = df)
head(sparse_matrix)

output_vector <- df[,Improved] == "Marked"

bst <- xgboost(data = sparse_matrix, label = output_vector, max.depth = 4,
               eta = 1, nthread = 2, nround = 10, objective = "binary:logistic")

importance <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst)
head(importance)

importanceRaw <- xgb.importance(feature_names = sparse_matrix@Dimnames[[2]], model = bst, data = sparse_matrix, label = output_vector)
# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)

xgb.plot.importance(importance_matrix = importanceRaw)

# X2 Test - higher better
c2 <- chisq.test(df$Age, output_vector)
print(c2)

c2 <- chisq.test(df$AgeDiscret, output_vector)
print(c2)


# https://github.com/dmlc/xgboost/tree/master/demo

# http://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/

library(nycflights13)

