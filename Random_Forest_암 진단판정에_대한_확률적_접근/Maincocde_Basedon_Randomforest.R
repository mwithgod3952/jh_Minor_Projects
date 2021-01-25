library(mlbench)
library(dplyr)
library(randomForest)
library(cluster)
library(cluster)

"Random_Forest_암 진단판정에_대한_확률적_접근"

data(BreastCancer)
str(BreastCancer)
BreastCancer %>% head()

#'@ST1_Type_변환
df = BreastCancer[-1]
df <- cbind(lapply(df[-10], function(x) as.numeric(as.character(x))), df[10])
str(df)

#'@Class-확인
distinct(select(df, Class))
# Class
# 1    benign
# 6 malignant

#' @ST2_Test/Trainset_생성
set.seed(123)
train.idx = sample(nrow(df), 0.7*nrow(df))
df.trainset = df[train.idx,]
df.testset = df[-train.idx,]
table(df.trainset$Class)
table(df.testset$Class)

# > table(df.trainset$Class)
# benign malignant 
# 308       181 
# 
# > table(df.testset$Class)
# benign malignant 
# 150        60 

#' @ST4_Model-Random_Forest
df.forest <- randomForest(Class ~ ., data = df.trainset,
                          na.action = na.roughfix,
                          importance = TRUE)

#' @결과확인1_ 예측결과 확률확인
df.forest.pred <- predict(df.forest, newdata = df.testset, type = "prob")
#' @결과확인1_ 예측결과 번주확인
df.forest.pred <- predict(df.forest, newdata = df.testset, type = "response")

#' @ST5_예측정확도 확인 
table(df.testset$Class, df.forest.pred,
      dnn = c("Actual", "Predicted"))

# Actual      benign  malignant
# benign        142          6
# malignant       1         58

mean(df.testset$Class == df.forest.pred, na.rm = TRUE)
#' @[1]_0.9661836, 예측정확도 96%

library(cluster)
windows()
clusplot(x=na.omit(df.testset[,-10]), clus=na.omit(df.forest.pred),
                   color=TRUE, shade=TRUE, labels=4, lines=0,
                   main="Classification from Breast Cancer Dataset")

