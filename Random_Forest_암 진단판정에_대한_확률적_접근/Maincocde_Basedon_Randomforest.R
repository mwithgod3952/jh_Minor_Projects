library(mlbench)
library(dplyr)

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


