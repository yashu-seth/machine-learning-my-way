library(caret)
library(Hmisc)
setwd("C:/work/R")

data <- read.csv("kaggle.csv")
data$Survived <- as.factor(data$Survived)
data$Embarked <- as.factor(data$Embarked)

for(i in 1:dim(data)[1]){
    if(is.na(data[i,"Age"])){
        if(data[i,"SibSp"]==3){
            data[i,"Age"] <- 22
        }
        else if(data[i,"SibSp"]>3){
            data[i,"Age"] <- 13
        }
        else {
            data[i,"Age"] <- 29
        }
    }
}

data$Sex <- as.factor(data$Sex)
data$Pclass <- as.factor(data$Pclass)

data$family <- sapply(strsplit(as.character(data$Name), ','),
                      function(x){x[1]})

data$family <- as.factor(data$family)

set.seed(2503)
intrain <- createDataPartition(data$Survived, p=0.68)

dtrain <- data[intrain[[1]],]
dtest <- data[-intrain[[1]],]

predictors <- c(2,3,5)
dtrain <- dtrain[,predictors]
dtest <- dtest[,predictors]


male <- dtrain$Sex=='male'
dt_male <- dtrain[male,-3]

dt_female <- dtrain[!male,-3]

ctrl <- trainControl(method='repeatedcv', repeats = 3)
knn_fit <- train(Survived~., data = dtrain,
                 method='knn', tuneLength = 15,
                 trControl = ctrl, preProc = c("center", "scale"))
knn_pred <- predict(knn_fit, dtest[,-1])

ctrl2 <- trainControtl(method='repeatedcv', repeats = 3)
log_fit <- train(Survived~., data=dtrain, method='glm',
                 family='binomial', tuneLength=25,
                 trControl = ctrl2)
log_pred <- predict(log_fit, dtest[,-1])

ggplot(aes(x=PassengerId,y=family,colour=Survived),
       data=data)+geom_point()+scale_y_discrete(labels=abbreviate)

lda_fit <- train(Survived~., data=dtrain, method='lda',
                 tuneLength=15, trControl = ctrl)
lda_pred <- predict(lda_fit, dtest[,-1])

qda_fit <- train(Survived~., data=dt_male, method='qda',
                 tuneLength=15, trControl = ctrl)
qda_pred <- predict(qda_fit, dtest[,-1])

sum(predict(rf_fit, dtest[,-1])==dtest$Survived)/dim(dtest)[1]



rf_fit <- train(Survived~., method = 'rf',
                data = dtrain,
                preProc = c('center', 'scale'),
                ntree = 60)
rf_pred <- predict(rf_fit, dtest[,-1])

pred_dtrain <- data.frame(knn_pred, log_pred, lda_pred, qda_pred,
                          rf_pred,
                          Survived=dtest$Survived)

com_fit <- train(Survived~., method='gam', data=pred_dtrain)


tree_fit <- train(Survived~., method = 'rpart', data = dtrain)
tree_pred <- predict(tree_fit, dtest[,-1])
library('rattle')
fancyRpartPlot(tree_fit$finalModel)
