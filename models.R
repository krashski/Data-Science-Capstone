
#### MODEL BUILDING

# load RDS
df.final <- readRDS('dffinal.RDS')

# remove variables not used for modeling
# (anything not beginning with a capital)
discard <- grep('^[a-z]', names(df.final), value = TRUE)
df.model <- subset(df.final, select = !names(df.final) %in% discard)

# recode target variable into a factor for model building
df.model$Target <- factor(df.model$Target, levels = c('1', '0'),
                          labels = c('Yes', 'No'))

# rearrange data frame to put target variable last
pred <- df.model[, -45]
df.model <- cbind(pred, df.model$Target)
names(df.model)[53] <- 'Target'

# split the data 60/20/20 into three sets: training, test and holdout. 
library(caret)
set.seed(42)
split1 <- createDataPartition(df.model$Target, p = .6)[[1]]
other <- df.model[-split1, ]
df.train <- df.model[split1, ]

set.seed(147)
split2 <- createDataPartition(other$Target, p = .5)[[1]]
df.test <- other[split2, ]
df.hold <- other[-split2, ]

# save workspace
rm(df.final, df.model, other, split1, split2, pred, discard)
save.image(file = 'models.RData')

# load workspace
load('models.RData')

# gradient boosted trees using xgboost
library(xgboost)
library(pROC)

# set tuning parameters
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)

# create a data frame to hold the results
testResults <- data.frame(Target = df.test$target)

# tune the model
xgbTune <- train(target ~., data = df.train, method = 'xgbTree', metric = 'ROC',
                 trControl = cvCtrl)

# model results
xgbTune
plot(xgbTune)

# predictions on test data
testResults$xgbCat <- predict(xgbTune, df.test)
testResults$xgbProb <- predict(xgbTune, df.test, type = 'prob')[, 1]

# compute ROC
xgbROC <- roc(testResults$Target, testResults$xgbProb,
              levels = rev(levels(df.test$target)))

# confusion matrix
xgbCM <- confusionMatrix(predict(xgbTune, df.test), testResults$Target)
xgbCM

# compute alternate threshold using Youden's J
xgbThresh <- coords(xgbROC, x = 'best', ret = 'threshold', best.method = 'youden')

# predictions on test data using alternate threshold
testResults$xgbAlt <- factor(ifelse(testResults$xgbProb > xgbThresh, 'Yes', 'No'), 
                             levels = levels(df.test$target))

# confusion matrix
xgbAltCM <- confusionMatrix(testResults$xgbAlt, testResults$Target)
xgbAltCM

# plot ROC for default and alternate thresholds
plot(xgbROC, print.thres = c(.5, xgbThresh), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, legacy.axes = TRUE)

# plot variable importance
plot(varImp(xgbTune))

# save workspace
save.image(file = 'models_final.RData')
