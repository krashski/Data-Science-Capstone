#### MODEL BUILDING

# load RDS
df.final <- readRDS('dffinal.RDS')

# exploratory data analysis
# percentage of restaurant failure overall
round(prop.table(table(df.final$Target)), 3)

# restaurant failure by city
round(prop.table(table(df.final$city, df.final$Target), 1), 3)

# restaurant failure by category
round(prop.table(table(df.final$cat, df.final$Target), 1), 3)

x <- as.data.frame(round(prop.table(table(df.final$cat, df.final$Target), 1), 3))
index <- with(x, order(Freq, decreasing = TRUE))
x[index, ]

# remove variables not used for modeling
# (anything not beginning with a capital letter)
discard <- grep('^[a-z]', names(df.final), value = TRUE)
df.model <- subset(df.final, select = !names(df.final) %in% discard)

# rearrange data frame to put target variable last
pred <- df.model[, -45]
df.model <- cbind(pred, df.model$Target)
names(df.model)[50] <- 'Target'

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

#############################################################

# load required libraries
library(doParallel)
library(xgboost)
library(pROC)
library(plyr)

# enable multi-core processing
cl <- makeCluster(detectCores())

# create a data frame to hold the results
testResults <- data.frame(Target = df.test$Target)
holdResults <- data.frame(Target = df.hold$Target)

# set training control parameters
cvCtrl <- trainControl(method = "repeatedcv", repeats = 5,
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE)
xgbGrid <- expand.grid(nrounds = seq(50, 250, by = 50),
                       max_depth = c(1:5),
                       eta = 0.3)

# begin parallel processing
registerDoParallel(cl)
set.seed(42)
xgbTune <- train(Target ~., data = df.train, method = 'xgbTree', metric = 'ROC',
                 tuneGrid = xgbGrid, trControl = cvCtrl)

# stop parallel processing
stopCluster()

# model results
xgbTune
plot(xgbTune)

# predictions on test data
testResults$xgbCat <- predict(xgbTune, df.test)
testResults$xgbProb <- predict(xgbTune, df.test, type = 'prob')[, 1]

# confusion matrix for test data
xgbCM <- confusionMatrix(predict(xgbTune, df.test), testResults$Target)
xgbCM

# predictions on holdout data
holdResults$xgbCat <- predict(xgbTune, df.hold)
holdResults$xgbProb <- predict(xgbTune, df.hold, type = 'prob')[, 1]

# ROC curve
xgbROC <- roc(holdResults$Target, holdResults$xgbProb,
              levels = rev(levels(df.hold$Target)))

# find optimal alternate ROC cutoff based on Youden's J
xgbThresh <- coords(xgbROC, x = 'best', ret = 'threshold', best.method = 'youden')
xgbThresh2 <- coords(xgbROC, x = 'best', best.method = 'youden')

# predictions based on alternate threshold
holdResults$xgbAlt <- factor(ifelse(holdResults$xgbProb > xgbThresh, 'Yes', 'No'), 
                             levels = levels(df.hold$Target))

# alternate confusion matrix
xgbAltCM <- confusionMatrix(holdResults$xgbAlt, holdResults$Target)
xgbAltCM

# plot ROC with original and alternate thresholds
plot(xgbROC, print.thres = c(.5, xgbThresh), type = "S",
     print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
     print.thres.cex = .8, legacy.axes = TRUE)

plot(varImp(xgbTune))

save.image(file = 'models_final.RData')

