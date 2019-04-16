rm(list=ls(all=TRUE))

library(kernlab)

#################################################
#          Online News Popularity Dataset
#################################################

OnlineNewsPopularity <-read.csv('OnlineNewsPopularity.csv',header = TRUE)

context2 <- within(OnlineNewsPopularity, rm(url, timedelta))

context2$shares_new <- 0

context2$shares_new[context2$shares > 1400] <- 1

#>>------Creating training and test dataset

## 75% of the sample size
smp_size <- floor(0.75 * nrow(context2))


set.seed(123)
train_ind <- sample(seq_len(nrow(context2)), size = smp_size)

train <- context2[train_ind, ]
test <- context2[-train_ind, ]

#-----------Linear Kernel---------------------#
svp_linear <- ksvm(shares_new~., data = train,type="C-svc",kernel="vanilladot")
svp_linear

#-->>Error in training and test data set
err_train_linear <- 1-mean(predict(svp_linear, train) == train$shares_new)
err_test_linear <- 1-mean(predict(svp_linear, test) == test$shares_new)
#>>----ROC Curve for training
pred_linear_train <- predict(svp_linear,train)
ROCRpred_linear_train<- ROCR::prediction(pred_linear_train, train$shares_new)
ROCRperf_linear_train <- ROCR::performance(ROCRpred_linear_train, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_linear_train, colorize = TRUE)  + title("Train ROC")

#>>---ROC Curve for testing
pred_linear_test <- predict(svp_linear,test)
ROCRpred_linear_test<- ROCR::prediction(pred_linear_test, test$shares_new)
ROCRperf_linear_test <- ROCR::performance(ROCRpred_linear_test, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_linear_test, colorize = TRUE)  + title("Test ROC")

#-----------Polynomial(degree = 2) Kernel---------------------#

svp_poly <- ksvm(shares_new~., data = train, type="C-svc",C = 100, kernel="polydot",kpar=list(degree = 2),scaled=c())
svp_poly
#-->>Error in training and test data set
err_train_poly <- 1-mean(predict(svp_poly, train) == train$shares_new)
err_test_poly <- 1-mean(predict(svp_poly, test) == test$shares_new)

#>>----ROC Curve for training
pred_poly_train <- predict(svp_poly,train)
ROCRpred_poly_train<- ROCR::prediction(pred_poly_train, train$shares_new)
ROCRperf_poly_train <- ROCR::performance(ROCRpred_poly_train, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_poly_train, colorize = TRUE)  + title("Train ROC")

#>>---ROC curve for testing
pred_poly_test <- predict(svp_poly,test)
ROCRpred_poly_test<- ROCR::prediction(pred_poly_test, test$shares_new)
ROCRperf_poly_test <- ROCR::performance(ROCRpred_poly_test, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_poly_test, colorize = TRUE)  + title("Test ROC")

#-----------Radial Basis kernel "Gaussian" Kernel---------------------#

svp_rbf <- ksvm(shares_new~., data = train, type="C-svc",C = 100, kernel="rbfdot",kpar=list(sigma=0.05),scaled=c())
svp_rbf

#-->>Error in training and test data set
err_train_rbf <- 1-mean(predict(svp_rbf, train) == train$shares_new)
err_test_rbf <- 1-mean(predict(svp_rbf, test) == test$shares_new)

#>>--ROC Curve for training dataset
pred_rbf_train <- predict(svp_rbf,train)
ROCRpred_rbf_train<- ROCR::prediction(pred_rbf_train, train$shares_new)
ROCRperf_rbf_train <- ROCR::performance(ROCRpred_rbf_train, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_rbf_train, colorize = TRUE)  + title("Train ROC")

#>>--ROC Curve for testing dataset
pred_rbf_test <- predict(svp_rbf,test)
ROCRpred_rbf_test<- ROCR::prediction(pred_rbf_test, test$shares_new)
ROCRperf_rbf_test <- ROCR::performance(ROCRpred_rbf_test, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_rbf_test)  + title("Test ROC")


#>>----Decision tree

library(rpart)
# fit model
fit <- rpart(shares~., data=train, control=rpart.control(minsplit=5), parms = list(split = "information"))
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, train)
# summarize accuracy
printcp(fit) # display the results 
plotcp(fit)
plot(fit, uniform=TRUE, main="Classification Tree for Online News Popularity")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#>>---Pruning dataset
pfit<- prune(fit, cp = 0.018453)

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Online News Popularity")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#>>---Boosting Decision  tree
train$shares_new <- as.factor(train$shares_new)
treecontrol <- trainControl(method = "repeatedcv", number =5, repeats = 5)
library(caret)
boosted_train <- train(shares_new~., data = train, method = "AdaBoost.M1", 
               tuneGrid = expand.grid(maxdepth = 5, mfinal = 10, coeflearn = "Zhu"),
               trCtrl = treecontrol)




#################################################
#          Bank Marketing Dataset
#################################################

context2 <-read.csv('bank-full.csv',header = TRUE, sep = ';')

context2$y_new <- 0

context2$y_new[context2$y == 'yes'] <- 1

#>>------Creating training and test dataset
## 75% of the sample size
smp_size <- floor(0.70 * nrow(context2))

set.seed(123)
train_ind <- sample(seq_len(nrow(context2)), size = smp_size)
anyNA(context2)
train <- context2[train_ind, ]
test <- context2[-train_ind, ]

#-----------Linear Kernel---------------------#
svp_linear <- ksvm(y_new~., data = train,type="C-svc",kernel="vanilladot")
svp_linear

#-->>Error in training and test data set
err_train_linear <- 1-mean(predict(svp_linear, train) == train$y_new)
err_test_linear <- 1-mean(predict(svp_linear, test) == test$y_new)

#>>---ROC Curve for training dataset
pred_linear_train <- predict(svp_linear,train)
ROCRpred_linear_train<- ROCR::prediction(pred_linear_train, train$y_new)
ROCRperf_linear_train <- ROCR::performance(ROCRpred_linear_train, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_linear_train, colorize = TRUE)  + title("Train ROC")

#>>---ROC Curve for testing dataset
pred_linear_test <- predict(svp_linear,test)
ROCRpred_linear_test<- ROCR::prediction(pred_linear_test, test$y_new)
ROCRperf_linear_test <- ROCR::performance(ROCRpred_linear_test, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_linear_test, colorize = TRUE)  + title("Test ROC")

#-----------Polynomial(degree = 2) Kernel---------------------#

svp_poly <- ksvm(y_new~., data = train, type="C-svc",C = 100, kernel="polydot",kpar=list(degree = 2),scaled=c())
svp_poly

#-->>Error in training and test data set
err_train_poly <- 1-mean(predict(svp_poly, train) == train$y_new)
err_test_poly <- 1-mean(predict(svp_poly, test) == test$y_new)

#>>--ROC Curve for training dataset
pred_poly_train <- predict(svp_poly,train)
ROCRpred_poly_train<- ROCR::prediction(pred_poly_train, train$y_new)
ROCRperf_poly_train <- ROCR::performance(ROCRpred_poly_train, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_poly_train, colorize = TRUE)  + title("Train ROC")

#>>--ROC curve for testing dataset
pred_poly_test <- predict(svp_poly,test)
ROCRpred_poly_test<- ROCR::prediction(pred_poly_test, test$y_new)
ROCRperf_poly_test <- ROCR::performance(ROCRpred_poly_test, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_poly_test, colorize = TRUE)  + title("Test ROC")


#-----------Radial Basis kernel "Gaussian" Kernel---------------------#

svp_rbf <- ksvm(y_new~., data = train, type="C-svc",C = 100, kernel="rbfdot",kpar=list(sigma=0.05),scaled=c())
svp_rbf

#-->>Error in training and test data set
err_train_rbf <- 1-mean(predict(svp_rbf, train) == train$y_new)
err_test_rbf <- 1-mean(predict(svp_rbf, test) == test$y_new)

#>>---ROC Curve for training dataset
pred_rbf_train <- predict(svp_rbf,train)
ROCRpred_rbf_train<- ROCR::prediction(pred_rbf_train, train$y_new)
ROCRperf_rbf_train <- ROCR::performance(ROCRpred_rbf_train, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_rbf_train, colorize = TRUE)  + title("Train ROC")

#>>---ROC curve for testing dataset
pred_rbf_test <- predict(svp_rbf,test)
ROCRpred_rbf_test<- ROCR::prediction(pred_rbf_test, test$y_new)
ROCRperf_rbf_test <- ROCR::performance(ROCRpred_rbf_test, measure = "tpr",x.measure = "fpr")
dev.off()
plot(ROCRperf_rbf_test)  + title("Test ROC")


#>>----Decision tree

library(rpart)
# fit model
fit <- rpart(y~., data=train[1:17], control=rpart.control(minsplit=8), parms = list(split = "information"))
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, train)
# summarize accuracy
printcp(fit) # display the results 
plotcp(fit)
plot(fit, uniform=TRUE, main="Classification Tree for Bank Marketing")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

#>>---Pruning dataset
pfit<- prune(fit, cp = 0.025)

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Bank Marketing")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)

#>>--Boosting decision tree
train$y_new <- as.factor(train$y_new)
treecontrol <- trainControl(method = "repeatedcv", number =5, repeats = 5)
library(caret)
boosted_train <- train(y_new~., data = train, method = "AdaBoost.M1", 
                       tuneGrid = expand.grid(maxdepth = 5, mfinal = 10, coeflearn = "Zhu"),
                       trCtrl = treecontrol)
