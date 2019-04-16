rm(list=ls(all=TRUE))

library(neuralnet)

#################################################
#          Online News Popularity Dataset
#################################################

OnlineNewsPopularity <-read.csv('OnlineNewsPopularity.csv',header = TRUE)

context2 <- within(OnlineNewsPopularity, rm(url, timedelta))

context2$shares_new <- 0

context2$shares_new[context2$shares > 1400] <- 1

maxs <- apply(context2, 2, max) 
mins <- apply(context2, 2, min)

scaled <- as.data.frame(scale(context2, center = mins, scale = maxs - mins))
index <- sample(1:nrow(context2),round(0.75*nrow(context2)))
train <- scaled[index,]
test <- scaled[-index,]


n <- names(train)
f <- as.formula(paste("shares_new~", paste(n[!n %in% "shares_new"], collapse = " + ")))
nn_2layer <- neuralnet(f,data=train,hidden=c(7,5), act.fct = "logistic", linear.output=T)
plot(nn_2layer)

nn_2_tanh <- neuralnet(f,data=train, hidden=c(7,5), act.fct = "tanh", linear.output = T)
plot(nn_2_tanh)

nn_2layer2 <- neuralnet(f,data=train,hidden=c(5,2), act.fct = "logistic", linear.output=T)
plot(nn_2layer2)

#nn_1layer <- neuralnet(f,data=train,hidden=1, act.fct = "logistic", linear.output=T)
#plot(nn_1layer)

nn_2tanh2 <- neuralnet(f,data=train,hidden=c(5,2), act.fct = "tanh", linear.output=T)
plot(nn_2tanh2)

#nn_1_tanh <- neuralnet(f, data = train, hidden =1, act.fct = "tanh", linear.output = T)
#plot(nn_1_tanh)

pr.nn_2layer <- compute(nn_2layer,test[,1:59])
pr.nn_2layer2 <- compute(nn_2layer2,test[,1:59])
pr.nn_2tanh <- compute(nn_2_tanh,test[,1:59])
pr.nn_2tanh2 <- compute(nn_2tanh2,test[,1:59])

pr1.nn_2layer <- pr.nn_2layer$net.result*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)
test.r_nn_2layer <- (test$shares_new)*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)

pr1.nn_2layer2 <- pr.nn_2layer2$net.result*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)
test.r_nn_2layer2 <- (test$shares_new)*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)

pr1.nn_2tanh <- pr.nn_2tanh$net.result*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)
test.r_nn_2tanh <- (test$shares_new)*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)

pr1.nn_2tanh2 <- pr.nn_2tanh2$net.result*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)
test.r_nn_2tanh2 <- (test$shares_new)*(max(context2$shares_new)-min(context2$shares_new))+min(context2$shares_new)

MSE.nn_2layer <- sum((test.r_nn_2layer - pr1.nn_2layer)^2)/nrow(test)
MSE.nn_2layer2 <- sum((test.r_nn_2layer2 - pr1.nn_2layer2)^2)/nrow(test)
MSE.nn_2tanh <- sum((test.r_nn_2tanh - pr1.nn_2tanh)^2)/nrow(test)
MSE.nn_2tanh2 <- sum((test.r_nn_2tanh2 - pr1.nn_2tanh2)^2)/nrow(test)

print(paste(MSE.nn_2layer,MSE.nn_2layer2,MSE.nn_2tanh,MSE.nn_2tanh2))

par(mfrow=c(1,2))

plot(test$shares_new,pr1.nn_2layer,col='red',main='Actual vs Predicted',pch=18,cex=0.7, ylim = c(-1,1))
abline(0,1,lwd=2)
legend('bottomright',legend='Logistic (7,5)',pch=18,col='red', bty='n')

plot(test$shares_new,pr1.nn_2layer2,col='red',main='Actual vs Predicted',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Logistic (5,2)',pch=18,col='red', bty='n')

plot(test$shares_new,pr1.nn_2tanh,col='red',main='Actual vs Predicted',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Tanh (7,5)',pch=18,col='red', bty='n')

plot(test$shares_new,pr1.nn_2tanh2,col='red',main='Actual vs Predicted',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Tanh (5,2)',pch=18,col='red', bty='n')



OnlineNewsPopularity <-read.csv('OnlineNewsPopularity.csv',header = TRUE)

context2 <- within(OnlineNewsPopularity, rm(url, timedelta))

context2$shares_new <- 0

context2$shares_new[context2$shares > 1400] <- 1

context2$shares_new = as.factor(context2$shares_new)

index <- sample(1:nrow(context2),round(0.75*nrow(context2)))
train <- context2[index,]
test <- context2[-index,]

library(class)

knn.1 <-  knn(train, test, train$shares_new, k=1)
knn.5 <-  knn(train, test, train$shares_new, k=5)
knn.20 <- knn(train, test, train$shares_new, k=20)

sum(test$shares_new == knn.1)/100
sum(test$shares_new == knn.5)/100
sum(test$shares_new == knn.20)/100

table(knn.1 ,test$shares_new)
table(knn.5 ,test$shares_new)
table(knn.20 ,test$shares_new)




rm(list=ls(all=TRUE))

library(neuralnet)

#################################################
#          Bank Marketing Dataset
#################################################

context2 <-read.csv('bank-full.csv',header = TRUE, sep = ';')

context2$y <- ifelse(context2$y == 'yes', 1, 0)
context2$housing <- ifelse(context2$housing == 'yes', 1, 0)
context2$loan <- ifelse(context2$loan == 'yes', 1, 0)
context2$default <- ifelse(context2$default == 'yes', 1, 0)

context3 <- context2[sapply(context2,is.numeric)]

maxs <- apply(context3, 2, max) 
mins <- apply(context3, 2, min)

scaled <- as.data.frame(scale(context3, center = mins, scale = maxs - mins))
index <- sample(1:nrow(context3),round(0.75*nrow(context3)))
train <- scaled[index,]
test <- scaled[-index,]


n <- names(train)
f <- as.formula(paste("y~", paste(n[!n %in% "y"], collapse = " + ")))
nn_2layer <- neuralnet(f,data=train,hidden=2, act.fct = "logistic", linear.output=T)
plot(nn_2layer)

nn_2_tanh <- neuralnet(f,data=train, hidden=2, act.fct = "tanh", linear.output = T)
plot(nn_2_tanh)

nn_2layer2 <- neuralnet(f,data=train,hidden=1, act.fct = "logistic", linear.output=T)
plot(nn_2layer2)

nn_2tanh2 <- neuralnet(f,data=train,hidden=1, act.fct = "tanh", linear.output=T)
plot(nn_2tanh2)

pr.nn_2layer <- compute(nn_2layer,test[,1:10])
pr.nn_2layer2 <- compute(nn_2layer2,test[,1:10])
pr.nn_2tanh <- compute(nn_2_tanh,test[,1:10])
pr.nn_2tanh2 <- compute(nn_2tanh2,test[,1:10])

pr1.nn_2layer <- pr.nn_2layer$net.result*(max(context3$y)-min(context3$y))+min(context3$y)
test.r_nn_2layer <- (test$y)*(max(context3$y)-min(context3$y))+min(context3$y)

pr1.nn_2layer2 <- pr.nn_2layer2$net.result*(max(context3$y)-min(context3$y))+min(context3$y)
test.r_nn_2layer2 <- (test$y)*(max(context3$y)-min(context3$y))+min(context3$y)

pr1.nn_2tanh <- pr.nn_2tanh$net.result*(max(context3$y)-min(context3$y))+min(context3$y)
test.r_nn_2tanh <- (test$y)*(max(context3$y)-min(context3$y))+min(context3$y)

pr1.nn_2tanh2 <- pr.nn_2tanh2$net.result*(max(context3$y)-min(context3$y))+min(context3$y)
test.r_nn_2tanh2 <- (test$y)*(max(context3$y)-min(context3$y))+min(context3$y)

MSE.nn_2layer <- sum((test.r_nn_2layer - pr1.nn_2layer)^2)/nrow(test)
MSE.nn_2layer2 <- sum((test.r_nn_2layer2 - pr1.nn_2layer2)^2)/nrow(test)
MSE.nn_2tanh <- sum((test.r_nn_2tanh - pr1.nn_2tanh)^2)/nrow(test)
MSE.nn_2tanh2 <- sum((test.r_nn_2tanh2 - pr1.nn_2tanh2)^2)/nrow(test)

print(paste(MSE.nn_2layer,MSE.nn_2layer2,MSE.nn_2tanh,MSE.nn_2tanh2))

par(mfrow=c(1,2))

plot(test$y,pr1.nn_2layer,col='red',main='Actual vs Predicted',pch=18,cex=0.7, ylim = c(-1,1))
abline(0,1,lwd=2)
legend('bottomright',legend='Logistic 2 nodes',pch=18,col='red', bty='n')

plot(test$y,pr1.nn_2layer2,col='red',main='Actual vs Predicted',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Logistic 1 nodes',pch=18,col='red', bty='n')

plot(test$y,pr1.nn_2tanh,col='red',main='Actual vs Predicted',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Tanh 2 nodes',pch=18,col='red', bty='n')

plot(test$y,pr1.nn_2tanh2,col='red',main='Actual vs Predicted',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='Tanh 1 nodes',pch=18,col='red', bty='n')


context2 <-read.csv('bank-full.csv',header = TRUE, sep = ';')

context2$y <- ifelse(context2$y == 'yes', 1, 0)
context2$housing <- ifelse(context2$housing == 'yes', 1, 0)
context2$loan <- ifelse(context2$loan == 'yes', 1, 0)
context2$default <- ifelse(context2$default == 'yes', 1, 0)

context3 <- context2[sapply(context2,is.numeric)]

context3$y = as.factor(context3$y)

index <- sample(1:nrow(context3),round(0.75*nrow(context3)))
train <- context3[index,]
test <- context3[-index,]

library(class)

knn.1 <-  knn(train, test, train$y, k=1)
knn.5 <-  knn(train, test, train$y, k=5)

sum(test$y == knn.1)/100
sum(test$y == knn.5)/100

table(knn.1 ,test$y)
table(knn.5 ,test$y)
