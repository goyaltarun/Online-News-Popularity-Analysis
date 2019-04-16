rm(list=ls(all=TRUE))



#################################################
#          Online News Popularity Dataset
#################################################

OnlineNewsPopularity <-read.csv('OnlineNewsPopularity.csv',header = TRUE)

context2 <- within(OnlineNewsPopularity, rm(url, timedelta))

context2$shares_new <- 0

context2$shares_new[context2$shares > 1400] <- 1

context3 <- within(context2, rm(shares))

maxs <- apply(context3, 2, max) 
mins <- apply(context3, 2, min)

context4 <- as.data.frame(scale(context3, center = mins, scale = maxs - mins))



maxClusters	<-	15


wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) {
  set.seed(123)
  model <- kmeans(context4,centers=i,nstart=20)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

OnlineNewsPopularity_Cluster <- kmeans(context4, 6, nstart = 20)
OnlineNewsPopularity_Cluster

library(factoextra)
fviz_cluster(OnlineNewsPopularity_Cluster, data = context4)

library(fastICA)

a <- fastICA(context4, 6, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

par(mfrow = c(1))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")

names(context4[,c(1:58)])

pca.out<-prcomp(context4[,c(1:58)],scale=TRUE)
pca.out

new_z <- pca.out$x
new_z

summary(pca.out)

plot(pca.out)

biplot(pca.out,scale = 0, cex=0.65)

dim(context4)
dim(pca.out$x)

context5 <- pca.out$x

context6 <- as.data.frame(context5[,c(1:31)])

context6$shares <- context4$shares_new

x_train<- as.data.frame(context6[0:-1000,])
x_test<- as.data.frame(context6[38645:39644,])
nrow(x_train)
nrow(x_test)
maxClusters	<-	15


wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) {
  set.seed(123)
  model <- kmeans(context6,centers=i,nstart=20)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

OnlineNewsPopularity_Cluster_new <- kmeans(context6, 8, nstart = 20)
OnlineNewsPopularity_Cluster_new

library(neuralnet)


nn_2layer <- neuralnet(shares ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 
                       + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21 
                       + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30 + PC31,
                       data=x_train,hidden=c(7,5), act.fct = "logistic", linear.output=T)
plot(nn_2layer)
nn<-compute(nn_2layer,x_train)
y_pred<-nn_2layer$response
head(x_train)
x_train$PC1
table(context6$shares,y_pred)





rm(list=ls(all=TRUE))



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

context4 <- as.data.frame(scale(context3, center = mins, scale = maxs - mins))



maxClusters	<-	15


wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) {
  set.seed(123)
  model <- kmeans(context4,centers=i,nstart=20)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

Bank_Cluster <- kmeans(context4, 4, nstart = 20)
Bank_Cluster

library(factoextra)
fviz_cluster(Bank_Cluster, data = context4)

library(fastICA)

a <- fastICA(context4, 6, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "C", row.norm = FALSE, maxit = 200, 
             tol = 0.0001, verbose = TRUE)

par(mfrow = c(1,1))
plot(a$X, main = "Pre-processed data")
plot(a$X %*% a$K, main = "PCA components")
plot(a$S, main = "ICA components")


pca.out<-prcomp(context4[,c(1:10)],scale=TRUE)
pca.out

new_z <- pca.out$x
new_z

summary(pca.out)

plot(pca.out)

biplot(pca.out,scale = 0, cex=0.65)

dim(context4)
dim(pca.out$x)

context5 <- pca.out$x

context6 <- as.data.frame(context5[,c(1:8)])

context6$y_new <- context4$y
nrow(context6)
x_train<- as.data.frame(context6[0:-1000,])
x_test<- as.data.frame(context6[44212:45211,])
nrow(x_train)
nrow(x_test)
maxClusters	<-	15


wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) {
  set.seed(123)
  model <- kmeans(context6,centers=i,nstart=20)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

Bank_Cluster_new <- kmeans(context6, 8, nstart = 20)
Bank_Cluster_new


library(neuralnet)


nn_2layer <- neuralnet(y_new ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8,
                       data=x_train,hidden=c(7,5), act.fct = "logistic", linear.output=T)
plot(nn_2layer)
nn<-compute(nn_2layer,x_train)
y_pred<-nn_2layer$response
head(x_train)
x_train$PC1
table(context6$shares,y_pred)



