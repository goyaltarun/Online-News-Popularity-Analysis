rm(list = ls(all = TRUE))

library(data.table)
context1 <- fread("OnlineNewsPopularity.csv")

summary(context1)

context2 <- within(context1, rm(url))

## 75% of the sample size
smp_size <- floor(0.75 * nrow(context2))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(context2)), size = smp_size)

train <- context2[train_ind, ]
test <- context2[-train_ind, ]
num_rows <- nrow(train)
train$timedelta <- rep(1, num_rows)

gradientDesc <- function(x, y, learn_rate, conv_threshold, n, max_iter) {
  plot(x, y, col = "blue", pch = 20, xlim=c(0,1))
  m <- runif(1, 0, 1)
  c <- runif(1, 0, 1)
  yhat <- m * x + c
  MSE <- sum((y - yhat) ^ 2) / n
  converged = F
  iterations = 0
  while(converged == F) {
    ## Implement the gradient descent algorithm
    m_new <- m - learn_rate * ((1 / n) * (sum((yhat - y) * x)))
    c_new <- c - learn_rate * ((1 / n) * (sum(yhat - y)))
    m <- m_new
    c <- c_new
    yhat <- m * x + c
    MSE_new <- sum((y - yhat) ^ 2) / n
    if(MSE - MSE_new <= conv_threshold) {
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
    iterations = iterations + 1
    if(iterations > max_iter) { 
      abline(c, m) 
      converged = T
      return(paste("Optimal intercept:", c, "Optimal slope:", m))
    }
  }
}



num_col <- ncol(train)
#x <- train[[i]]
for (i in 2:(num_col-1))
{
  
 gradientDesc(train[[i]], train$shares, 0.00003, 0.001, 32, 2500000)
  
  if(i!=num_col-1)invisible(readline(prompt="Press [enter] to continue"))
}

