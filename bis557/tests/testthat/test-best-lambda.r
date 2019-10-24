#This test script's framework is modified from the version of test-best-lambda.r

#simulate the data:reference:<Ridge Regression and Principal Component Analysis> P.50


library(testthat)

context("Test the output of homework 2 Q3.")

test_that("You best_lambda() function works in case1.", {
  #simulate the data:
  n <- 200; p <- 4; N <- 500; M <- 20
  beta <- c(1, -1, 0.5, 0)
  mu <- rep(0, p)
  Sigma <- matrix(0.9, nrow = p, ncol = p)
  diag(Sigma) <- 1
  X <- MASS::mvrnorm(n, mu, Sigma)
  y <- X %*% beta + rnorm(n, sd = 5)
  x_test <- MASS::mvrnorm(n, mu, Sigma)
  y_test <- x_test %*% beta + rnorm(n, sd = 5)
  y_test <- as.numeric(y_test)
  data<-cbind(y_test,x_test)
  colnames(data)<-c('Y','X1','X2','X3','X4')
  data<-data.frame(data)

  #test
  best_lambda <- best_lambda(Y~.,data,lambdas=seq(1,1000,10),10)

  fit_lm <- cv.glmnet(x=x_test,y=y_test,lambda=seq(1,1000,10))

  expect_equivalent(fit_lm$lambda.min, best_lambda,
                    tolerance = 20)
})


test_that("You best_lambda() function works in case2.", {
  #simulate the data:
  n <- 200; p <- 4; N <- 500; M <- 20
  beta <- c(1, -1, 0.5, 0)
  mu <- rep(0, p)
  Sigma <- matrix(0.9, nrow = p, ncol = p)
  diag(Sigma) <- 1
  X <- MASS::mvrnorm(n, mu, Sigma)
  y <- X %*% beta + rnorm(n, sd = 5)
  x_test <- MASS::mvrnorm(n, mu, Sigma)
  y_test <- x_test %*% beta + rnorm(n, sd = 5)
  y_test <- as.numeric(y_test)
  data<-cbind(y_test,x_test)
  colnames(data)<-c('Y','X1','X2','X3','X4')
  data<-data.frame(data)

  #test
  best_lambda <- best_lambda(Y~.,data,lambdas=seq(1,2000,20),50)

  fit_lm <- cv.glmnet(x=x_test,y=y_test,lambda=seq(1,2000,20))

  expect_equivalent(fit_lm$lambda.min, best_lambda,
                    tolerance = 20)
})

test_that("You best_lambda() function works in case3.", {
  #simulate the data:
  n <- 200; p <- 4; N <- 500; M <- 20
  beta <- c(1, -1, 0.5, 0)
  mu <- rep(0, p)
  Sigma <- matrix(0.9, nrow = p, ncol = p)
  diag(Sigma) <- 1
  X <- MASS::mvrnorm(n, mu, Sigma)
  y <- X %*% beta + rnorm(n, sd = 5)
  x_test <- MASS::mvrnorm(n, mu, Sigma)
  y_test <- x_test %*% beta + rnorm(n, sd = 5)
  y_test <- as.numeric(y_test)
  data<-cbind(y_test,x_test)
  colnames(data)<-c('Y','X1','X2','X3','X4')
  data<-data.frame(data)

  #test
  best_lambda <- best_lambda(Y~.,data,lambdas=seq(1,2,0.02),50)

  fit_lm <- cv.glmnet(x=x_test,y=y_test,lambda=seq(1,2,0.02))

  expect_equivalent(fit_lm$lambda.min, best_lambda,
                    tolerance = 0.1)
})
