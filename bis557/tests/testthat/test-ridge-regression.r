#This test script's framework is modified from the version of test-ridge-regression.r

#simulate the data:reference:<Ridge Regression and Principal Component Analysis> P.50


library(testthat)

context("Test the output of homework 2 Q2.")

test_that("You ridge_regression() function works in case1.", {
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
  fit_ridge <- ridge_regression(formula =Y ~ ., data = data,lambda=1)

  fit_lm <- lm.ridge(formula =Y ~ ., data = data,lambda=1)

  expect_equivalent(fit_lm$coef, fit_ridge$coefficients[-1],
                    tolerance = 1e-1)
})

test_that("You ridge_regression() function works in case2.", {
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

  fit_ridge <- ridge_regression(formula =Y ~ ., data = data,lambda=0.5)

  fit_lm <- lm.ridge(formula =Y ~ ., data = data,lambda=0.5)

  expect_equivalent(fit_lm$coef, fit_ridge$coefficients[-1],
                    tolerance = 1e-1)
})

test_that("You ridge_regression() function works in case3.", {
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
  fit_ridge <- ridge_regression(formula =Y ~ ., data = data,lambda=1.2)

  fit_lm <- lm.ridge(formula =Y ~ ., data = data,lambda=1.2)

  expect_equivalent(fit_lm$coef, fit_ridge$coefficients[-1],
                    tolerance = 1e-1)
})

