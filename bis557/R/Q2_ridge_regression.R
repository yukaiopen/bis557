#' @title conduct the ridge regression
#'
#' @description This function is aimed to conduct the ridge regression
#' @param formula The equation of the ridge regression model
#' @param data The data needed to be fitted by ridge model
#' @param lambda the parameter for the ridge regression
#' @return A list of the coefficients of the ridge model
#' @examples
#' ridge_regression(formula = Sepal.Length ~ ., data = iris,lambda=1)
#' lm.ridge(formula = Sepal.Length ~ ., data = iris,lambda=1)
#' @import stats
#' @export


ridge_regression <- function(formula,data,lambda) {

  vars<-all.vars(formula)
   X <- model.matrix(formula, data)
   Y <- matrix(data[,vars[1]], ncol = 1)
  #Singular Value Decomposition of a Matrix
  #Reference to the textbook: <Ridge Regression and Principal Component Analysis>  P.52
  U <- svd(X)$u
  V <- svd(X)$v
  d <- svd(X)$d
  D <- diag(d/(d^2 + lambda))
  coefficients <-V%*%D%*%t(U)%*%Y

  #deal with data structure
  coefficients<-data.frame(coefficients)
  #rename the columns
  colnames(coefficients)="coefficients"
  return(coefficients)
}
