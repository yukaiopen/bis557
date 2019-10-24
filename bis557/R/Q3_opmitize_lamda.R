#' @title optimizing the ridge parameter λ.
#'
#' @description This function is aimed to optimize the ridge parameter λ.
#' @param formula The equation of the ridge regression model
#' @param data The data needed to be fitted by ridge model
#' @param lambdas  a set of candidate lambda for optimizing
#' @param nsplit the fold for sampling
#' @return the best lambda
#' @examples
# best_lambda(Y~.,data,lambdas=seq(1,1000,10),10)
#' @import stats MASS caret
#' @export


best_lambda<-function(formula,data,lambdas,nsplit){
  vars<-all.vars(formula)
  X <- model.matrix(formula, data)
  Y <- matrix(data[,vars[1]], ncol = 1)
  N <- nrow(X)

  #split into nsplit
  #ref: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
  split_id <- split(sample(1:N,replace = FALSE), 1:nsplit)

  #conduct cross validation
  MSE<-vector()
  for(i in 1:length(lambdas))
  {
    MSE[i]<-0
    for(j in 1:nsplit)
    {
      coef <- ridge_regression(formula,data[-split_id[[j]],],lambda=lambdas[i])
      MSE[i] <-MSE[i]+sum((X[split_id[[j]], ] %*% as.matrix(coef) - Y[split_id[[j]],])^2)
    }
    MSE[i]<- MSE[i]/nsplit
  }

  #get the lambda for the least MSE
  lambda <- lambdas[which.min(MSE)]

  #return the lambda
  return(lambda)
}
