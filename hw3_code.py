#Auther: Yukai Wang

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import KFold

#set up simulated data 
iris = sns.load_dataset("iris")
X = iris[["sepal_width", "petal_length", "petal_width"]]
X = X.values
Y = iris["sepal_length"]
Y = Y.values


#Code for Question1
#reference to http://fa.bianp.net/blog/2011/ridge-regression-path/
def ridge_regression(X, Y, lambda_ridge):
    U, s, Vt = np.linalg.svd(X, full_matrices=False)
    d = s / (s** 2 + lambda_ridge)
    return np.dot(d * U.T.dot(Y), Vt).T

#Code for Question2
#using Cross-validation to choose the best lambda
def best_lambda(X, Y, lambdas):
    Sum_error = [0 for i in range(len(lambdas))]
    for k in range(len(lambdas)):
        cv = KFold(n_splits=10,shuffle=False) #split into 10 folders
        for train_index, test_index in cv.split(X): # refer to https://medium.com/datadriveninvestor/k-fold-cross-validation-6b8518070833
            train_x, test_x, train_y, test_y = X[train_index], X[test_index], Y[train_index], Y[test_index]
            fit = ridge_regression(train_x, train_y, lambdas[k])
            predict_y = test_x.dot(fit)
            Sum_error[k] = Sum_error[k] + np.sum((predict_y - test_y) ** 2)
        Sum_error[k] = Sum_error[k] / 10

    return (lambdas[Sum_error.index(min(Sum_error))])

#Code for Question3

#set up simulated data
np.random.seed(0)
X = np.random.normal(loc=0.0, scale=1.0, size=(1000, 6))
beta = np.array([[3],[2],[1],[0],[0],[0]])
delta = np.random.normal(loc=0.0, scale=0.1, size=(1000, 1))
Y = np.matmul(X, beta) + delta

#The following two functions(soft_threshold and coordinate_descent_lasso) are referring to https://xavierbourretsicotte.github.io/lasso_implementation.html
def soft_threshold(rho,lamda):
    if rho < - lamda:
        return (rho + lamda)
    elif rho >  lamda:
        return (rho - lamda)
    else: 
        return 0
        
#This function refers to https://xavierbourretsicotte.github.io/lasso_implementation.html
def coordinate_descent_lasso(beta,X,Y,lamda):
    #get the number of variables of X 
    n = X.shape[1]
    iter=30
    #normalizing X 
    X = X / (np.linalg.norm(X,axis = 0)) 
    #Looping for 100 iterations
    for i in range(1000): 
        for j in range(n):
            #implementation the lasso here
            X_j = X[:,j].reshape(-1,1)
            y_pred = X @ beta
            rho = X_j.T @ (Y - y_pred  + beta[j]*X_j)
            beta[j] =  soft_threshold(rho, lamda)/iter
    return beta

n = X.shape[1]
start_beta = np.ones((n,1))
#lamda = 0.1
beta = coordinate_descent_lasso(start_beta,X,Y,lamda = 1)
print(beta)