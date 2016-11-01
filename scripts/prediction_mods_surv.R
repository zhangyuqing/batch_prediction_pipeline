######## Mas-o-menos ########
predMas <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, survival object
){
  library(simulatorZ)
  beta <- plusMinus(t(trn_set), y_trn)
  lp_train <- t(trn_set) %*% beta
  lp_test <- t(tst_set) %*% beta
  
  res <- list(pred_trn=lp_train, pred_tst=lp_test)
  return(res)
}



######## Ridge regression ########
predRidge <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, survival object
){
  library(glmnet)
  obj <- cv.glmnet(x=t(trn_set), y=y_trn, family="cox", 
                   lambda=exp(seq(from=-10, to=10, by=1)), 
                   alpha=0, nfolds=10)
  best_lambda <- obj$lambda.min
  mod_ridge <- glmnet(x=t(trn_set), y=y_trn, family="cox", alpha=0,
                      lambda=best_lambda)
  pred_train_ridge <- predict(mod_ridge, t(trn_set))
  pred_test_ridge <-  predict(mod_ridge, t(tst_set))
  
  res <- list(pred_trn=pred_train_ridge, pred_tst=pred_test_ridge)
  return(res)
}