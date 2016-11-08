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
                   lambda=10^(3:-2), 
                   alpha=0, nfolds=10) # alpha=0 the ridge penalty
  best_lambda <- obj$lambda.min
  mod_ridge <- glmnet(x=t(trn_set), y=y_trn, family="cox", alpha=0,
                      lambda=best_lambda)
  pred_train_ridge <- predict(mod_ridge, t(trn_set))
  pred_test_ridge <-  predict(mod_ridge, t(tst_set))
  
  res <- list(pred_trn=pred_train_ridge, pred_tst=pred_test_ridge)
  return(res)
}



######## LASSO ########
predLasso <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, survival object
){
  library(glmnet)
  obj <- cv.glmnet(x=t(trn_set), y=y_trn, family="cox", 
                   lambda=10^(3:-2), 
                   alpha=1, nfolds=10) # alpha=1 is the lasso penalty
  best_lambda <- obj$lambda.min
  mod_lasso <- glmnet(x=t(trn_set), y=y_trn, family="cox", alpha=1,
                      lambda=best_lambda)
  pred_train_lasso <- predict(mod_lasso, t(trn_set))
  pred_test_lasso <-  predict(mod_lasso, t(tst_set))
  
  res <- list(pred_trn=pred_train_lasso, pred_tst=pred_test_lasso)
  return(res)
}



######## Unicox ########
predUnicox <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, survival object
){
  library(uniCox)
  mod_unicox <- uniCox(x=t(trn_set), y=y_trn[, 1], status=y_trn[, 2])
  obj <- uniCoxCV(mod_unicox, x=t(trn_set), y=y_trn[, 1], status=y_trn[, 2], nfolds=10)
  
  pred_train_unicox <- predict.uniCox(mod_unicox, t(trn_set))[, which(obj$devcvm==max(obj$devcvm))]
  pred_test_unicox <-  predict.uniCox(mod_unicox, t(tst_set))[, which(obj$devcvm==max(obj$devcvm))]
  
  res <- list(pred_trn=pred_train_unicox, pred_tst=pred_test_unicox)
  return(res)
}
  
  
  
######## SuperPC ########
predSuperPC <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn,
  # response of training set, survival object
  y_tst
  # response of test set, survival object
){
  library(superpc)
  superpc_train_data <- list(x=trn_set, y=y_trn[, 1], censoring.status=y_trn[, 2])
  superpc_test_data <- list(x=tst_set, y=y_tst[, 1], censoring.status=y_tst[, 2])
  mod_superpc <- superpc.train(data=superpc_train_data, type="survival")
  obj <- superpc.cv(mod_superpc, data=superpc_train_data, n.components=1)
  thres <- obj$thresholds[which(obj$scor==min(obj$scor))]
  
  obj_train <- superpc.predict(mod_superpc, data=superpc_train_data, 
                               newdata=superpc_train_data, threshold=thres, n.components=1)
  pred_train_superpc <- obj_train$v.pred[, 1]
  obj_test <- superpc.predict(mod_superpc, data=superpc_train_data, 
                              newdata=superpc_test_data, threshold=thres, n.components=1)
  pred_test_superpc <- obj_test$v.pred[, 1]
  
  res <- list(pred_trn=pred_train_superpc, pred_tst=pred_test_superpc)
  return(res)
}



######## CoxBoost ########
predCoxBoost <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, survival object
){
  library(CoxBoost)
  # obj <- cv.CoxBoost(time=y_trn[, 1], status=y_trn[, 2], 
  #                    x=t(trn_set), maxstepno=200, K=10)
  mod_coxboost <- CoxBoost(time=y_trn[, 1], status=y_trn[, 2], x=t(trn_set), 
                           stepno=100, standardize=FALSE)
  beta <- coef(mod_coxboost)
  pred_train_coxboost <- t(trn_set) %*% beta
  pred_test_coxboost <-  t(tst_set) %*% beta
  
  res <- list(pred_trn=pred_train_coxboost, pred_tst=pred_test_coxboost)
  return(res)
}
