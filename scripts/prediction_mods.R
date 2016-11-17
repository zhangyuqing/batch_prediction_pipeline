######## LASSO ########
predLasso <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, binary & numeric
){
  library(glmnet)
  obj <- cv.glmnet(x=t(trn_set), y=y_trn, family="binomial", 
                   lambda=exp(seq(from=-10, to=10, by=1)), alpha=1,
                   type.measure="class", nfolds=10)
  best_lambda <- obj$lambda.min
  mod_logit <- glmnet(x=t(trn_set),y=y_trn, family="binomial", alpha=1,
                      lambda=best_lambda)
  pred_train_logit <- predict(mod_logit, t(trn_set), type="response")
  pred_test_logit <-  predict(mod_logit, t(tst_set), type="response")
  # Type "response" gives the fitted probabilities for "binomial"
  res <- list(pred_trn=pred_train_logit, pred_tst=pred_test_logit)
  return(res)
}



######## Elastic net ########
predElnet <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn 
  # response of training set, binary & numeric
){
  library(caret)
  parGrid <- expand.grid(lambda=exp(seq(from=-10, to=10, by=1)),
                         alpha=seq(from=0, to=1, by=0.1))
  ctrl <- trainControl(method = "cv", number=10)
  mod_elnet <- train(x=t(trn_set), y=as.factor(y_trn), family = "binomial",
                     method="glmnet",
                     trControl=ctrl,
                     tuneGrid=parGrid)
  pred_train_elnet <- predict(mod_elnet, t(trn_set), type="prob")[,"1"] 
  pred_test_elnet <- predict(mod_elnet, t(tst_set), type="prob")[,"1"] 
  # either "raw" or "prob", for the number/class predictions or class probabilities
  res <- list(pred_trn=pred_train_elnet, pred_tst=pred_test_elnet)
  return(res)
}



######## Naive Bayes ########
predNB <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, binary & numeric
){
  library(e1071)
  mod_nb <- naiveBayes(x=t(trn_set), y=as.factor(y_trn))
  pred_train_nb <- predict(mod_nb, t(trn_set), type="raw")[,"1"]
  pred_test_nb <- predict(mod_nb, t(tst_set), type="raw")[,"1"]
  # If "raw", the conditional a-posterior probabilities for each class are returned
  res <- list(pred_trn=pred_train_nb, pred_tst=pred_test_nb)
  return(res)
}



######## SVM ########
predSVM <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, binary & numeric
){
  library(e1071)
  tune_ctrl <- tune.control(sampling="cross", cross=10)
  obj <- tune(svm, train.x=t(trn_set), train.y=as.factor(y_trn),
              tunecontrol=tune_ctrl,
              ranges=list(type="C-classification",
                          kernel="linear",
                          cost=exp(seq(from=-10, to=10, by=1))))
  best_cost <- obj$best.parameters[,"cost"]
  mod_svm <- svm(x=t(trn_set), y=as.factor(y_trn),
                 type="C-classification", kernel="linear",
                 cost=best_cost, probability=TRUE)
  pred_train_svm <- predict(mod_svm, t(trn_set), probability=TRUE)
  pred_train_svm <- attr(pred_train_svm, "probabilities")[,"1"]
  pred_test_svm <- predict(mod_svm, t(tst_set), probability=TRUE)
  pred_test_svm <- attr(pred_test_svm, "probabilities")[,"1"]
  
  res <- list(pred_trn=pred_train_svm, pred_tst=pred_test_svm)
  return(res)
}



######## kNN ########
# predKNN <- function(
#   training_df,
#   # data.frame of training set with response in it
#   test_df
#   # data.frame of test set with response in it
# ){
#   library(kknn)
#   k_seq <- seq(from=1,to=150,by=1)
#   f <- as.formula(paste("response ~ ", paste(colnames(training_df)[-ncol(training_df)], collapse= "+",sep="")))
#   obj <- train.kknn(formula=f, data=training_df, ks=k_seq,
#                     kernel="rectangular")
#   best_k <- obj$best.parameters$k
#   mod_knn_trn <- kknn(formula=f, train=training_df, test=training_df, 
#                       k=best_k, kernel="rectangular")
#   pred_train_knn <- mod_knn_trn$prob[,"1"]
#   mod_knn <- kknn(formula=f, train=training_df, test=test_df, 
#                   k=best_k, kernel="rectangular")
#   pred_test_knn <- mod_knn$prob[,"1"]
#   
#   res <- list(pred_trn=pred_train_knn, pred_tst=pred_test_knn)
#   return(res)
# }

predKNN <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, binary & numeric
){
  library(caret)
  parGrid <- expand.grid(k=seq(from=1,to=50,by=1))
  ctrl <- trainControl(method = "cv", number=10)
  mod_knn <- train(x=t(trn_set), y=as.factor(y_trn), 
                     method="knn",
                     trControl=ctrl,
                     tuneGrid=parGrid)
  pred_train_knn <- predict(mod_knn, t(trn_set), type="prob")[,"1"] 
  pred_test_knn <- predict(mod_knn, t(tst_set), type="prob")[,"1"] 
  
  res <- list(pred_trn=pred_train_knn, pred_tst=pred_test_knn)
  return(res)
}


######## Random Forest ########  
predRF <- function(
  training_df,
  # data.frame of training set with response in it
  test_df
  # data.frame of test set with response in it
){
  library(caret)
  ctrl <- trainControl(method="cv", number=10)
  f <- as.formula(paste("response ~ ", paste(colnames(training_df)[-ncol(training_df)], collapse= "+",sep="")))
  mod_rf <- train(form=f, data=training_df, 
                  method="rf", metric="Accuracy", 
                  tuneLength=15,
                  trControl=ctrl)
  pred_train_rf <- predict(mod_rf, training_df, type="prob")[,"1"]
  pred_test_rf <- predict(mod_rf, test_df, type="prob")[,"1"]
  
  res <- list(pred_trn=pred_train_rf, pred_tst=pred_test_rf)
  return(res)
}



######## Neural Net ########  
predNnet <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, binary & numeric
){
  library(caret)
  parGrid <- expand.grid(size=seq(from=2, to=3, by=1),
                         decay=10^seq(from=-2, to=-1, by=1))
  ctrl <- trainControl(method = "cv", number=4)
  mod_nnet <- train(x=t(trn_set), y=as.factor(y_trn), maxit=1000, MaxNWts = 50000,
                    method="nnet", trace=FALSE,
                    trControl=ctrl,
                    tuneGrid=parGrid)
  pred_train_nnet <- predict(mod_nnet, t(trn_set), type="prob")[,"1"]
  pred_test_nnet <- predict(mod_nnet, t(tst_set), type="prob")[,"1"] 
  
  res <- list(pred_trn=pred_train_nnet, pred_tst=pred_test_nnet)
  return(res)
}



######## Mas-o-menos ########  
predMas <- function(
  trn_set,
  # gene-by-sample expression matrix for training
  tst_set, 
  # gene-by-sample expression matrix for test
  y_trn
  # response of training set, binary & numeric
){
  trn_set_norm <- t(scale(t(trn_set), center=TRUE, scale=TRUE))
  tst_set_norm <- t(scale(t(tst_set), center=TRUE, scale=TRUE))
  
  training_df_norm <- data.frame(t(trn_set_norm), as.factor(y_trn))
  colnames(training_df_norm) <- c(paste("gene", 1:nrow(trn_set_norm), sep=""), "response")
  rownames(training_df_norm) <- 1:ncol(trn_set_norm)
  test_df_norm <- data.frame(t(tst_set_norm), as.factor(y_tst))
  colnames(test_df_norm) <- c(paste("gene", 1:nrow(tst_set_norm), sep=""), "response")
  rownames(test_df_norm) <- 1:ncol(tst_set_norm)
  
  alpha <- rep(0, nrow(trn_set_norm))
  for(j in 1:nrow(trn_set_norm)){
    f <- as.formula(paste("response ~ 0 +", paste(colnames(training_df_norm)[j], 
                                                  collapse= "+",sep="")))
    ctr <- glm.control(maxit=1000)
    mod_tmp <- glm(f, data=training_df_norm, family=binomial, control=ctr)
    alpha[j] <- coef(mod_tmp)
  }
  v <- (2*(alpha>0)-1)/sqrt(nrow(trn_set))
  
  pred_train_plusminus <- t(trn_set_norm) %*% as.matrix(v)
  pred_train_plusminus <- 1/(1+exp(- pred_train_plusminus))
  pred_test_plusminus <- t(tst_set_norm) %*% as.matrix(v)
  pred_test_plusminus <- 1/(1+exp(- pred_test_plusminus))
  
  res <- list(pred_trn=pred_train_plusminus, pred_tst=pred_test_plusminus)
  return(res)
}



######## kTSP (k top scoring pairs) ########  
# predkTSP <- function(){
#   library(switchBox)
#   
# }