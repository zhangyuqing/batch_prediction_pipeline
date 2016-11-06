metaPred <- function(
  trn_set_lst, 
  # list of training set separated by batch: whole set, batch 1, batch 2, batch 3,...
  y_trn_lst, 
  # list of training response, separated by batch
  tst_set, 
  # test set
  y_tst,
  # response of test
  func_name,
  # function to use for prediction
  n_batch_train
  # number of batches in the training set
){
  #### train model on the whole set and each batch in train set, then predict on the test set
  predres <- stats_train <- stats_test <- list()
  for(i in 1:length(trn_set_lst)){
    if(class(trn_set_lst[[1]]) == "matrix"){
      predres[[i]] <- func_name(trn_set=trn_set_lst[[i]], tst_set=tst_set, y_trn=y_trn_lst[[i]])
    }else if(class(trn_set_lst[[1]]) == "data.frame"){
      predres[[i]] <- func_name(training_df=training_df_lst[[i]], test_df=test_df)
    }
  }
  names(predres) <- names(trn_set_lst)
  
  
  #### compute performance statistics
  for(i in 1:length(predres)){
    stats_train[[i]] <- computeStats(pred_vec=predres[[i]]$pred_trn, true_label=y_trn_lst[[i]], cutoff=0.5)
    stats_test[[i]] <- computeStats(pred_vec=predres[[i]]$pred_tst, true_label=y_tst, cutoff=0.5)
  }
  names(stats_train) <- names(stats_test) <- names(trn_set_lst)
  
  
  #### Integrate prediction from models in each batch (simple average of predicted probabilities)
  meta_pred_test <- c(); meta_stat <- list()
  if(n_batch_train > 0){
    pred_tst_lst <- lapply(predres[2:length(predres)], function(pred){
      return(pred$pred_tst)
    })
    tmp <- do.call(cbind, pred_tst_lst)
    meta_pred_test <- apply(tmp, 1, mean)
    meta_stat <- computeStats(pred_vec=meta_pred_test, true_label=y_tst, cutoff=0.5)
  }
  
  res <- list(predres=predres, 
              stats_train=stats_train, stats_test=stats_test,
              meta_pred_test=meta_pred_test, meta_stat=meta_stat)
  return(res)
}