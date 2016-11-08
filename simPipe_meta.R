rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")

# load("tests/command_args.RData")
# command_args=command_args2
# rm(command_args1, command_args2, command_args3, command_args4, command_args5)

source("scripts/modStats_new.R")
source("scripts/baseDat.R")
source("scripts/batchDat.R")
source("scripts/adjBatch.R")
source("scripts/prediction_mods.R")
source("scripts/helper.R")
source("scripts/metaPred.R")


######################## Set Parameters ########################
command_args <- commandArgs(trailingOnly=TRUE)
if (length(command_args) < 16){print("ERROR: At least 16 parameters!"); quit(save = "no", status = 1, runLast = FALSE)}


#### Baseline Datasets Parameters ####

# number of genes
n_genes <- as.numeric(command_args[1]) # 600 # numberof genes
n_on <- as.numeric(command_args[2]) # 100 # number of biomarkers

# gene distribution
on_mean <- as.numeric(command_args[3]) # 7.5 # mean expression when genes are "on"
off_mean <- as.numeric(command_args[4]) # 7 # mean expression when genes are "off"
on_var <- as.numeric(command_args[5]) # 1.8 # variance expression when genes are "on"
off_var <- as.numeric(command_args[6]) # 1.8 # variance expression when genes are "on"

# number of samples
n_sample_train <- as.numeric(command_args[7]) # 200 # number of samples in the training set
n_case_train <- as.numeric(command_args[8]) 
n_control_train <- n_sample_train - n_case_train
n_sample_test <- as.numeric(command_args[9]) # 200 # number of samples in the test set
n_case_test <- as.numeric(command_args[10])
n_control_test <- n_sample_test - n_case_test


#### Batch Parameters ####
# add batch effect to baseline data? (T/F)
withBatch <- as.logical(command_args[11]) # TRUE/FALSE
# mean or variance difference
batch_meanvar_arg <- command_args[12] # mean/var
# batch adjustment 
sep_cmb <- command_args[13] # separate/combined
combat_mod <- command_args[14] # mod/null

# number of batches in the training and test set
n_batch_train <- as.numeric(command_args[15]) # number of batches in the training set
n_batch_test <- as.numeric(command_args[16]) # number of batches in the test set
if(n_batch_train==1 || n_batch_test==1){print("ERROR: At least 2 batches within each dataset, if not 0!"); quit(save = "no", status = 1, runLast = FALSE)}

if (length(command_args) != 16+2*(n_batch_train+n_batch_test)){
  print("ERROR: Input the right parameters!")
  #print("Usage: ")
  quit(save = "no", status = 1, runLast = FALSE)
}
# number of cases and controls in each batch
cases_batch_train <- controls_batch_train <- rep(0, n_batch_train)
cases_batch_test <- controls_batch_test <- rep(0, n_batch_test)
if(length(command_args) > 17){
  new_args <- command_args[17:length(command_args)]
  if(n_batch_train > 0){
    case_tmp_id <- seq(from=1, to=2*n_batch_train, by=2)
    cases_batch_train <- as.numeric(new_args[case_tmp_id])
    controls_batch_train <- as.numeric(new_args[case_tmp_id+rep(1, n_batch_train)])
    rm(case_tmp_id)
  }
  if(n_batch_test > 0){
    case_tmp_id <- seq(from=(2*n_batch_train+1), to=length(new_args), by=2)
    cases_batch_test <- as.numeric(new_args[case_tmp_id])
    controls_batch_test <- as.numeric(new_args[case_tmp_id+rep(1, n_batch_test)])
  }
}
# check # of samples in batches with # in training/test set
if(n_batch_train > 0 & (sum(cases_batch_train)!=n_case_train)){print("ERROR: Cases in training set doesn't match!"); quit(save = "no", status = 1, runLast = FALSE)}
if(n_batch_train > 0 & (sum(controls_batch_train)!=n_control_train)){print("ERROR: Controls in training set doesn't match!"); quit(save = "no", status = 1, runLast = FALSE)}
if(n_batch_test > 0 & (sum(cases_batch_test)!=n_case_test)){print("ERROR: Cases in test set doesn't match!"); quit(save = "no", status = 1, runLast = FALSE)}
if(n_batch_test > 0 & (sum(controls_batch_test)!=n_control_test)){print("ERROR: Controls in test set doesn't match!"); quit(save = "no", status = 1, runLast = FALSE)}  


#### Pipeline Parameters ####
iterations <- 20
set.seed(1)



######################## Run pipeline ########################
res_mat_trn <- res_mat_tst <- list()
for(iter in 1:iterations){
  print(paste("SIMULATION:",iter,sep=""))
  
  #### Step 1: simulate baseline datasets ####
  baseLst <- baseDat(n_trn=n_sample_train, n_trn_case=n_case_train,   
                     n_tst=n_sample_test, n_tst_case=n_case_test,
                     n_genes=n_genes, n_biomarker=n_on, 
                     on_mean=on_mean, off_mean=off_mean, on_var=on_var, off_var=off_var)
  
  
  #### Step 2: add batch effect ####
  # simulate batch matrices
  batchLst <- batchDat(n_batch_train=n_batch_train, n_batch_test=n_batch_test,
                       cases_batch_train=cases_batch_train, controls_batch_train=controls_batch_train,
                       cases_batch_test=cases_batch_test, controls_batch_test=controls_batch_test,
                       batch_meanvar_arg=batch_meanvar_arg, 
                       n_genes=n_genes)
  # add batch matrices to baseline data
  baseBatch_Lst <- baseLst
  if(n_batch_train==0 & n_batch_test==0 & withBatch){
    print("No batch specified. setting withBatch = FALSE.")
    withBatch=FALSE
  }
  if(withBatch){
    if(n_batch_train > 0){
      for(i in 1:n_batch_train){
        baseBatch_Lst$trn_x[, batchLst$batch_ind_trn[[i]]] <- baseLst$trn_x[, batchLst$batch_ind_trn[[i]]] + batchLst$batchMat[[i]]
      } 
    }
    if(n_batch_test > 0){
      for(j in (n_batch_train+1):(n_batch_train+n_batch_test)){
        baseBatch_Lst$tst_x[, batchLst$batch_ind_tst[[j-n_batch_train]]] <- baseLst$tst_x[, batchLst$batch_ind_tst[[j-n_batch_train]]] + batchLst$batchMat[[j]]
      }
    }
  }
  
  
  #### Step 3: Batch adjustment (Optional) and Partition training set by batch ####
  if((!is.null(batchLst$batch_train)) & (!is.null(batchLst$batch_test))){
    batchLst$batch_test <- batchLst$batch_test + rep(max(batchLst$batch_train), length(batchLst$batch_test))
    names(batchLst$batch_ind_tst) <- as.numeric(names(batchLst$batch_ind_tst)) + rep(max(batchLst$batch_train), length(names(batchLst$batch_ind_tst)))
  }
  combatLst <- adjBatch(datLst=baseBatch_Lst,
                        batch_train=batchLst$batch_train, batch_test=batchLst$batch_test,
                        sep_cmb=sep_cmb, combat_mod=combat_mod)
  
  
  #### Step 4: Preparation before training and validation ####
  trn_set_lst <-  y_trn_lst <- list()
  ## Partition on train
  trn_set_lst[[1]] <- combatLst$trn_x
  y_trn_lst[[1]] <- combatLst$trn_y
  names(trn_set_lst) <- names(y_trn_lst) <- "wholeset"
  if(n_batch_train > 0){
    for(i in 1:n_batch_train){
      trn_set_lst[[i+1]] <- combatLst$trn_x[, batchLst$batch_ind_trn[[i]]]
      y_trn_lst[[i+1]] <- combatLst$trn_y[batchLst$batch_ind_trn[[i]]]
    }
    names(trn_set_lst) <- names(y_trn_lst) <- c("wholeset", paste("Batch", 1:n_batch_train, sep=""))
  }
  ## test
  tst_set <- combatLst$tst_x
  y_tst <- combatLst$tst_y
  
  # normalization by gene (Optional)
  # trn_set <- t(scale(t(trn_set), center=TRUE, scale=TRUE))
  # tst_set <- t(scale(t(tst_set), center=TRUE, scale=TRUE))
  
  # swith to data.frame
  training_df_lst <- list()
  for(i in 1:length(trn_set_lst)){
    training_df_lst[[i]] <- data.frame(t(trn_set_lst[[i]]), as.factor(y_trn_lst[[i]]))
    colnames(training_df_lst[[i]]) <- c(paste("gene", 1:nrow(trn_set_lst[[i]]), sep=""), "response")
    rownames(training_df_lst[[i]]) <- 1:ncol(trn_set_lst[[i]])
  }
  
  test_df <- data.frame(t(tst_set), as.factor(y_tst))
  colnames(test_df) <- c(paste("gene", 1:nrow(tst_set), sep=""), "response")
  rownames(test_df) <- 1:ncol(tst_set)
  
  
  #### Step 5: Predictive Models ####
  # LASSO
  res_lasso <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                        tst_set=tst_set, y_tst=y_tst,
                        func_name=predLasso, n_batch_train=n_batch_train)

  # Elastic Net
  res_elnet <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                        tst_set=tst_set, y_tst=y_tst,
                        func_name=predElnet, n_batch_train=n_batch_train)
  
  # Naive Bayes
  res_nb <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                     tst_set=tst_set, y_tst=y_tst,
                     func_name=predNB, n_batch_train=n_batch_train)
  
  # SVM
  res_svm <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                      tst_set=tst_set, y_tst=y_tst,
                      func_name=predSVM, n_batch_train=n_batch_train)
  
  # kNN
  res_knn <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                      tst_set=tst_set, y_tst=y_tst,
                      func_name=predKNN, n_batch_train=n_batch_train)
  
  # Random Forest
  res_rf <- metaPred(trn_set_lst=training_df_lst, y_trn_lst=y_trn_lst,
                     tst_set=test_df, y_tst=y_tst,
                     func_name=predRF, n_batch_train=n_batch_train)
  
  # Neural Network
  res_nnet <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                       tst_set=tst_set, y_tst=y_tst,
                       func_name=predNnet, n_batch_train=n_batch_train)
  
  # Mas-o-menos 
  res_mas <- metaPred(trn_set_lst=trn_set_lst, y_trn_lst=y_trn_lst,
                      tst_set=tst_set, y_tst=y_tst,
                      func_name=predMas, n_batch_train=n_batch_train)
  
  # Majority Voting
  # test
  pred_test_vote <- pred_train_vote <- stats_vote_test <- stats_vote_train <- list()
  for(i in 1:(n_batch_train+1)){
    test_mat <- cbind(res_lasso$predres[[i]]$pred_tst, res_elnet$predres[[i]]$pred_tst,
                      res_nb$predres[[i]]$pred_tst, res_svm$predres[[i]]$pred_tst,
                      res_knn$predres[[i]]$pred_tst, res_rf$predres[[i]]$pred_tst,
                      res_nnet$predres[[i]]$pred_tst, res_mas$predres[[i]]$pred_tst)
    weight_seq <- c(res_lasso$stats_test[[i]]$AUC, res_elnet$stats_test[[i]]$AUC,
                    res_nb$stats_test[[i]]$AUC, res_svm$stats_test[[i]]$AUC,
                    res_knn$stats_test[[i]]$AUC, res_rf$stats_test[[i]]$AUC,
                    res_nnet$stats_test[[i]]$AUC, res_mas$stats_test[[i]]$AUC)
    weight_seq <- weight_seq / sum(weight_seq)
    pred_test_vote[[i]] <- test_mat %*% weight_seq
    stats_vote_test[[i]] <- computeStats(pred_vec=pred_test_vote[[i]], true_label=y_tst, cutoff=0.5)
  }
  # train
  for(i in 1:(n_batch_train+1)){
    train_mat <- cbind(res_lasso$predres[[i]]$pred_trn, res_elnet$predres[[i]]$pred_trn,
                      res_nb$predres[[i]]$pred_trn, res_svm$predres[[i]]$pred_trn,
                      res_knn$predres[[i]]$pred_trn, res_rf$predres[[i]]$pred_trn,
                      res_nnet$predres[[i]]$pred_trn, res_mas$predres[[i]]$pred_trn)
    weight_seq <- c(res_lasso$stats_train[[i]]$AUC, res_elnet$stats_train[[i]]$AUC,
                    res_nb$stats_train[[i]]$AUC, res_svm$stats_train[[i]]$AUC,
                    res_knn$stats_train[[i]]$AUC, res_rf$stats_train[[i]]$AUC,
                    res_nnet$stats_train[[i]]$AUC, res_mas$stats_train[[i]]$AUC)
    weight_seq <- weight_seq / sum(weight_seq)
    pred_train_vote[[i]] <- train_mat %*% weight_seq
    stats_vote_train[[i]] <- computeStats(pred_vec=pred_train_vote[[i]], true_label=y_trn_lst[[i]], cutoff=0.5)
  }
  predres_vote <- list()
  for(i in 1:length(pred_test_vote)){
    predres_vote[[i]] <- list(pred_trn=pred_train_vote[[i]], pred_tst=pred_test_vote[[i]])
  }
  names(predres_vote) <- names(stats_vote_test) <- names(stats_vote_train) <- names(trn_set_lst)
  
  meta_pred_test_vote <- c(); meta_stat_vote <- list()
  if(n_batch_train > 0){
    tmp=do.call(cbind, pred_test_vote[2:length(pred_test_vote)])
    meta_pred_test_vote <- apply(tmp, 1, mean)
    meta_stat_vote <- computeStats(pred_vec=meta_pred_test_vote, true_label=y_tst, cutoff=0.5)
  }
  res_vote <- list(predres=predres_vote,
                   stats_train=stats_vote_train, stats_test=stats_vote_test,
                   meta_pred_test=meta_pred_test_vote,
                   meta_stat=meta_stat_vote)
}

samples_batch_train <- conCat(cases_batch_train, controls_batch_train)
samples_batch_test <- conCat(cases_batch_test, controls_batch_test)
filename_seq <- c(withBatch, batch_meanvar_arg, sep_cmb, combat_mod,
                  n_sample_train, n_case_train, n_sample_test, n_case_test,
                  n_batch_train, n_batch_test,
                  samples_batch_train, samples_batch_test)
save(res_lasso, res_elnet, res_nb, res_svm, 
     res_knn, res_rf, res_nnet, res_mas, res_vote,
     batchLst, combatLst, 
     file=paste("results_meta/", paste(filename_seq, collapse= "_"), ".RData", sep=""))