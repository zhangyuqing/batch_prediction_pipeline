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
#combatSets <- list()
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
  
  
  #### Step 3: Adjust batch with ComBat ####
  if((!is.null(batchLst$batch_train)) & (!is.null(batchLst$batch_test))){
    batchLst$batch_test <- batchLst$batch_test + rep(max(batchLst$batch_train), length(batchLst$batch_test))
    names(batchLst$batch_ind_tst) <- as.numeric(names(batchLst$batch_ind_tst)) + rep(max(batchLst$batch_train), length(names(batchLst$batch_ind_tst)))
  }
  combatLst <- adjBatch(datLst=baseBatch_Lst, 
                        batch_train=batchLst$batch_train, batch_test=batchLst$batch_test, 
                        sep_cmb=sep_cmb, combat_mod=combat_mod)
  
  
  #### Step 4: Preparation before training and validation ####
  trn_set <- combatLst$trn_x
  y_trn <- combatLst$trn_y
  tst_set <- combatLst$tst_x
  y_tst <- combatLst$tst_y
  
  # normalization by gene (Optional)
  # trn_set <- t(scale(t(trn_set), center=TRUE, scale=TRUE))
  # tst_set <- t(scale(t(tst_set), center=TRUE, scale=TRUE))
  
  # shuffle individuals to mix up cases and controls #
  ind_trn <- sample(1:ncol(trn_set), ncol(trn_set), replace=FALSE)
  ind_tst <- sample(1:ncol(tst_set), ncol(tst_set), replace=FALSE)
  trn_set <- trn_set[, ind_trn]
  y_trn <- y_trn[ind_trn]
  tst_set <- tst_set[, ind_tst]
  y_tst <- y_tst[ind_tst]
  # swith to data.frame
  training_df <- data.frame(t(trn_set), as.factor(y_trn))
  colnames(training_df) <- c(paste("gene", 1:nrow(trn_set), sep=""), "response")
  rownames(training_df) <- 1:ncol(trn_set)
  test_df <- data.frame(t(tst_set), as.factor(y_tst))
  colnames(test_df) <- c(paste("gene", 1:nrow(tst_set), sep=""), "response")
  rownames(test_df) <- 1:ncol(tst_set)
  
  
  #### Step 5: Predictive Models ####
  # LASSO
  res_lasso <- predLasso(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn) 
  stats_lasso_train <- computeStats(pred_vec=res_lasso$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_lasso_test <- computeStats(pred_vec=res_lasso$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # Elastic Net
  res_elnet <- predElnet(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn) 
  stats_elnet_train <- computeStats(pred_vec=res_elnet$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_elnet_test <- computeStats(pred_vec=res_elnet$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # Naive Bayes
  res_nb <- predNB(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn) 
  stats_nb_train <- computeStats(pred_vec=res_nb$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_nb_test <- computeStats(pred_vec=res_nb$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # SVM
  res_svm <- predSVM(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn) 
  stats_svm_train <- computeStats(pred_vec=res_svm$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_svm_test <- computeStats(pred_vec=res_svm$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # kNN
  res_knn <- predKNN(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn) 
  stats_knn_train <- computeStats(pred_vec=res_knn$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_knn_test <- computeStats(pred_vec=res_knn$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # Random Forest
  res_rf <- predRF(training_df=training_df, test_df=test_df) 
  stats_rf_train <- computeStats(pred_vec=res_rf$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_rf_test <- computeStats(pred_vec=res_rf$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # Neural Network
  res_nnet <- predNnet(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn)
  stats_nnet_train <- computeStats(pred_vec=res_nnet$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_nnet_test <- computeStats(pred_vec=res_nnet$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # Mas-o-menos 
  res_mas <- predMas(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn)
  stats_mas_train <- computeStats(pred_vec=res_mas$pred_trn, true_label=y_trn, cutoff=0.5)
  stats_mas_test <- computeStats(pred_vec=res_mas$pred_tst, true_label=y_tst, cutoff=0.5)
  
  # Majority Voting
  # test
  test_mat <- rbind(unlist(stats_lasso_test), unlist(stats_elnet_test),
                    unlist(stats_nb_test), unlist(stats_svm_test),
                    unlist(stats_knn_test), unlist(stats_rf_test),
                    unlist(stats_nnet_test), unlist(stats_mas_test))
  rownames(test_mat) <- c("LASSO", "Elastic Net",
                         "Naive Bayes", "SVM",
                         "kNN", "Random Forest",
                         "Neural Nets","Mas-o-menos")
  weight_seq <- test_mat[,"accuracy"]
  weight_seq <- weight_seq / sum(weight_seq)
  pred_test_mat <- cbind(res_lasso$pred_tst, res_elnet$pred_tst,
                         res_nb$pred_tst, res_svm$pred_tst, 
                         res_knn$pred_tst, res_rf$pred_tst,
                         res_nnet$pred_tst, res_mas$pred_tst)
  pred_test_vote <- pred_test_mat%*%weight_seq
  stats_vote_test <- computeStats(pred_vec=pred_test_vote, true_label=y_tst, cutoff=0.5)
  # train
  train_mat <- rbind(unlist(stats_lasso_train), unlist(stats_elnet_train),
                     unlist(stats_nb_train), unlist(stats_svm_train),
                     unlist(stats_knn_train), unlist(stats_rf_train),
                     unlist(stats_nnet_train), unlist(stats_mas_train))
  rownames(train_mat) <- c("LASSO", "Elastic Net",
                           "Naive Bayes", "SVM",
                           "kNN", "Random Forest",
                           "Neural Nets","Mas-o-menos")
  weight_seq <- train_mat[,"accuracy"]
  weight_seq <- weight_seq / sum(weight_seq)
  pred_train_mat <- cbind(res_lasso$pred_trn, res_elnet$pred_trn,
                          res_nb$pred_trn, res_svm$pred_trn, 
                          res_knn$pred_trn, res_rf$pred_trn,
                          res_nnet$pred_trn, res_mas$pred_trn)
  pred_train_vote <- pred_train_mat%*%weight_seq
  stats_vote_train <- computeStats(pred_vec=pred_train_vote, true_label=y_trn, cutoff=0.5)
  
  
  #### Step 6: summarize results ####
  ## Summarization ##
  res_mat_tst[[iter]] <- rbind(unlist(stats_lasso_test), unlist(stats_elnet_test),
                               unlist(stats_nb_test), unlist(stats_svm_test),
                               unlist(stats_knn_test), unlist(stats_rf_test),
                               unlist(stats_nnet_test), unlist(stats_mas_test),
                               unlist(stats_vote_test))
  res_mat_trn[[iter]] <- rbind(unlist(stats_lasso_train), unlist(stats_elnet_train),
                               unlist(stats_nb_train), unlist(stats_svm_train),
                               unlist(stats_knn_train), unlist(stats_rf_train),
                               unlist(stats_nnet_train), unlist(stats_mas_train),
                               unlist(stats_vote_train))
  rownames(res_mat_tst[[iter]]) <- rownames(res_mat_trn[[iter]]) <- c("LASSO", "Elastic Net",
                                                                      "Naive Bayes", "SVM",
                                                                      "kNN", "Random Forest",
                                                                      "Neural Nets", "Mas-o-menos",
                                                                      "Weighted Majority")
  # baseSets[[iter]] <- baseLst
  # baseBatchSets[[iter]] <- baseBatch_Lst
  # combatSets[[iter]] <- combatLst
}

samples_batch_train <- conCat(cases_batch_train, controls_batch_train)
samples_batch_test <- conCat(cases_batch_test, controls_batch_test)
filename_seq <- c(withBatch, batch_meanvar_arg, sep_cmb, combat_mod,
                  n_sample_train, n_case_train, n_sample_test, n_case_test,
                  n_batch_train, n_batch_test,
                  samples_batch_train, samples_batch_test)
save(res_mat_tst, res_mat_trn,
     #baseSets, baseBatchSets, combatSets, 
     batchLst, combatLst, 
     file=paste("results/", paste(filename_seq, collapse= "_"), ".RData", sep=""))