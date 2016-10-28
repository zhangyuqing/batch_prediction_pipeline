rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/12_refine_pipeline/")
setwd("~/simPipe_v2/")

source("scripts/modStats_new.R")
source("scripts/baseDat.R")
source("scripts/batchDat.R")
source("scripts/adjBatch.R")
source("scripts/prediction_mods.R")


############ Read in parameters ###########
command_args <- commandArgs(trailingOnly=TRUE)
if (length(command_args) != 12){
  print("ERROR: Submit the correct options!")
  print("Rscript 
        <add batch effect to baseline (TRUE/FALSE)> <difference between batch (mean/var)> 
        <batch adjust: separate/combined/none> <ComBat mod: mod/null/none> 
        <train size> <test size> 
        <10x percentage of cases in train> < 10x percentage of cases in test>
        <10x percentage of case in batch 1> < 10x percentage of control in batch 1> 
        <10x percentage of case in batch 3> < 10x percentage of control in batch 3>")
  quit(save = "no", status = 1, runLast = FALSE)
}



############ Set Parameters ############

#### Baseline Datasets Parameters ####
# gene distribution
on_mean <- 7.5 # mean expression when genes are "on"
off_mean <- 7 # mean expression when genes are "off"
on_var <- 1.8 # variance expression when genes are "on"
off_var <- 1.8 # variance expression when genes are "on"
# number of samples
n_sample_trn <- as.numeric(command_args[5]) # number of samples in the training set
n_sample_tst <- as.numeric(command_args[6]) # number of samples in the test set
p_trn_case <- as.numeric(command_args[7])/10
p_tst_case <- as.numeric(command_args[8])/10
# number of genes
n_genes <- 600 # numberof genes
n_on <- 100 # number of biomarkers

#### Batch Parameters ####
# balanced/unbalanced design
p_batch1_case <- as.numeric(command_args[9])/10  #batch 1 has p_batch1_case/100 percent of the train cases
p_batch1_control <- as.numeric(command_args[10])/10 #batch 1 has p_batch1_control/100 percent of the train controls
p_batch3_case <- as.numeric(command_args[11])/10 #batch 3 has p_batch3_case/100 percent of the test cases
p_batch3_control <- as.numeric(command_args[12])/10 #batch 3 has p_batch3_control/100 percent of the test controls
# the sum of pcase and pcontrol close to 1 => size balance, otherwise not balanced
withBatch <- as.logical(command_args[1]) # add batch effect to baseline data?
## mean or variance difference
batch_meanvar_arg <- command_args[2]

#### Batch Adjustment Parameters ####
sep_cmb <- command_args[3]
combat_mod <- command_args[4]

#### Pipeline Parameters ####
iterations <- 100
set.seed(1)



############ Run pipeline ############
res_mat_trn <- res_mat_tst <- list()
combatSets <- list()
for(iter in 1:iterations){
  print(paste("SIMULATION:",iter,sep=""))
  
  #### Step 1: simulate baseline datasets ####
  baseLst <- baseDat(n_trn=n_sample_trn, pcase_trn=p_trn_case,   
                     n_tst=n_sample_tst, pcase_tst=p_tst_case,
                     n_genes=n_genes, n_biomarker=n_on, 
                     on_mean=on_mean, off_mean=off_mean, on_var=on_var, off_var=off_var)
  
  
  #### Step 2: add batch effect ####
  # simulate batch matrices
  batchLst <- batchDat(ncase_trn=baseLst$n_trn_case, nctrl_trn=baseLst$n_trn_control,
                       ncase_tst=baseLst$n_tst_case, nctrl_tst=baseLst$n_tst_control,
                       pcase_batch1=p_batch1_case, pctrl_batch1=p_batch1_control, 
                       pcase_batch3=p_batch3_case, pctrl_batch3=p_batch3_control,
                       batch_meanvar_arg=batch_meanvar_arg, 
                       n_genes=n_genes)
  # add to baseline data
  baseBatch_Lst <- baseLst
  if(withBatch){
    for(i in 1:2){
      baseBatch_Lst$trn_x[, batchLst$batch_ind_trn[[i]]] <- baseLst$trn_x[, batchLst$batch_ind_trn[[i]]] + batchLst$batchMat[[i]]
    }                       
    for(j in 3:4){
      baseBatch_Lst$tst_x[, batchLst$batch_ind_tst[[j-2]]] <- baseLst$tst_x[, batchLst$batch_ind_tst[[j-2]]] + batchLst$batchMat[[j]]
    }
  }
  
  
  #### Step 3: Adjust batch with ComBat ####
  batch_train <- rep(0, n_sample_trn); batch_test <- rep(0, n_sample_tst)
  for(i in 1:2){
    batch_train[batchLst$batch_ind_trn[[i]]] <- i
  }
  for(j in 3:4){
    batch_test[batchLst$batch_ind_tst[[j-2]]] <- j
  }
  combatLst <- adjBatch(datLst=baseBatch_Lst, batch_train=batch_train, batch_test=batch_test, 
                        sep_cmb=sep_cmb, combat_mod=combat_mod)
  
  
  #### Step 4: Preparation before training and validation ####
  trn_set <- combatLst$trn_x
  y_trn <- combatLst$trn_y
  tst_set <- combatLst$tst_x
  y_tst <- combatLst$tst_y
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
  rownames(res_mat_tst[[iter]]) <- rownames(res_mat_trn [[iter]]) <- c("LASSO", "Elastic Net",
                                                                      "Naive Bayes", "SVM",
                                                                      "kNN", "Random Forest",
                                                                      "Neural Nets", "Mas-o-menos",
                                                                      "Weighted Majority")
  # baseSets[[iter]] <- baseLst
  # baseBatchSets[[iter]] <- baseBatch_Lst
  combatSets[[iter]] <- combatLst
  batchInfo <- batchLst
}

filename_seq <- c(withBatch, batch_meanvar_arg, sep_cmb, combat_mod,
                  n_sample_trn, n_sample_tst, p_trn_case*10, p_tst_case*10,
                  p_batch1_case*10, p_batch1_control*10, p_batch3_case*10, p_batch3_control*10)
save(res_mat_tst, res_mat_trn,
     #baseSets, baseBatchSets, 
     combatSets, batchInfo,
     file=paste("results/simPipe_", paste(filename_seq, collapse= "_"), ".RData", sep=""))