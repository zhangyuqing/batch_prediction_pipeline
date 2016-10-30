rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
setwd("/restricted/projectnb/combat/batch_prediction_pipeline/")

source("scripts/modStats_new.R")
source("scripts/adjBatch.R")
source("scripts/prediction_mods.R")


######################## Load Data ########################


######################## Input Arguments ########################
command_args <- commandArgs(trailingOnly=TRUE)
if (length(command_args) != 2){
  print("ERROR: 2 parameters required: <sep_cmb> <combat_mod>") 
  quit(save = "no", status = 1, runLast = FALSE)
}
sep_cmb <- command_args[1] # separate/combined
combat_mod <- command_args[2] # mod/null
iterations <- 20
set.seed(1)



######################## Run pipeline ########################
res_mat_trn <- res_mat_tst <- list()
for(iter in 1:iterations){
  print(paste("SIMULATION:",iter,sep=""))
  
  #### Step 1: Set up Input ####
  baseBatch_Lst <- list(trn_x=trn_set, trn_y=y_trn,
                        tst_x=tst_set, tst_y=y_tst,
                        n_trn_case=n_trn_case, n_trn_control=n_trn_control, 
                        n_tst_case=n_tst_case, n_tst_control=n_tst_control)
  batch_train <- c()
  batch_test <- c()
  
  #### Step 2: Adjust batch with ComBat ####
  combatLst <- adjBatch(datLst=baseBatch_Lst, 
                        batch_train=batch_train, batch_test=batch_test, 
                        sep_cmb=sep_cmb, combat_mod=combat_mod)
  
  
  #### Step 3: Preparation before training and validation ####
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
  
  
  #### Step 4: Predictive Models ####
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
  
  
  #### Step 5: summarize results ####
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
}

save(res_mat_tst, res_mat_trn,
     file="results/readDat.RData")