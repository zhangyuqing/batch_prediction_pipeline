rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")

source("scripts/modStats_new.R")
source("scripts/adjBatch.R")
source("scripts/prediction_mods.R")



######################## Input Arguments ########################
command_args <- commandArgs(trailingOnly=TRUE)
if (length(command_args) != 4){
  print("ERROR: 4 parameters required: <data> <sep_cmb> <combat_mod> <norm_ind>") 
  quit(save = "no", status = 1, runLast = FALSE)
}
dataname <- command_args[1] # name of data to be loaded
sep_cmb <- command_args[2] # separate/combined
combat_mod <- command_args[3] # mod/null
norm_ind <- as.logical(command_args[4]) # standardize data first?


######################## Load Data ########################
load(paste("data/", dataname, ".RData", sep=""))


######################## Run pipeline ########################
set.seed(1)

#### Step 1: Set up Input ####
rm_ind <- which(batch %in% c("2", "33", "44", "56", "63", "70", "109"))
dat <- VPA_set[, -rm_ind]
response <- VPA_condition[-rm_ind]
batch <- batch[-rm_ind]

train_ind <- which(batch %in% c("506", "513", "602", "603", "626", "650"))
test_ind <- which(batch %in% c("727", "725", "757", "750", "767", "765"))

trn_x <- dat[, train_ind]
trn_y <- response[train_ind]
batch_train <- batch[train_ind]
tst_x <- dat[, test_ind]
tst_y <- response[test_ind]
batch_test <- batch[test_ind]

baseBatch_Lst <- list(trn_x=trn_x, trn_y=trn_y, tst_x=tst_x, tst_y=tst_y)


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
if(norm_ind){
  trn_set <- t(scale(t(trn_set), center=TRUE, scale=TRUE))
  tst_set <- t(scale(t(tst_set), center=TRUE, scale=TRUE))
}
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


#### Step 5: Summarization ####
res_mat_tst <- rbind(unlist(stats_lasso_test), unlist(stats_elnet_test),
                     unlist(stats_nb_test), unlist(stats_svm_test),
                     unlist(stats_knn_test), unlist(stats_rf_test),
                     unlist(stats_nnet_test), unlist(stats_mas_test),
                     unlist(stats_vote_test))
res_mat_trn <- rbind(unlist(stats_lasso_train), unlist(stats_elnet_train),
                     unlist(stats_nb_train), unlist(stats_svm_train),
                     unlist(stats_knn_train), unlist(stats_rf_train),
                     unlist(stats_nnet_train), unlist(stats_mas_train),
                     unlist(stats_vote_train))
rownames(res_mat_tst) <- rownames(res_mat_trn) <- c("LASSO", "Elastic Net",
                                                    "Naive Bayes", "SVM",
                                                    "kNN", "Random Forest",
                                                    "Neural Nets", "Mas-o-menos",
                                                    "Weighted Majority")


filename_seq <- c(dataname, sep_cmb, combat_mod, norm_ind)
save(res_mat_tst, res_mat_trn, combatLst, 
     file=paste("results/", paste(filename_seq, collapse= "_"), ".RData", sep=""))