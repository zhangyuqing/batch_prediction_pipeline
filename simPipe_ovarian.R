rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")

source("scripts/adjBatch.R")
source("scripts/prediction_mods_surv.R")



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
library(Biobase)
load(paste("data/", dataname, ".RData", sep=""))
esets <- esets[c(8,5)]
names(esets)


######################## Run pipeline ########################
set.seed(1)

#### Step 1: Set up Input ####
trn_x <- exprs(esets[[1]])
trn_y <- esets[[1]]$y
batch_train <- esets[[1]]$batch
tst_x <- exprs(esets[[2]])
tst_y <- esets[[2]]$y
batch_test <- esets[[2]]$batch
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
  
## normalization by gene (Optional)
if(norm_ind){
  trn_set <- t(scale(t(trn_set), center=TRUE, scale=TRUE))
  tst_set <- t(scale(t(tst_set), center=TRUE, scale=TRUE))
}

  
#### Step 4: Predictive Models ####
library(Hmisc)
  
## Mas-o-menos
res_mas <- predMas(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn) 
stats_mas_train <- rcorr.cens(-res_mas$pred_trn, y_trn)["C Index"]
stats_mas_test <- rcorr.cens(-res_mas$pred_tst, y_tst)["C Index"]
  
## Ridge
res_ridge <- predRidge(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn)
stats_ridge_train <- rcorr.cens(-res_ridge$pred_trn, y_trn)["C Index"]
stats_ridge_test <- rcorr.cens(-res_ridge$pred_tst, y_tst)["C Index"]
  
## LASSO 
res_lasso <- predLasso(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn)
stats_lasso_train <- rcorr.cens(-res_lasso$pred_trn, y_trn)["C Index"]
stats_lasso_test <- rcorr.cens(-res_lasso$pred_tst, y_tst)["C Index"]

## Unicox 
res_unicox<- predUnicox(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn)
stats_unicox_train <- rcorr.cens(-res_unicox$pred_trn, y_trn)["C Index"]
stats_unicox_test <- rcorr.cens(-res_unicox$pred_tst, y_tst)["C Index"]

## SuperPC 
res_superpc<- predSuperPC(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn, y_tst=y_tst)
stats_superpc_train <- rcorr.cens(-res_superpc$pred_trn, y_trn)["C Index"]
stats_superpc_test <- rcorr.cens(-res_superpc$pred_tst, y_tst)["C Index"]

## CoxBoost
res_coxboost <- predCoxBoost(trn_set=trn_set, tst_set=tst_set, y_trn=y_trn)
stats_coxboost_train <- rcorr.cens(-res_coxboost$pred_trn, y_trn)["C Index"]
stats_coxboost_test <- rcorr.cens(-res_coxboost$pred_tst, y_tst)["C Index"]

## Weighted Voting
# train
pred_trn_vote <- cbind(res_mas$pred_trn, res_ridge$pred_trn, res_lasso$pred_trn,
                       res_unicox$pred_trn, res_superpc$pred_trn, res_coxboost$pred_trn)
weightseq <- c(stats_mas_train, stats_ridge_train, stats_lasso_train,
               stats_unicox_train, stats_superpc_train, stats_coxboost_train)
weightseq <- weightseq / sum(weightseq)
pred_trn_vote <- pred_trn_vote %*% weightseq
# test
pred_tst_vote <- cbind(res_mas$pred_tst, res_ridge$pred_tst, res_lasso$pred_tst,
                       res_unicox$pred_tst, res_superpc$pred_tst, res_coxboost$pred_tst)
weightseq <- c(stats_mas_test, stats_ridge_test, stats_lasso_test,
               stats_unicox_test, stats_superpc_test, stats_coxboost_test)
weightseq <- weightseq / sum(weightseq)
pred_tst_vote <- pred_tst_vote %*% weightseq
# summarize
res_vote <- list(pred_trn=pred_trn_vote, pred_tst=pred_tst_vote)
stats_vote_train <- rcorr.cens(-res_vote$pred_trn, y_trn)["C Index"]
stats_vote_test <- rcorr.cens(-res_vote$pred_tst, y_tst)["C Index"]
  
  
#### Step 6: summarize results ####
## Summarization ##
res_mat_tst <- c(stats_mas_test, stats_ridge_test, stats_lasso_test,
                 stats_unicox_test, stats_superpc_test, stats_coxboost_test,
                 stats_vote_test)
res_mat_trn <- c(stats_mas_train, stats_ridge_train, stats_lasso_train,
                 stats_unicox_train, stats_superpc_train, stats_coxboost_train,
                 stats_vote_train)
  
names(res_mat_tst) <- names(res_mat_trn) <- c("Mas-o-menos", "Ridge", "Lasso",
                                              "UniCox", "SuperPC", "CoxBoost", "WeightedVote")


filename_seq <- c(dataname, sep_cmb, combat_mod, norm_ind)
save(res_mat_tst, res_mat_trn, combatLst, 
     file=paste("results/", paste(filename_seq, collapse= "_"), ".RData", sep=""))