rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
setwd("/restricted/projectnb/combat/batch_prediction_pipeline/")

#source("scripts/modStats_new.R")
source("scripts/adjBatch.R")
source("scripts/prediction_mods_surv.R")



######################## Input Arguments ########################
command_args <- commandArgs(trailingOnly=TRUE)
if (length(command_args) != 3){
  print("ERROR: 3 parameters required: <data> <sep_cmb> <combat_mod>") 
  quit(save = "no", status = 1, runLast = FALSE)
}
dataname <- command_args[1] # name of data to be loaded
sep_cmb <- command_args[2] # separate/combined
combat_mod <- command_args[3] # mod/null



######################## Load Data ########################
library(Biobase)
load(paste("data/", dataname, ".RData", sep=""))



######################## Run pipeline ########################
iterations <- 100
set.seed(1)

res_mat_trn <- res_mat_tst <- list()
setsID <- list()
for(iter in 1:iterations){
  print(paste("SIMULATION:",iter,sep=""))
  
  #### Step 1: Set up Input ####
  ## For curated Ovarian data
  setsID[[iter]] <- sample(length(esets), 2, replace=FALSE)
  
  trn_x <- exprs(esets[[setsID[[iter]][1]]])
  trn_y <- pData(esets[[setsID[[iter]][1]]])$y
  batch_train <- pData(esets[[setsID[[iter]][1]]])$batch
  tst_x <- exprs(esets[[setsID[[iter]][2]]])
  tst_y <- pData(esets[[setsID[[iter]][2]]])$y
  batch_test <- pData(esets[[setsID[[iter]][2]]])$batch
  baseBatch_Lst <- list(trn_x=trn_x, trn_y=trn_y, batch_train=batch_train,
                        tst_x=tst_x, tst_y=tst_y, batch_test=batch_test)
    
  
  #### Step 2: Adjust batch with ComBat ####
  combatLst <- adjBatch(datLst=baseBatch_Lst, 
                        batch_train=baseBatch_Lst$batch_train, 
                        batch_test=baseBatch_Lst$batch_test, 
                        sep_cmb=sep_cmb, combat_mod=combat_mod)
  
  
  #### Step 3: Preparation before training and validation ####
  trn_set <- combatLst$trn_x
  y_trn <- combatLst$trn_y
  tst_set <- combatLst$tst_x
  y_tst <- combatLst$tst_y
  
  ## normalization by gene (Optional)
  # trn_set <- t(scale(t(trn_set), center=TRUE, scale=TRUE))
  # tst_set <- t(scale(t(tst_set), center=TRUE, scale=TRUE))
  
  ## shuffle individuals to mix up cases and controls #
  # ind_trn <- sample(1:ncol(trn_set), ncol(trn_set), replace=FALSE)
  # ind_tst <- sample(1:ncol(tst_set), ncol(tst_set), replace=FALSE)
  # trn_set <- trn_set[, ind_trn]
  # y_trn <- y_trn[ind_trn]
  # tst_set <- tst_set[, ind_tst]
  # y_tst <- y_tst[ind_tst]
  ## swith to data.frame
  # training_df <- data.frame(t(trn_set), as.factor(y_trn))
  # colnames(training_df) <- c(paste("gene", 1:nrow(trn_set), sep=""), "response")
  # rownames(training_df) <- 1:ncol(trn_set)
  # test_df <- data.frame(t(tst_set), as.factor(y_tst))
  # colnames(test_df) <- c(paste("gene", 1:nrow(tst_set), sep=""), "response")
  # rownames(test_df) <- 1:ncol(tst_set)
  
  
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
  
  
  #### Step 6: summarize results ####
  ## Summarization ##
  res_mat_tst[[iter]] <- c(stats_mas_test, stats_ridge_test)
  res_mat_trn[[iter]] <- c(stats_mas_train, stats_ridge_train)
  
  names(res_mat_tst[[iter]]) <- names(res_mat_trn [[iter]]) <- c("Mas-o-menos", "Ridge")
  # baseSets[[iter]] <- baseLst
  # baseBatchSets[[iter]] <- baseBatch_Lst
  # combatSets[[iter]] <- combatLst
}

filename_seq <- c(dataname, sep_cmb, combat_mod)
save(res_mat_tst, res_mat_trn, combatLst, setsID,
     file=paste("results/", paste(filename_seq, collapse= "_"), ".RData", sep=""))