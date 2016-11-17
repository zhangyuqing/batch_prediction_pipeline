################ Conclusions from simulation ################
rm(list=ls())
#setwd("C:/Users/zhang/Documents/Work/Evan/predictive-models/100616-refine-pipeline/results")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/results/")

source("../summaRes/loadStat.R")
source("../summaRes/plotBox2.R")
source("../summaRes/loadGaps.R")
source("../summaRes/plotLine.R")
source("../summaRes/plotLine2.R")
source("../summaRes/loadTrnsTsts.R")

experiments_ID <- c("200_100_200_100_2_2_50_50_50_50_50_50_50_50", # balanced
                    "200_40_200_140_2_2_20_80_20_80_70_30_70_30", # trn/tst %
                    "200_100_200_100_2_2_30_70_70_30_20_80_80_20", # batch %
                    "200_100_200_100_2_2_20_60_80_40_30_90_70_10", # batch size & %
                    "200_40_200_140_2_2_8_144_32_16_42_54_98_6") # all
method_names <- c("Lasso", "Elnet", "NaiveBayes", "SVM",
                  "kNN", "RandForest", "NeuralNets", "Mas-o-menos","WeightedVoting")
design_names <- c("balanced", "trn_tst", "batch_portion", "batch_portion_size", "all")
combat_names <- c("base", "batch", "sep_mod",
                  "sep_hyb", "cmb_mod", "cmp_null")


#### Boxplot of performance statistics
AUC_lst <- sens_lst <- spec_lst <- list()
for(expID in 1:length(experiments_ID)){
  statList <- loadStat(experiments_ID[expID], datadir="simulation/") ## all performance matrices for one design
  
  AUC_lst[[expID]] <- plotBox2(datList=statList, method_names=method_names, 
                               statname="AUC", expID=expID)
  sens_lst[[expID]] <- plotBox2(datList=statList, method_names=method_names, 
                                statname="sensitivity", expID=expID)
  spec_lst[[expID]] <- plotBox2(datList=statList, method_names=method_names, 
                                statname="specificity", expID=expID)
}  
names(AUC_lst) <- names(sens_lst) <- names(spec_lst) <- design_names
rm(statList)


#### Quantitative table of the statistics
## AUC
for(designIND in 1:length(design_names)){
  AUC_design <- AUC_lst[[designIND]]
  for(method_row in 1:length(method_names)){
    stats_by_method_trn <- matrix(0, nrow=5, ncol=length(combat_names),
                                  dimnames=list(c("mean-train", "median-train", 
                                                  "var-train", "min-train", 
                                                  "max-train"), 
                                                combat_names))
    stats_by_method_tst <- matrix(0, nrow=5, ncol=length(combat_names),
                                  dimnames=list(c("mean-test", "median-test", 
                                                  "var-test", "min-test", 
                                                  "max-test"), 
                                                combat_names))
    # train
    tmp_train <- AUC_design[[method_row]]$trn
    tmp_train <- do.call(cbind, tmp_train)
    stats_by_method_trn[1, ] <- apply(tmp_train, 2, mean)
    stats_by_method_trn[2, ] <- apply(tmp_train, 2, median)
    stats_by_method_trn[3, ] <- apply(tmp_train, 2, var)
    stats_by_method_trn[4, ] <- apply(tmp_train, 2, min)
    stats_by_method_trn[5, ] <- apply(tmp_train, 2, max)
    # test
    tmp_test <- AUC_design[[method_row]]$tst
    tmp_test <- do.call(cbind, tmp_test)
    stats_by_method_tst[1, ] <- apply(tmp_test, 2, mean)
    stats_by_method_tst[2, ] <- apply(tmp_test, 2, median)
    stats_by_method_tst[3, ] <- apply(tmp_test, 2, var)
    stats_by_method_tst[4, ] <- apply(tmp_test, 2, min)
    stats_by_method_tst[5, ] <- apply(tmp_test, 2, max)
    stats_by_method_mat <- rbind(stats_by_method_trn, stats_by_method_tst)
    write.csv(round(stats_by_method_mat, 5),
              file=paste("../figures/boxplots/AUC/", 
                         paste(method_names[method_row], designIND, sep="_"), 
                         ".csv", sep=""))
  }
}

## sensitivity
for(designIND in 1:length(design_names)){
  sens_design <- sens_lst[[designIND]]
  for(method_row in 1:length(method_names)){
    stats_by_method_trn <- matrix(0, nrow=5, ncol=length(combat_names),
                                  dimnames=list(c("mean-train", "median-train", 
                                                  "var-train", "min-train", 
                                                  "max-train"), 
                                                combat_names))
    stats_by_method_tst <- matrix(0, nrow=5, ncol=length(combat_names),
                                  dimnames=list(c("mean-test", "median-test", 
                                                  "var-test", "min-test", 
                                                  "max-test"), 
                                                combat_names))
    # train
    tmp_train <- sens_design[[method_row]]$trn
    tmp_train <- do.call(cbind, tmp_train)
    stats_by_method_trn[1, ] <- apply(tmp_train, 2, mean)
    stats_by_method_trn[2, ] <- apply(tmp_train, 2, median)
    stats_by_method_trn[3, ] <- apply(tmp_train, 2, var)
    stats_by_method_trn[4, ] <- apply(tmp_train, 2, min)
    stats_by_method_trn[5, ] <- apply(tmp_train, 2, max)
    # test
    tmp_test <- sens_design[[method_row]]$tst
    tmp_test <- do.call(cbind, tmp_test)
    stats_by_method_tst[1, ] <- apply(tmp_test, 2, mean)
    stats_by_method_tst[2, ] <- apply(tmp_test, 2, median)
    stats_by_method_tst[3, ] <- apply(tmp_test, 2, var)
    stats_by_method_tst[4, ] <- apply(tmp_test, 2, min)
    stats_by_method_tst[5, ] <- apply(tmp_test, 2, max)
    stats_by_method_mat <- rbind(stats_by_method_trn, stats_by_method_tst)
    write.csv(round(stats_by_method_mat, 5),
              file=paste("../figures/boxplots/sensitivity/", 
                         paste(method_names[method_row], designIND, sep="_"), 
                         ".csv", sep=""))
  }
}

## specificity
for(designIND in 1:length(design_names)){
  spec_design <- spec_lst[[designIND]]
  for(method_row in 1:length(method_names)){
    stats_by_method_trn <- matrix(0, nrow=5, ncol=length(combat_names),
                                  dimnames=list(c("mean-train", "median-train", 
                                                  "var-train", "min-train", 
                                                  "max-train"), 
                                                combat_names))
    stats_by_method_tst <- matrix(0, nrow=5, ncol=length(combat_names),
                                  dimnames=list(c("mean-test", "median-test", 
                                                  "var-test", "min-test", 
                                                  "max-test"), 
                                                combat_names))
    # train
    tmp_train <- spec_design[[method_row]]$trn
    tmp_train <- do.call(cbind, tmp_train)
    stats_by_method_trn[1, ] <- apply(tmp_train, 2, mean)
    stats_by_method_trn[2, ] <- apply(tmp_train, 2, median)
    stats_by_method_trn[3, ] <- apply(tmp_train, 2, var)
    stats_by_method_trn[4, ] <- apply(tmp_train, 2, min)
    stats_by_method_trn[5, ] <- apply(tmp_train, 2, max)
    # test
    tmp_test <- spec_design[[method_row]]$tst
    tmp_test <- do.call(cbind, tmp_test)
    stats_by_method_tst[1, ] <- apply(tmp_test, 2, mean)
    stats_by_method_tst[2, ] <- apply(tmp_test, 2, median)
    stats_by_method_tst[3, ] <- apply(tmp_test, 2, var)
    stats_by_method_tst[4, ] <- apply(tmp_test, 2, min)
    stats_by_method_tst[5, ] <- apply(tmp_test, 2, max)
    stats_by_method_mat <- rbind(stats_by_method_trn, stats_by_method_tst)
    write.csv(round(stats_by_method_mat, 5),
              file=paste("../figures/boxplots/specificity/", 
                         paste(method_names[method_row], designIND, sep="_"), 
                         ".csv", sep=""))
  }
}


#### Line chart of AUC train, AUC test, AUC gap (by design)
## AUC gap
design_gaps_List <- list()
for(expID in 1:length(experiments_ID)){
  design_gaps_List[[expID]] <- loadGaps(experiments_ID[expID], datadir="simulation/") ## all AUC gaps matrices for one design
}
names(design_gaps_List) <- c("balanced", "trn/tst %", "batch %", "batch size & %", "all")
plotLine(design_gaps_List)

## AUC train
design_trns_List <- list()
for(expID in 1:length(experiments_ID)){
  design_trns_List[[expID]] <- loadTrns(experiments_ID[expID], datadir="simulation/") ## all AUC gaps matrices for one design
}
names(design_trns_List) <- c("balanced", "trn/tst %", "batch %", "batch size & %", "all")
plotLine(design_trns_List)

## AUC test
design_tsts_List <- list()
for(expID in 1:length(experiments_ID)){
  design_tsts_List[[expID]] <- loadTsts(experiments_ID[expID], datadir="simulation/") ## all AUC gaps matrices for one design
}
names(design_tsts_List) <- c("balanced", "trn/tst %", "batch %", "batch size & %", "all")
plotLine(design_tsts_List)


#### Line chart of AUC train, AUC test, AUC gap (by combat adjustment method)
design_regex <- c("1.csv", "2.csv", "3.csv", "4.csv", "5.csv")
stats_files_dir <- "../figures/boxplots/AUC/"
stats_filelst <- dir(stats_files_dir)
for(i in 1:length(design_regex)){
  part_files <- stats_filelst[grep(design_regex[i], stats_filelst)]
  print(length(part_files))
  stats_readin_lst <- list()
  for(j in 1:length(part_files)){
    stats_readin_lst[[j]] <- read.csv(paste(stats_files_dir, part_files[j], sep=""), 
                                      header=TRUE, row.names="X")
  }
  # train
  readin_mean_trn <- lapply(stats_readin_lst, function(stats_readin){
    return(stats_readin["mean-train", ])
  })
  names(readin_mean_trn) <- sub('......$', '', part_files)
  readin_mean_trn <- do.call(rbind, readin_mean_trn)
  plotLine2(readin_mean_trn, outdir="../figures/linechart_sim/AUC_train_bydata/", 
            design_name=design_names[i], ylab_text="Average AUC Train")
  
  # test
  readin_mean_tst <- lapply(stats_readin_lst, function(stats_readin){
    return(stats_readin["mean-test", ])
  })
  names(readin_mean_tst) <- sub('......$', '', part_files)
  readin_mean_tst <- do.call(rbind, readin_mean_tst)
  plotLine2(readin_mean_tst, outdir="../figures/linechart_sim/AUC_test_bydata/", 
            design_name=design_names[i], ylab_text="Average AUC Test")
  # gap
  plotLine2(readin_mean_trn-readin_mean_tst, outdir="../figures/linechart_sim/AUC_gap_bydata/", 
            design_name=design_names[i], ylab_text="Average AUC Gaps")
  
}



################ Mean-only ################
#### TODO


################ Real Data Example: Ovarian ################
rm(list=ls())

#### Load Data ####
load("oval/ovarian_none_none_FALSE.RData")
batch_trn_mat <- res_mat_trn
batch_tst_mat <- res_mat_tst
rm(res_mat_trn, res_mat_tst)
load("oval/ovarian_separate_null_FALSE.RData")
sephyb_trn_mat <- res_mat_trn
sephyb_tst_mat <- res_mat_tst
rm(res_mat_trn, res_mat_tst)
load("oval/ovarian_combined_null_FALSE.RData")
cmbnull_trn_mat <- res_mat_trn
cmbnull_tst_mat <- res_mat_tst
rm(res_mat_trn, res_mat_tst)
rm(combatLst)


#### Train Mat ####
train_mat <- t(rbind(batch_trn_mat, sephyb_trn_mat, cmbnull_trn_mat))
colnames(train_mat) <- c("w/o adjustment", "2-step hyb", "1-step null")

#### Test Mat ####
test_mat <- t(rbind(batch_tst_mat, sephyb_tst_mat, cmbnull_tst_mat))
colnames(test_mat) <- c("w/o adjustment", "2-step hyb", "1-step null")

#### Gap Mat ####
gap_mat <- train_mat - test_mat


#### Line Chart ####
datmat <- gap_mat
library(RColorBrewer)
n <- nrow(datmat)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colorseq = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
png(file="../figures/linechart_ovarian/ovarian_gap.png",
    width=800, height=600)
par(xpd=T, mar=par()$mar+c(1,1,1,12))  
plot(datmat[1, ], type="b", col=colorseq[1],
     ylim=c(min(datmat), max(datmat)),
     ylab="C statistics", xlab="", 
     main="Train - Test", xaxt="n",
     cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
axis(1, at=1:ncol(datmat), labels=colnames(datmat), cex.axis=1.3)
for(i in 2:nrow(datmat)){
  lines(datmat[i, ], type="b", col=colorseq[i])
}
legend(x=3.2, y=mean(datmat)+0.02, legend=rownames(datmat),
       col=colorseq[1:nrow(datmat)], lty=rep(1,n), lwd=rep(2.5,n),
       cex=1.3)
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off() 

write.csv(round(train_mat,3), file="ovarian_train_bydata.csv")
write.csv(round(test_mat,3), file="ovarian_test_bydata.csv")
write.csv(round(gap_mat,3), file="ovarian_gap_bydata.csv")


################ Real Data Example: C-Map VPA ################
rm(list=ls())
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")
statname <- "AUC"

#### Load Data ####
load("results/cmap/cmap_whole_VPA_none_none_FALSE.RData")
batch_trn_mat <- res_mat_trn[, statname]
batch_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/cmap/cmap_whole_VPA_separate_mod_FALSE.RData")
sepmod_trn_mat <- res_mat_trn[, statname]
sepmod_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/cmap/cmap_whole_VPA_separate_null_FALSE.RData")
sephyb_trn_mat <- res_mat_trn[, statname]
sephyb_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/cmap/cmap_whole_VPA_combined_mod_FALSE.RData")
cmbmod_trn_mat <- res_mat_trn[, statname]
cmbmod_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/cmap/cmap_whole_VPA_combined_null_FALSE.RData")
cmbnull_trn_mat <- res_mat_trn[, statname]
cmbnull_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
rm(combatLst)

#### Train Mat ####
train_mat <- t(rbind(batch_trn_mat, sepmod_trn_mat, cmbmod_trn_mat, sephyb_trn_mat, cmbnull_trn_mat))
colnames(train_mat) <- c("w/o adjustment", "2-step full", "1-step full", "2-step hyb", "1-step null")

#### Test Mat ####
test_mat <- t(rbind(batch_tst_mat, sepmod_tst_mat, cmbmod_tst_mat, sephyb_tst_mat, cmbnull_tst_mat))
colnames(test_mat) <- c("w/o adjustment", "2-step full", "1-step full", "2-step hyb", "1-step null")

#### Gap Mat ####
gap_mat <- train_mat - test_mat


#### Line Chart ####
datmat <- gap_mat
library(RColorBrewer)
n <- nrow(datmat)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colorseq = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
png(file="figures/linechart_cmap/cmapWhole_VPA_AUC_gap_bydata.png",
    width=800, height=600)
par(xpd=T, mar=par()$mar+c(1,1,1,12))  
plot(datmat[1, ], type="b", col=colorseq[1],
     ylim=c(min(datmat), max(datmat)),
     ylab=statname, xlab="", 
     main="Train - Test", xaxt="n",
     cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
axis(1, at=1:ncol(datmat), labels=colnames(datmat), cex.axis=1.3)
for(i in 2:nrow(datmat)){
  lines(datmat[i, ], type="b", col=colorseq[i])
}
legend(x=5.2, y=mean(datmat)+0.02, legend=rownames(datmat),
       col=colorseq[1:nrow(datmat)], lty=rep(1,n), lwd=rep(2.5,n),
       cex=1.3)
par(mar=c(5, 4, 4, 2) + 0.1)
dev.off() 

write.csv(round(train_mat,3), file="cmapWhole_VPA_AUC_train_bydata.csv")
write.csv(round(test_mat,3), file="cmapWhole_VPA_AUC_test_bydata.csv")
write.csv(round(gap_mat,3), file="cmapWhole_VPA_AUC_gap_bydata.csv")
