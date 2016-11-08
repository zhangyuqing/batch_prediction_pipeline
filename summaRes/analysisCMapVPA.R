rm(list=ls())
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")
statname <- "specificity"

#### Load Data ####
load("results/short_cmap_VPA_none_none_FALSE.RData")
batch_trn_mat <- res_mat_trn[, statname]
batch_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/short_cmap_VPA_separate_mod_FALSE.RData")
sepmod_trn_mat <- res_mat_trn[, statname]
sepmod_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/short_cmap_VPA_separate_null_FALSE.RData")
sephyb_trn_mat <- res_mat_trn[, statname]
sephyb_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/short_cmap_VPA_combined_mod_FALSE.RData")
cmbmod_trn_mat <- res_mat_trn[, statname]
cmbmod_tst_mat <- res_mat_tst[, statname]
rm(res_mat_trn, res_mat_tst)
load("results/short_cmap_VPA_combined_null_FALSE.RData")
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
png(file="lineChart/cmap_VPA_gap_spec.png",
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
