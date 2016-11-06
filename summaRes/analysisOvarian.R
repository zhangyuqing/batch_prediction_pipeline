rm(list=ls())
setwd("/restricted/projectnb/combat/batch_prediction_pipeline/results/")


#### Load Data ####
load("ovarian_none_none.RData")
batch_trn <- do.call(rbind, res_mat_trn)
batch_tst <- do.call(rbind, res_mat_tst)
rm(res_mat_trn, res_mat_tst)

load("ovarian_separate_mod.RData")
sepmod_trn <- do.call(rbind, res_mat_trn)
sepmod_tst <- do.call(rbind, res_mat_tst)
rm(res_mat_trn, res_mat_tst)

load("ovarian_separate_null.RData")
sephyb_trn <- do.call(rbind, res_mat_trn)
sephyb_tst <- do.call(rbind, res_mat_tst)
rm(res_mat_trn, res_mat_tst)

load("ovarian_combined_mod.RData")
cmbmod_trn <- do.call(rbind, res_mat_trn)
cmbmod_tst <- do.call(rbind, res_mat_tst)
rm(res_mat_trn, res_mat_tst)

load("ovarian_combined_null.RData")
cmbnull_trn <- do.call(rbind, res_mat_trn)
cmbnull_tst <- do.call(rbind, res_mat_tst)
rm(res_mat_trn, res_mat_tst)


#### Create boxplot ####
method_name <- "Mas-o-menos"
png(file=paste("../boxplots/ovarian_", method_name, ".png", sep=""),
    width=600, height=400)
res <- list(batch_trn[, method_name], batch_tst[, method_name],
            sepmod_trn[, method_name], sepmod_tst[, method_name],
            sephyb_trn[, method_name], sephyb_tst[, method_name],
            cmbmod_trn[, method_name], cmbmod_tst[, method_name],
            cmbnull_trn[, method_name], cmbnull_tst[, method_name])
names(res) <- NULL
color.seq <- rep(c("blue","orange"), times=length(res)/2)
par(xpd=T,mar=par()$mar+c(0,0,3,5))
boxplot(res, col=color.seq, ylab="C Index", xlab="", boxwex = 0.5, ylim=c(0,1),
        width=rep(0.3, length(res)), cex.axis=1.1, cex.lab=1.1, xaxt="n")
legend(11, 0.98, legend=c("train","test"),
       col=c("blue","orange"),pch=c(16,16), cex=1.1,text.width=1.1,bty="n")
s = seq(from=0, to=1, by=0.002)
lines(rep(2.5,length(s)),s,type="l",lty=2)
lines(rep(4.5,length(s)),s,type="l",lty=2)
lines(rep(6.5,length(s)),s,type="l",lty=2)
lines(rep(8.5,length(s)),s,type="l",lty=2)

text(x=1.5,y=1.1,labels=c("Batch"),cex=1)
text(x=3.5,y=1.1,labels=c("2-step mod"),cex=1)
text(x=5.5,y=1.1,labels=c("2-step hyb"),cex=1)
text(x=7.5,y=1.1,labels=c("1-step mod"),cex=1)
text(x=9.5,y=1.1,labels=c("1-step null"),cex=1)
text(x=2, y=1.3, labels=c(method_name),cex=1.3)
dev.off()