rm(list=ls())
#setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")

library(Biobase)
load("data/ovarian.RData")
esets <- esets[c(8,5)]
names(esets)

library(survival)

fit_TCGA <- survfit(y ~ 1, data = pData(esets[[1]]))
plot(fit_TCGA, xlab="Time (days)", ylab="Probability of Survival", main="TCGA (Train)")

fit_GSE <- survfit(y ~ 1, data = pData(esets[[2]]))
plot(fit_GSE, xlab="Time (days)", ylab="Probability of Survival", main="GSE30161 (Test)")


load("data/ovarian.RData")
for(i in 1:length(esets)){
  ff <- survfit(y ~ 1, data=pData(esets[[i]]))
  png(file=paste("../survival_plot/",sub('.....$','', names(esets)[i]),".png", sep=""),
      width=454, height=327)
  plot(ff, xlab="Time (days)", ylab="Probability of Survival", main=sub('.....$','', names(esets)[i]))
  dev.off()
}
