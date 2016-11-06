rm(list=ls())
setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
cmap_design <- read.csv("../cmap_design.csv", as.is=TRUE, header=TRUE)

drugname <- c("valproic acid", "vorinostat", "trichostatin A") ## VPA, SAHA, TSA
grep(drugname[3], cmap_design$cmap_name)
