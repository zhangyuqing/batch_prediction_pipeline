rm(list=ls())
setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/data/")
library(Biobase)
library(BatchQC)
load("ovarian.RData")

setID <- 2

edata <- exprs(esets[[setID]])
batch <- esets[[setID]]$batch
#condition <- esets[[setID]]$vital_status

one_sam_batch <- names(table(batch))[which(table(batch)==1)]
rm_ind <- which(batch %in% one_sam_batch)
if(length(rm_ind) > 0){
  edata <- edata[, -rm_ind]
  batch <- batch[-rm_ind]
}

na_batch <- which(is.na(batch))
if(length(na_batch)>0){
  edata <- edata[, -na_batch]
  batch <- batch[-na_batch]
}

batchQC(edata, batch=batch, condition=NULL, 
        report_file=paste("batchqc_cleaned_", names(esets)[setID], ".html", sep=""), 
        report_dir="../../batchQC_res/", 
        report_option_binary="111111111",
        view_report=FALSE, interactive=TRUE, batchqc_output=FALSE, log2cpm_transform=FALSE)
# ERROR if not remove single-sample batches:
#   Error in if (var(data.matrix[i, batch2[[j]]]) == 0) { : 
#   missing value where TRUE/FALSE needed
# ERROR after removing single-sample batches: run until 48%
#   Quitting from lines 178-179 (batchqc_report.Rmd) 
#   Error in quantile.default(med_cor, p = 0.25) : 
#   missing values and NaN's not allowed if 'na.rm' is FALSE


rm(list=ls())
load("ovarian.RData")
names(esets)
rm(esets)
library(curatedOvarianData)
data(GSE14764_eset)
data(GSE18520_eset)
data(GSE26193_eset)
data(GSE26712_eset)
data(GSE30161_eset)
data(GSE9891_eset)
data(PMID17290060_eset)
data(TCGA_eset)

esets <- list(GSE14764=GSE14764_eset, GSE18520=GSE18520_eset,
              GSE26193=GSE26193_eset, GSE26712=GSE26712_eset,
              GSE30161=GSE30161_eset, GSE9891=GSE9891_eset,
              PMID17290060=PMID17290060_eset, TCGA=TCGA_eset)
setID <- 8

edata <- exprs(esets[[setID]])
batch <- esets[[setID]]$batch

one_sam_batch <- names(table(batch))[which(table(batch)==1)]
rm_ind <- which(batch %in% one_sam_batch)
if(length(rm_ind) > 0){
  edata <- edata[, -rm_ind]
  batch <- batch[-rm_ind]
}

na_batch <- which(is.na(batch))
if(length(na_batch)>0){
  edata <- edata[, -na_batch]
  batch <- batch[-na_batch]
}

batchQC(edata, batch=batch, condition=NULL, 
        report_file=paste("batchqc_", names(esets)[setID], ".html", sep=""), 
        report_dir="../../batchQC_res/", 
        report_option_binary="111111111",
        view_report=FALSE, interactive=TRUE, 
        batchqc_output=FALSE, log2cpm_transform=FALSE)

