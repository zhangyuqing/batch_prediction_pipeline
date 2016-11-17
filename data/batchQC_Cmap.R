rm(list=ls())
# setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/batch_prediction_pipeline/")
# load("C:/Users/zhang/Documents/Work/Evan/predictive-models/111116-13-refine-pipeline/cmap_whole.RData")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")
load("data/cmap_whole.RData")


## BatchQC on whole cmap dataset 
library(BatchQC)

## ERROR again with one-sample batch(es)
one_sam_batch <- names(table(cmap_batch))[which(table(cmap_batch)==1)]
rm_ind <- which(cmap_batch %in% one_sam_batch)
if(length(rm_ind) > 0){
  cmap_mat <- cmap_mat[, -rm_ind]
  cmap_batch <- cmap_batch[-rm_ind]
  cmap_condition <- cmap_condition[-rm_ind]
}

batchQC(cmap_mat, batch=cmap_batch, condition=cmap_condition, 
        report_file="batchqc_cmap_Whole.html", 
        report_dir="../batchQC_res/", 
        report_option_binary="111111111",
        view_report=FALSE, interactive=TRUE, 
        batchqc_output=FALSE, log2cpm_transform=FALSE)



## BatchQC on VPA set
VPA_case_mat <- cmap_mat[, VPA_case_ind]
VPA_ctrl_mat <- cmap_mat[, VPA_ctrl_ind]
VPA_mat <- cbind(VPA_case_mat, VPA_ctrl_mat)
VPA_condition <- c(rep(1, ncol(VPA_case_mat)), rep(0, ncol(VPA_ctrl_mat)))
VPA_batch <- cmap_batch[c(VPA_case_ind, VPA_ctrl_ind)]

one_sam_batch <- names(table(VPA_batch))[which(table(VPA_batch)==1)]
rm_ind <- which(VPA_batch %in% one_sam_batch)
if(length(rm_ind) > 0){
  VPA_mat <- VPA_mat[, -rm_ind]
  VPA_batch <- VPA_batch[-rm_ind]
  VPA_condition <- VPA_condition[-rm_ind]
}

batchQC(VPA_mat, batch=VPA_batch, condition=VPA_condition, 
        report_file="batchqc_cmap_VPA.html", 
        report_dir="../batchQC_res/", 
        report_option_binary="111111111",
        view_report=FALSE, interactive=TRUE, 
        batchqc_output=FALSE, log2cpm_transform=FALSE)



## BatchQC on SAHA set
SAHA_case_mat <- cmap_mat[, SAHA_case_ind]
SAHA_ctrl_mat <- cmap_mat[, SAHA_ctrl_ind]
SAHA_mat <- cbind(SAHA_case_mat, SAHA_ctrl_mat)
SAHA_condition <- c(rep(1, ncol(SAHA_case_mat)), rep(0, ncol(SAHA_ctrl_mat)))
SAHA_batch <- cmap_batch[c(SAHA_case_ind, SAHA_ctrl_ind)]

one_sam_batch <- names(table(SAHA_batch))[which(table(SAHA_batch)==1)]
rm_ind <- which(SAHA_batch %in% one_sam_batch)
if(length(rm_ind) > 0){
  SAHA_mat <- SAHA_mat[, -rm_ind]
  SAHA_batch <- SAHA_batch[-rm_ind]
  SAHA_condition <- SAHA_condition[-rm_ind]
}

batchQC(SAHA_mat, batch=SAHA_batch, condition=SAHA_condition, 
        report_file="batchqc_cmap_SAHA.html", 
        report_dir="../batchQC_res/", 
        report_option_binary="111111111",
        view_report=FALSE, interactive=TRUE, 
        batchqc_output=FALSE, log2cpm_transform=FALSE)



## BatchQC on TSA set
TSA_case_mat <- cmap_mat[, TSA_case_ind]
TSA_ctrl_mat <- cmap_mat[, TSA_ctrl_ind]
TSA_mat <- cbind(TSA_case_mat, TSA_ctrl_mat)
TSA_condition <- c(rep(1, ncol(TSA_case_mat)), rep(0, ncol(TSA_ctrl_mat)))
TSA_batch <- cmap_batch[c(TSA_case_ind, TSA_ctrl_ind)]

one_sam_batch <- names(table(TSA_batch))[which(table(TSA_batch)==1)]
rm_ind <- which(TSA_batch %in% one_sam_batch)
if(length(rm_ind) > 0){
  TSA_mat <- TSA_mat[, -rm_ind]
  TSA_batch <- TSA_batch[-rm_ind]
  TSA_condition <- TSA_condition[-rm_ind]
}

batchQC(TSA_mat, batch=TSA_batch, condition=TSA_condition, 
        report_file="batchqc_cmap_TSA.html", 
        report_dir="../batchQC_res/", 
        report_option_binary="111111111",
        view_report=FALSE, interactive=TRUE, 
        batchqc_output=FALSE, log2cpm_transform=FALSE)
