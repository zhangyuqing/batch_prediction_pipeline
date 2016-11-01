rm(list=ls())
dataname <- "signature"
testname <- "ICBP"
datadir <- "/restricted/projectnb/pathsig/20150929_bild_paper_new_ASSIGN/data/"
traindir <- "signatures/"
testdir <- "test_data/"
#datadir <- "C:/Users/zhang/Documents/Work/Evan/oncogenic_signature_ABild/"


#### training set ####
batch1 <- read.table(paste(datadir, traindir, "GFP18_AKT_BAD_HER2_IGF1R_RAF_ERK.tpmlog", sep=""), as.is=TRUE)
batch2 <- read.table(paste(datadir, traindir, "GFP30_KRAS-GV_KRAS-QH_KRAS-WT_tpmlog.txt", sep=""), as.is=TRUE)
batch3 <- read.table(paste(datadir, traindir, "18_GFP_EGFR_TPMlog2.txt", sep=""), as.is=TRUE)

## clean of training files
batch1 <- batch1[, -grep("ERK", colnames(batch1))]
batch2 <- batch2[match(rownames(batch1), rownames(batch2)), ]
sum(rownames(batch1) != rownames(batch2))
sum(rownames(batch1) != rownames(batch3))

train_set <- cbind(batch1, batch2, batch3)
batch_train <- c(rep(1, ncol(batch1)), rep(2, ncol(batch2)), rep(3, ncol(batch3)))
cond1 <- rep(0, ncol(batch1)); cond2 <- rep(0, ncol(batch2)); cond3 <- rep(0, ncol(batch3))
# condition in batch 1
cond1[grep("GFP", colnames(batch1))] <- 0
cond1[grep("AKT", colnames(batch1))] <- 1
cond1[grep("BAD", colnames(batch1))] <- 2
cond1[grep("IGF1R", colnames(batch1))] <- 3
cond1[grep("RAF", colnames(batch1))] <- 4
cond1[grep("HER2", colnames(batch1))] <- 5
# condition in batch 2
cond2[grep("GFP", colnames(batch2))] <- 0
cond2[grep("KRAS_WT", colnames(batch2))] <- 6
cond2[grep("KRAS_GV", colnames(batch2))] <- 7
cond2[grep("KRAS_QH", colnames(batch2))] <- 8
# condition in batch 3
cond3[grep("Control", colnames(batch3))] <- 0
cond3[grep("EGFR", colnames(batch3))] <- 9
condition_train <- c(cond1, cond2, cond3)


#### Test set ####
TCGA_file <- paste(datadir, testdir, "PANCAN24_BRCA_1119_TPMlog2.txt", sep="")  
ICBP_file <- paste(datadir, testdir, "icbp_Rsubread_tpmlog.txt", sep="")
if(testname=="TCGA"){
  test_set <- read.table(TCGA_file, as.is=TRUE)
  test_set <- test_set[match(rownames(train_set), rownames(test_set)), ]
  condition_test <- read.csv("/restricted/projectnb/pathsig/20150929_bild_paper_new_ASSIGN/data/Datasets/TCGA-BRCA-RBN.csv")
}else if(testname=="ICBP"){
  test_set <- read.delim(ICBP_file, as.is=TRUE)
  rownames(test_set) <- test_set[, 1]
  test_set <- test_set[, -1]
  condition_test <- read.table("/restricted/projectnb/pathsig/20150929_bild_paper_new_ASSIGN/data/correlation_data/proteomics.txt")
  
  sample_overlap <- intersect(rownames(condition_test), colnames(test_set))
  test_set <- test_set[, match(sample_overlap, colnames(test_set))]
  condition_test <- condition_test[match(sample_overlap, rownames(condition_test)), ]
}else{
  print("ERROR: Wrong test file!")
  quit(save = "no", status = 1, runLast = FALSE)
}  
batch_test <- c()


#### How to use condition ??? ####



#### Save Data ####
n_trn_case <- length(which(condition_train==1))
n_trn_control <- length(which(condition_train==0))
n_tst_case <- length(which(condition_test==1))
n_tst_control <- length(which(condition_test==0))
baseBatch_Lst <- list(trn_x=train_set, trn_y=condition_train, batch_train=batch_train,
                      tst_x=test_set, tst_y=condition_test, batch_test=batch_test,
                      n_trn_case=n_trn_case, n_trn_control=n_trn_control, 
                      n_tst_case=n_tst_case, n_tst_control=n_tst_control)
save(baseBatch_Lst, file=paste(dataname, '_', testname, ".RData", sep=""))