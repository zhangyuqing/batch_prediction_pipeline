#### baseLst ####
names(baseLst)

baseLst$n_trn_case
baseLst$n_trn_control
baseLst$n_tst_case
baseLst$n_tst_control

baseLst$trn_y
length(baseLst$trn_y)
sum(baseLst$trn_y)
baseLst$tst_y
length(baseLst$tst_y)
sum(baseLst$tst_y)

# check-heatmap for trn_x and tst_x


#### batchLst ####
names(batchLst)
batchLst$batch_par
batchLst$n.batches
sapply(batchLst$batchMat, dim)

batchLst$batch_ind_trn
which(batchLst$batch_train==1)

batchLst$batch_ind_tst
which(batchLst$batch_test==1)

which(is.na(batchLst$batchMat[[1]]))
which(batchLst$batchMat[[1]]==0)

n_batch_train+n_batch_test==length(batchLst$n.batches)

# if there are batches in both train and test
batchLst$batch_test
which(batchLst$batch_test==5)


#### For not adding batch to baseline ####
sum(baseLst$trn_x!=baseBatch_Lst$trn_x)
sum(baseLst$tst_x!=baseBatch_Lst$tst_x)
sum(baseLst$trn_y!=baseBatch_Lst$trn_y)
sum(baseLst$tst_y!=baseBatch_Lst$tst_y)

sum(baseBatch_Lst$trn_x!=combatLst$trn_x)
sum(baseBatch_Lst$tst_x!=combatLst$tst_x)
sum(baseBatch_Lst$trn_y!=combatLst$trn_y)
sum(baseBatch_Lst$tst_y!=combatLst$tst_y)

sum(baseLst$trn_x!=combatLst$trn_x)
sum(baseLst$tst_x!=combatLst$tst_x)
sum(baseLst$trn_y!=combatLst$trn_y)
sum(baseLst$tst_y!=combatLst$tst_y)
