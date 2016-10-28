loadCorr <- function(
  expID
  ## the design of experiment, exp 5_5_5_5_5_5
){
  filenames <- dir()[grep(expID,dir())]
  
  # baseline
  load(filenames[grep("FALSE_mean_none_none", filenames)])
  baseline_train <- lapply(combatSets, function(set){
    return(cor(set$trn_x))
  })
  baseline_test <- lapply(combatSets, function(set){
    return(cor(set$tst_x))
  })
  rm(combatSets)
  
  # batch
  load(filenames[grep("TRUE_mean_none_none", filenames)])
  batch_train <- lapply(combatSets, function(set){
    return(cor(set$trn_x))
  })
  batch_test <- lapply(combatSets, function(set){
    return(cor(set$tst_x))
  })
  rm(combatSets)
  
  # sep_mod
  load(filenames[grep("TRUE_mean_separate_mod", filenames)])
  sep_mod_train <- lapply(combatSets, function(set){
    return(cor(set$trn_x))
  })
  sep_mod_test <- lapply(combatSets, function(set){
    return(cor(set$tst_x))
  })
  rm(combatSets)
  
  # cmb_mod
  load(filenames[grep("TRUE_mean_combined_mod", filenames)])
  cmb_mod_train <- lapply(combatSets, function(set){
    return(cor(set$trn_x))
  })
  cmb_mod_test <- lapply(combatSets, function(set){
    return(cor(set$tst_x))
  })
  rm(combatSets)
  
  # sep_hyb
  load(filenames[grep("TRUE_mean_separate_null", filenames)])
  sep_hyb_train <- lapply(combatSets, function(set){
    return(cor(set$trn_x))
  })
  sep_hyb_test <- lapply(combatSets, function(set){
    return(cor(set$tst_x))
  })
  rm(combatSets)
  
  # cmb_null
  load(filenames[grep("TRUE_mean_combined_null", filenames)])
  cmb_null_train <- lapply(combatSets, function(set){
    return(cor(set$trn_x))
  })
  cmb_null_test <- lapply(combatSets, function(set){
    return(cor(set$tst_x))
  })
  rm(combatSets)
  
  rm(baseSets, baseBatchSets, batchInfo, res_mat_trn, res_mat_tst)
  res <- list(baseline=list(trn=baseline_train, tst=baseline_test),
              batch=list(trn=batch_train, tst=batch_test),
              sep_mod=list(trn=sep_mod_train, tst=sep_mod_test),
              sep_hyb=list(trn=sep_hyb_train, tst=sep_hyb_test),
              cmb_mod=list(trn=cmb_mod_train, tst=cmb_mod_test),
              cmb_null=list(trn=cmb_null_train, tst=cmb_null_test))
  return(res)
}