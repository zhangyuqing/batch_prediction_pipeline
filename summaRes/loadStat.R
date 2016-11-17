loadStat <- function(
  expID,
  ## the design of experiment
  datadir
  ## dir of the result files
){
  filenames <- dir(datadir)[grep(expID,dir(datadir))]
  
  # baseline
  load(paste(datadir, filenames[grep("FALSE_mean_none_none", filenames)], sep=""))
  baseline_train <- res_mat_trn
  baseline_test <- res_mat_tst
  rm(res_mat_trn, res_mat_tst)
  
  # batch
  load(paste(datadir, filenames[grep("TRUE_mean_none_none", filenames)], sep=""))
  batch_train <- res_mat_trn
  batch_test <- res_mat_tst
  rm(res_mat_trn, res_mat_tst)
  
  # sep_mod
  load(paste(datadir, filenames[grep("TRUE_mean_separate_mod", filenames)], sep=""))
  sep_mod_train <- res_mat_trn
  sep_mod_test <- res_mat_tst
  rm(res_mat_trn, res_mat_tst)
  
  # cmb_mod
  load(paste(datadir, filenames[grep("TRUE_mean_combined_mod", filenames)], sep=""))
  cmb_mod_train <- res_mat_trn
  cmb_mod_test <- res_mat_tst
  rm(res_mat_trn, res_mat_tst)
  
  # sep_hyb
  load(paste(datadir, filenames[grep("TRUE_mean_separate_null", filenames)], sep=""))
  sep_hyb_train <- res_mat_trn
  sep_hyb_test <- res_mat_tst
  rm(res_mat_trn, res_mat_tst)
  
  # cmb_null
  load(paste(datadir, filenames[grep("TRUE_mean_combined_null", filenames)], sep=""))
  cmb_null_train <- res_mat_trn
  cmb_null_test <- res_mat_tst
  rm(res_mat_trn, res_mat_tst)
  
  #rm(baseSets, baseBatchSets, combatSets, batchInfo)
  res <- list(baseline=list(trn=baseline_train, tst=baseline_test),
              batch=list(trn=batch_train, tst=batch_test),
              sep_mod=list(trn=sep_mod_train, tst=sep_mod_test),
              sep_hyb=list(trn=sep_hyb_train, tst=sep_hyb_test),
              cmb_mod=list(trn=cmb_mod_train, tst=cmb_mod_test),
              cmb_null=list(trn=cmb_null_train, tst=cmb_null_test))
  return(res)
}