computeTrains <- function(
  train_matlst
  ## output of pipeline, performance statistics on training set
){
  AUC_trn <- lapply(train_matlst, function(mat){
    return(mat[, "AUC"])
  })
  AUC_trn <- do.call(cbind, AUC_trn)
  colnames(AUC_trn) <- 1:ncol(AUC_trn)
  return(AUC_trn)
}


computeTests <- function(
  test_matlst
  ## output of pipeline, performance statistics on training set
){
  AUC_tst <- lapply(test_matlst, function(mat){
    return(mat[, "AUC"])
  })
  AUC_tst <- do.call(cbind, AUC_tst)
  colnames(AUC_tst) <- 1:ncol(AUC_tst)
  return(AUC_tst)
}


loadTrns <- function(
  expID,
  ## the design of experiment, exp 5_5_5_5_5_5
  datadir
){
  filenames <- dir(datadir)[grep(expID,dir(datadir))]
  
  # baseline
  load(paste(datadir, filenames[grep("FALSE_mean_none_none", filenames)], sep=""))
  baseline_trns <- computeTrains(train_matlst=res_mat_trn)
  rm(res_mat_trn, res_mat_tst)
  
  # batch
  load(paste(datadir, filenames[grep("TRUE_mean_none_none", filenames)], sep=""))
  batch_trns <- computeTrains(train_matlst=res_mat_trn)
  rm(res_mat_trn, res_mat_tst)
  
  # sep_mod
  load(paste(datadir, filenames[grep("TRUE_mean_separate_mod", filenames)], sep=""))
  sep_mod_trns <- computeTrains(train_matlst=res_mat_trn)
  rm(res_mat_trn, res_mat_tst)
  
  # cmb_mod
  load(paste(datadir, filenames[grep("TRUE_mean_combined_mod", filenames)], sep=""))
  cmb_mod_trns <- computeTrains(train_matlst=res_mat_trn)
  rm(res_mat_trn, res_mat_tst)
  
  # sep_hyb
  load(paste(datadir, filenames[grep("TRUE_mean_separate_null", filenames)], sep=""))
  sep_hyb_trns <- computeTrains(train_matlst=res_mat_trn)
  rm(res_mat_trn, res_mat_tst)
  
  # cmb_null
  load(paste(datadir, filenames[grep("TRUE_mean_combined_null", filenames)], sep=""))
  cmb_null_trns <- computeTrains(train_matlst=res_mat_trn)
  rm(res_mat_trn, res_mat_tst)
  
  #rm(baseSets, baseBatchSets, combatSets, batchInfo)
  res <- list(baseline=baseline_trns, batch=batch_trns,
              sep_mod=sep_mod_trns, sep_hyb=sep_hyb_trns,
              cmb_mod=cmb_mod_trns, cmb_null=cmb_null_trns)
  return(res)
}


loadTsts <- function(
  expID,
  ## the design of experiment, exp 5_5_5_5_5_5
  datadir
){
  filenames <- dir(datadir)[grep(expID,dir(datadir))]
  
  # baseline
  load(paste(datadir, filenames[grep("FALSE_mean_none_none", filenames)], sep=""))
  baseline_tsts <- computeTests(test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
  
  # batch
  load(paste(datadir, filenames[grep("TRUE_mean_none_none", filenames)], sep=""))
  batch_tsts <- computeTests(test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
  
  # sep_mod
  load(paste(datadir, filenames[grep("TRUE_mean_separate_mod", filenames)], sep=""))
  sep_mod_tsts <- computeTests(test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
  
  # cmb_mod
  load(paste(datadir, filenames[grep("TRUE_mean_combined_mod", filenames)], sep=""))
  cmb_mod_tsts <- computeTests(test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
  
  # sep_hyb
  load(paste(datadir, filenames[grep("TRUE_mean_separate_null", filenames)], sep=""))
  sep_hyb_tsts <- computeTests(test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
  
  # cmb_null
  load(paste(datadir, filenames[grep("TRUE_mean_combined_null", filenames)], sep=""))
  cmb_null_tsts <- computeTests(test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
  
  #rm(baseSets, baseBatchSets, combatSets, batchInfo)
  res <- list(baseline=baseline_tsts, batch=batch_tsts,
              sep_mod=sep_mod_tsts, sep_hyb=sep_hyb_tsts,
              cmb_mod=cmb_mod_tsts, cmb_null=cmb_null_tsts)
  return(res)
}