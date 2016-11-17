computeGaps <- function(
  train_matlst, 
  ## output of pipeline, performance statistics on training set
  test_matlst
  ## output of pipeline, performance statistics on test set
){
  AUC_trn <- lapply(train_matlst, function(mat){
    return(mat[, "AUC"])
  })
  AUC_tst <- lapply(test_matlst, function(mat){
    return(mat[, "AUC"])
  })
  AUC_gaps <- matrix(0, nrow=length(AUC_trn[[1]]), ncol=length(train_matlst))
  for(i in 1:length(train_matlst)){ ## i for iterations
    AUC_gaps[, i] <- AUC_trn[[i]] - AUC_tst[[i]]
  }
  rownames(AUC_gaps) <- rownames(train_matlst[[1]])
  colnames(AUC_gaps) <- 1:ncol(AUC_gaps)
  return(AUC_gaps)
}



loadGaps <- function(
  expID,
  ## the design of experiment, exp 5_5_5_5_5_5
  datadir
){
  filenames <- dir(datadir)[grep(expID,dir(datadir))]
    
  # baseline
  load(paste(datadir, filenames[grep("FALSE_mean_none_none", filenames)], sep=""))
  baseline_gaps <- computeGaps(train_matlst=res_mat_trn, test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
    
  # batch
  load(paste(datadir, filenames[grep("TRUE_mean_none_none", filenames)], sep=""))
  batch_gaps <- computeGaps(train_matlst=res_mat_trn, test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
    
  # sep_mod
  load(paste(datadir, filenames[grep("TRUE_mean_separate_mod", filenames)], sep=""))
  sep_mod_gaps <- computeGaps(train_matlst=res_mat_trn, test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
    
  # cmb_mod
  load(paste(datadir, filenames[grep("TRUE_mean_combined_mod", filenames)], sep=""))
  cmb_mod_gaps <- computeGaps(train_matlst=res_mat_trn, test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
    
  # sep_hyb
  load(paste(datadir, filenames[grep("TRUE_mean_separate_null", filenames)], sep=""))
  sep_hyb_gaps <- computeGaps(train_matlst=res_mat_trn, test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
    
  # cmb_null
  load(paste(datadir, filenames[grep("TRUE_mean_combined_null", filenames)], sep=""))
  cmb_null_gaps <- computeGaps(train_matlst=res_mat_trn, test_matlst=res_mat_tst)
  rm(res_mat_trn, res_mat_tst)
    
  #rm(baseSets, baseBatchSets, combatSets, batchInfo)
  res <- list(baseline=baseline_gaps, batch=batch_gaps,
              sep_mod=sep_mod_gaps, sep_hyb=sep_hyb_gaps,
              cmb_mod=cmb_mod_gaps, cmb_null=cmb_null_gaps)
  return(res)
}