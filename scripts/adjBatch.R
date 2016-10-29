adjBatch <- function(
  datLst, 
  ## list of the previous output which contains the training and test x and y
  batch_train,
  ## batch index in training set
  batch_test,
  ## batch index in test set
  sep_cmb,
  ## separate or combined adjustment (2-step or 1-step)
  combat_mod
  ## full or null model / design mat to use in ComBat
){
  library(sva)
  mod_train <- model.matrix(~datLst$trn_y)
  mod_test <- model.matrix(~datLst$tst_y)
  
  if((!is.null(batch_train)) & (!is.null(batch_test))){ #### both training and test set contain batches
    if(sep_cmb=="separate"){
      if(combat_mod=="mod"){
        # step 1
        trn_set <- ComBat(datLst$trn_x, batch=batch_train, mod=mod_train)
        tst_set <- ComBat(datLst$tst_x, batch=batch_test, mod=mod_test)
        
        # step 2
        batch_idx <- c(rep(1, ncol(trn_set)), rep(2,ncol(tst_set)))
        mod <- rbind(mod_train, mod_test); rownames(mod) <- 1:nrow(mod)
        combined_set <- ComBat(cbind(trn_set,tst_set), batch=batch_idx, mod=mod)
        datLst$trn_x <- combined_set[, 1:ncol(trn_set)]
        datLst$tst_x <- combined_set[, (ncol(trn_set)+1):(ncol(trn_set)+ncol(tst_set))]
      }else if(combat_mod=="null"){
        # step 1
        trn_set <- ComBat(datLst$trn_x, batch=batch_train, mod=mod_train)
        tst_set <- ComBat(datLst$tst_x, batch=batch_test, mod=NULL)
        
        # step 2
        batch_idx <- c(rep(1, ncol(trn_set)), rep(2,ncol(tst_set)))
        combined_set <- ComBat(cbind(trn_set,tst_set), batch=batch_idx, mod=NULL)
        datLst$trn_x <- combined_set[, 1:ncol(trn_set)]
        datLst$tst_x <- combined_set[, (ncol(trn_set)+1):(ncol(trn_set)+ncol(tst_set))]
      }
    }else if(sep_cmb=="combined"){
      if(combat_mod=="mod"){
        mod <- rbind(mod_train, mod_test); rownames(mod) <- 1:nrow(mod)
        batch_idx <- c(batch_train, batch_test)
        combined_set <- ComBat(cbind(datLst$trn_x, datLst$tst_x), batch=batch_idx, mod=mod)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[,(ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }else if(combat_mod=="null"){
        batch_idx <- c(batch_train, batch_test)
        combined_set <- ComBat(cbind(datLst$trn_x, datLst$tst_x), batch=batch_idx, mod=NULL)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }
    }
  }else if(is.null(batch_test)){ #### only the training set contain batches, no batch in test set
    if(sep_cmb=="separate"){
      if(combat_mod=="mod"){
        # step 1
        trn_set <- ComBat(datLst$trn_x, batch=batch_train, mod=mod_train)
        
        # step 2
        batch_idx <- c(rep(1, ncol(datLst$trn_x)), rep(2, ncol(datLst$tst_x)))
        mod <- rbind(mod_train, mod_test); rownames(mod) <- 1:nrow(mod)
        combined_set <- ComBat(cbind(trn_set, datLst$tst_x), batch=batch_idx, mod=mod)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }else if(combat_mod=="null"){
        # step 1
        trn_set <- ComBat(datLst$trn_x, batch=batch_train, mod=mod_train)
        
        # step 2
        batch_idx <- c(rep(1, ncol(datLst$trn_x)), rep(2, ncol(datLst$tst_x)))
        combined_set <- ComBat(cbind(trn_set, datLst$tst_x), batch=batch_idx, mod=NULL)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }
    }else if(sep_cmb=="combined"){
      if(combat_mod=="mod"){
        mod <- rbind(mod_train, mod_test); rownames(mod) <- 1:nrow(mod)
        batch_idx <- c(batch_train, rep((max(batch_train)+1), ncol(datLst$tst_x)))
        combined_set <- ComBat(cbind(datLst$trn_x, datLst$tst_x), batch=batch_idx, mod=mod)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }else if(combat_mod=="null"){
        batch_idx <- c(batch_train, rep((max(batch_train)+1), ncol(datLst$tst_x)))
        combined_set <- ComBat(cbind(datLst$trn_x, datLst$tst_x), batch=batch_idx, mod=NULL)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }
    }
  }else if(is.null(batch_train)){ # only the test set contain batches, no batch in training set
    if(sep_cmb=="separate"){
      if(combat_mod=="mod"){
        # step 1
        tst_set <- ComBat(datLst$tst_x, batch=batch_test, mod=mod_test)
        
        # step 2
        batch_idx <- c(rep(1, ncol(datLst$trn_x)), rep(2, ncol(datLst$tst_x)))
        mod <- rbind(mod_train, mod_test); rownames(mod) <- 1:nrow(mod)
        combined_set <- ComBat(cbind(datLst$trn_x, tst_set), batch=batch_idx, mod=mod)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }else if(combat_mod=="null"){
        # step 1
        tst_set <- ComBat(datLst$tst_x, batch=batch_test, mod=NULL)
        
        # step 2
        batch_idx <- c(rep(1, ncol(datLst$trn_x)), rep(2, ncol(datLst$tst_x)))
        combined_set <- ComBat(cbind(datLst$trn_x, tst_set), batch=batch_idx, mod=NULL)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }
    }else if(sep_cmb=="combined"){
      if(combat_mod=="mod"){
        mod <- rbind(mod_train, mod_test); rownames(mod) <- 1:nrow(mod)
        batch_idx <- c(rep((max(batch_test)+1), ncol(datLst$trn_x)), batch_test)
        combined_set <- ComBat(cbind(datLst$trn_x, datLst$tst_x), batch=batch_idx, mod=mod)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }else if(combat_mod=="null"){
        batch_idx <- c(rep((max(batch_test)+1), ncol(datLst$trn_x)), batch_test)
        combined_set <- ComBat(cbind(datLst$trn_x, datLst$tst_x), batch=batch_idx, mod=NULL)
        datLst$trn_x <- combined_set[, 1:ncol(datLst$trn_x)]
        datLst$tst_x <- combined_set[, (ncol(datLst$trn_x)+1):(ncol(datLst$trn_x)+ncol(datLst$tst_x))]
      }
    }
  }
  return(datLst)
}