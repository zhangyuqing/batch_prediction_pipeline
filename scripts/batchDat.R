batchDat <- function(
  ncase_trn,
  # number of cases in the training set
  nctrl_trn,
  # number of controls in the training set
  ncase_tst,
  # number of cases in the test set
  nctrl_tst,
  # number of controls in the test set
  pcase_batch1,
  # Among cases in the training set, the percentage within batch 1
  pctrl_batch1,
  # Among controls in the training set, the percentage within batch 1
  pcase_batch3,
  # Among cases in the test set, the percentage within batch 3
  pctrl_batch3,
  # Among controls in the test set, the percentage within batch 3
  batch_meanvar_arg,
  # batches different in mean or variance?
  n_genes
  # number of genes
){
  ## number of samples in batches and their indices
  batch1_case <- floor(ncase_trn * pcase_batch1)
  batch1_control <- floor(nctrl_trn * pctrl_batch1)
  
  batch2_case <- ncase_trn - batch1_case
  batch2_control <- nctrl_trn - batch1_control
  
  batch3_case <- floor(ncase_tst * pcase_batch3)
  batch3_control <- floor(nctrl_tst * pctrl_batch3)
  
  batch4_case <- ncase_tst - batch3_case
  batch4_control <- nctrl_tst - batch3_control
  
  nbatches_trn <- c(batch1_case+batch1_control, batch2_case+batch2_control) ## batch 1,2 in train set
  nbatches_tst <- c(batch3_case+batch3_control, batch4_case+batch4_control) ## batch 3,4 in test set
  n.batches <- c(nbatches_trn,nbatches_tst) ## how many individuals there are in batch 1-4
  
  batch1_idx <- c(1:batch1_case, ncase_trn+(1:batch1_control))
  batch2_idx <- setdiff(1:(ncase_trn+nctrl_trn), batch1_idx)
  batch3_idx <- c(1:batch3_case, ncase_tst+(1:batch3_control))
  batch4_idx <- setdiff(1:(ncase_tst+nctrl_tst), batch3_idx)
  batch_ind_trn <- list(batch1_idx, batch2_idx) 
  batch_ind_tst <- list(batch3_idx, batch4_idx) 
  
  ## batch mean and variances
  batch_par <- list()
  if(batch_meanvar_arg=="mean"){
    # mean
    batch_par[[1]] <- c(-2, 0.1, 11, 1) # mean, sd of gaussian; alpha, beta for InvGamma
    batch_par[[2]] <- c(1, 0.1, 11, 1)
    batch_par[[3]] <- c(3, 0.1, 11, 1)
    batch_par[[4]] <- c(-1, 0.1, 11, 1)
  }else if(batch_meanvar_arg=="var"){
    # var
    batch_par[[1]] <- c(0, 0.1, 102, 101) # mean, sd of gaussian; alpha, beta for InvGamma
    batch_par[[2]] <- c(0, 0.1, 1602, 6404)
    batch_par[[3]] <- c(0, 0.1, 8102, 72909)
    batch_par[[4]] <- c(0, 0.1, 25602, 409616)
  }
  
  
  ## simulate parameters from hyper-pars
  library(MCMCpack)
  gamma <- delta2 <- list()
  for(i in 1:length(n.batches)){
    gamma[[i]] <- rnorm(n_genes, mean=batch_par[[i]][1], sd=batch_par[[i]][2])
    delta2[[i]] <- rinvgamma(n_genes, shape=batch_par[[i]][3], scale=batch_par[[i]][4])
  }
  
  ## simulate batch matrices
  batches <- list()
  for(i in 1:length(n.batches)){
    batches[[i]] <- matrix(0, nrow=n_genes, ncol=n.batches[i])
    for(g in 1:n_genes){
      batches[[i]][g, ] <- rnorm(n.batches[i], mean=gamma[[i]][g], sd=sqrt(delta2[[i]][g]))
    }
  }
  
  res <- list(batchMat=batches,
              batch_ind_trn=batch_ind_trn, batch_ind_tst=batch_ind_tst,
              n.batches=n.batches, batch_par=batch_par)
  return(res)
}