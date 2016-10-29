batchDat <- function(
  n_batch_train,
  # number of batches in training set
  n_batch_test,
  # number of batches in test set
  cases_batch_train,
  # vector, number of cases in each batch in training set
  controls_batch_train,
  # vector, number of controls in each batch in training set
  cases_batch_test,
  # vector, number of cases in each batch in test set
  controls_batch_test,
  # vector, number of controls in each batch in test set
  batch_meanvar_arg,
  # batches different in mean or variance?
  n_genes
  # number of genes
){
  ## generate index of batches for samples
  # number of samples in each batch in training set
  nbatches_trn <- cases_batch_train + controls_batch_train 
  # number of samples in each batch in test set
  nbatches_tst <- cases_batch_test + controls_batch_test 
  # number of samples in each batch (across both train and test set)
  n.batches <- c(nbatches_trn, nbatches_tst) 
  
  # list of index: which samples belong to which batch
  batch_ind_trn <- batch_ind_tst <- list()
  batch_train <- batch_test <- c()
  if(n_batch_train > 0){
    tmp_case <- tmp_ctrl <- c()
    for(i in 1:n_batch_train){
      tmp_case <- c(tmp_case, rep(i, cases_batch_train[i]))
      tmp_ctrl <- c(tmp_ctrl, rep(i, controls_batch_train[i]))
    }
    batch_train <- c(tmp_case, tmp_ctrl)
    for(i in 1:n_batch_train){
      batch_ind_trn[[i]] <- which(batch_train==i)
    }
    names(batch_ind_trn) <- 1:n_batch_train
  }
  if(n_batch_test > 0){
    tmp_case <- tmp_ctrl <- c()
    for(i in 1:n_batch_test){
      tmp_case <- c(tmp_case, rep(i, cases_batch_test[i]))
      tmp_ctrl <- c(tmp_ctrl, rep(i, controls_batch_test[i]))
    }
    batch_test <- c(tmp_case, tmp_ctrl)
    for(i in 1:n_batch_test){
      batch_ind_tst[[i]] <- which(batch_test==i)
    }
    names(batch_ind_tst) <- 1:n_batch_test
  }
  
  
  ## batch mean and variances
  batch_par <- list()
  if(batch_meanvar_arg=="mean"){
    mu_seq <- c(-2, 1, 3, -1, 4, 2, -3, -4, 1.5, -2.5) # 10 batches maximum
    for(i in 1:length(n.batches)){
      batch_par[[i]] <- c(mu_seq[i], 0.1, 11, 1) # mean, sd of gaussian; alpha, beta for InvGamma
    }
  }else if(batch_meanvar_arg=="var"){
    alpha_beta_seq <- list(c(102, 101), c(1602, 6404), c(8102, 72909), c(25602, 409616)) # 4 batches maximum
    for(i in 1:length(n.batches)){
      batch_par[[i]] <- c(0, 0.1, alpha_beta_seq[[i]]) # mean, sd of gaussian; alpha, beta for InvGamma
    }
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
              batch_train=batch_train, batch_test=batch_test,
              n.batches=n.batches, batch_par=batch_par)
  return(res)
}