baseDat <- function(
  n_trn,  
  # number of individuals in the training set
  pcase_trn, 
  # percentage of cases (positive samples) in training set
  n_tst, 
  # number of individuals in the test set
  pcase_tst, 
  # percentage of cases in test set
  n_genes, 
  # number of genes
  n_biomarker, 
  # number of signatures / biomarkers (diffentially expressed between case / control)
  on_mean, 
  # mean expression when genes are activated
  off_mean, 
  # mean expression when genes are "off"
  on_var, 
  # variance when genes are activated
  off_var 
  # variance when genes are "off"
){
  n_trn_case <- floor(n_trn * pcase_trn) # number of cases in training set
  n_trn_control <- n_trn - n_trn_case # number of controls in training set
  n_tst_case <- floor(n_tst * pcase_tst) # number of cases in the test set
  n_tst_control <- n_tst - n_tst_case # number of controls in the test set
  
  trn_set <- matrix(0, nrow=n_genes, ncol=n_trn)
  tst_set <- matrix(0, nrow=n_genes, ncol=n_tst)
  y_trn <- c(rep(1, n_trn_case), rep(0, n_trn_control)) 
  y_tst <- c(rep(1, n_tst_case), rep(0, n_tst_control))
  ## 1---case, 0---control
  
  for(i in 1:n_genes){
    if(i<=n_biomarker){
      # train set
      trn_set[i, which(y_trn==1)] <- rnorm(n_trn_case, mean=on_mean, sd=sqrt(on_var)) # case
      trn_set[i, which(y_trn==0)] <- rnorm(n_trn_control, mean=off_mean, sd=sqrt(off_var)) # control
      # test set
      tst_set[i, which(y_tst==1)] <- rnorm(n_tst_case, mean=on_mean, sd=sqrt(on_var)) # case
      tst_set[i, which(y_tst==0)] <- rnorm(n_tst_control, mean=off_mean, sd=sqrt(off_var)) # control
    }else{
      # train set
      trn_set[i, ] <- rnorm(n_trn, mean=off_mean, sd=sqrt(off_var))
      # test set
      tst_set[i, ] <- rnorm(n_tst, mean=off_mean, sd=sqrt(off_var))
    }
  }
  
  res <- list(trn_x=trn_set, trn_y=y_trn,
              tst_x=tst_set, tst_y=y_tst,
              n_trn_case=n_trn_case, n_trn_control=n_trn_control, 
              n_tst_case=n_tst_case, n_tst_control=n_tst_control)
  return(res)
}