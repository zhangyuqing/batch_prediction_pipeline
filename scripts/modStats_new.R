modStats <- function(tp, fp, tn, fn){
  sens <- tp/(tp+fn)
  spec <- tn/(tn+fp)
  ppv <- tp/(tp+fp)
  npv <- tn/(tn+fn)
  accu <- (tp+tn)/(tp+fp+tn+fn)
  f1 <- 2*tp/(2*tp+fp+fn)
  res <- list(accuracy=accu,
              sensitivity=sens,
              specificity=spec,
              NPV=npv,
              PPV=ppv,
              F1=f1)
  return(res)
}


computeStats <- function(pred_vec, true_label, cutoff){
  library(AUC)
  tp <- sum(pred_vec >= cutoff & true_label==1)
  fp <- sum(pred_vec >= cutoff & true_label==0)
  tn <- sum(pred_vec < cutoff & true_label==0)
  fn <- sum(pred_vec < cutoff & true_label==1)
  stats <- append(list(tp=tp,fp=fp,tn=tn,fn=fn),
                  modStats(tp=tp,fp=fp,tn=tn,fn=fn))
  stats <- append(stats,
                  list(AUC=AUC::auc(AUC::roc(pred_vec,as.factor(true_label)))))
}