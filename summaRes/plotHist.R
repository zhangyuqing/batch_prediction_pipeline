hist_ttest <- function(vec_base, vec2, methodname, designname, combatname){
  # two sample t-test
  t_test_res <- t.test(vec_base, vec2, alternative="less")
  pvalue <- round(t_test_res$p.value,4)
  
  # Draw histogram
  png(file=paste(paste("../histograms/", methodname, "/", sep=""),
                 paste(designname, "_", combatname, ".png", sep=""), sep=""),
      width=600, height=600)
  par(xpd=T, mar=par()$mar+c(0,0,0,5))  
  p2 <- hist(vec2, breaks=10, plot=FALSE)
  p1 <- hist(vec_base, breaks=10, plot=FALSE)
  plot(p2, 
       col=rgb(1,0,0, 0.5), xlab="AUC gaps", 
       xlim=c(min(min(vec_base),min(vec2))-(max(vec2)/10), 
              max(max(vec_base),max(vec2))+(max(vec2)/10)),
       ylim=c(0, 20),
       main=paste(designname, "   ", methodname,": P=", pvalue, sep=""))
  plot(p1, 
       col=rgb(0,0,1, 0.5), add=TRUE)
  legend(x=max(max(vec_base), max(vec2))+(max(vec2)/5), 
         y=10, 
         legend=c("baseline", combatname),
         col=c("blue", "red"), pch=c(16,16))
  par(mar=c(5, 4, 4, 2) + 0.1)
  dev.off()
  
  return(t_test_res$p.value)
}



plotHist <- function(gapsList, method_names, expID){
  design_names <- c("balanced", "trn_tst_portion", "batch_portion", "batch_size_portion", "all")
  
  ttest_lst <- matrix(rep(0, times=length(method_names)*(length(gapsList)-1)),
                      nrow=length(method_names))
  rownames(ttest_lst) <- method_names
  colnames(ttest_lst) <- names(gapsList)[-1]
  
  for(method_row in 1:length(method_names)){ 
    # Extract AUC gaps for that specific method
    method_gaps <- list()
    for(i in 1:length(gapsList)){    # i for baseline/batch/combat
      method_gaps[[i]] <- gapsList[[i]][method_row, ]
    }
    names(method_gaps) <- names(gapsList)
    #sapply(method_gaps, length)
    
    # compare baseline gaps with other gaps
    for(i in 2:length(method_gaps)){
      ttest_lst[method_row, i-1] <- hist_ttest(vec_base=method_gaps$baseline, 
                                               vec2=method_gaps[[i]],
                                               methodname=method_names[method_row],
                                               designname=design_names[expID],
                                               combatname=names(method_gaps)[i])
    }
  }
  return(ttest_lst)
}