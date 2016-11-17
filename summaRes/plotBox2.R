plotBox2 <- function(datList, method_names, statname, expID){
  
  output <- list()
  for(method_row in 1:length(method_names)){
    
    #### Extract performance statistics for this method ####
    train_stats <- test_stats <- list()
    for(i in 1:length(datList)){
      # i for baseline/batch/sep-mod/cmb-mod...
      train_stats[[i]] <- test_stats[[i]] <- rep(0, length(datList[[i]]$trn))
      for(j in 1:length(datList[[i]]$trn)){
        # j for 1-20 iterations
        tmp1 <- (datList[[i]]$trn)[[j]]
        train_stats[[i]][j] <- tmp1[method_row, statname]
        tmp2 <- (datList[[i]]$tst)[[j]]
        test_stats[[i]][j]  <- tmp2[method_row, statname]
        rm(tmp1, tmp2)
      }
    }
    names(train_stats) <- names(test_stats) <- c("base", "batch", "sep_mod",
                                                 "sep_hyb", "cmb_mod", "cmp_null")
    
    #### Draw boxplot ####
    png(file=paste(paste("../figures/boxplots/", statname, "/", sep=""), 
                   paste(method_names[method_row], expID, sep="_"), 
                   ".png", sep=""),
        width=600, height=400)
    res <- list(train_stats$base, test_stats$base,
                train_stats$batch, test_stats$batch,
                train_stats$sep_mod, test_stats$sep_mod,
                train_stats$sep_hyb, test_stats$sep_hyb,
                train_stats$cmb_mod, test_stats$cmb_mod,
                train_stats$cmp_null, test_stats$cmp_null)
    names(res) <- NULL
    color.seq <- rep(c("blue","orange"), times=length(datList))
    par(xpd=T,mar=par()$mar+c(0,0,3,5))
    boxplot(res, col=color.seq, ylab=statname, xlab="", boxwex = 0.5, ylim=c(0,1),
            width=rep(0.3,2*length(datList)), cex.axis=1.1, cex.lab=1.1, xaxt="n")
    legend(13, 0.98, legend=c("train","test"),col=c("blue","orange"),pch=c(16,16), cex=1.1,text.width=1.1,bty="n")
    s = seq(from=0, to=1, by=0.002)
    lines(rep(2.5,length(s)),s,type="l",lty=2)
    lines(rep(4.5,length(s)),s,type="l",lty=2)
    lines(rep(6.5,length(s)),s,type="l",lty=2)
    lines(rep(8.5,length(s)),s,type="l",lty=2)
    lines(rep(10.5,length(s)),s,type="l",lty=2)
    
    text(x=1.5,y=1.1,labels=c("Baseline"),cex=0.9)
    text(x=3.5,y=1.1,labels=c("Batch"),cex=0.9)
    text(x=5.5,y=1.1,labels=c("2-step mod"),cex=0.9)
    text(x=7.5,y=1.1,labels=c("2-step hyb"),cex=0.9)
    text(x=9.5,y=1.1,labels=c("1-step mod"),cex=0.9)
    text(x=11.5,y=1.1,labels=c("1-step null"),cex=0.9)
    
    design_names <- c("balanced", "trn/tst %", "batch %", "batch size & %", "all")
    text(x=2,y=1.2,labels=paste(method_names[method_row], design_names[expID], sep=" : "),cex=1.5)
    par(mar=c(5, 4, 4, 2) + 0.1)
    dev.off()
     
    output[[method_row]] <- list(trn=train_stats, tst=test_stats)
  }
  names(output) <- method_names
  return(output)
}