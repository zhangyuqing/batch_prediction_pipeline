plotLine <- function(design_gaps_List){
  
  for(datID in 1:length(design_gaps_List[[1]])){
    ## one plot for baseline/batch/combat
    mean_AUCgaps <- matrix(0, 
                           nrow=nrow(design_gaps_List[[1]][[1]]), 
                           ncol=length(design_gaps_List)) # 9 method by 5 design
    for(designID in 1:length(design_gaps_List)){
      gaps_mat <- design_gaps_List[[designID]][[datID]]
      mean_AUCgaps[, designID] <- apply(gaps_mat, 1, mean)
    }
    rownames(mean_AUCgaps) <- rownames(design_gaps_List[[1]][[1]]) # names of method
    colnames(mean_AUCgaps) <- names(design_gaps_List) # names of design
    
    ## plot
    library(RColorBrewer)
    n <- nrow(mean_AUCgaps)
    qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
    colorseq = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
    png(file=paste("../lineChart/", names(design_gaps_List[[1]])[datID], ".png", sep=""),
        width=800, height=600)
    par(xpd=T, mar=par()$mar+c(1,1,1,12))  
    plot(mean_AUCgaps[1, ], type="b", col=colorseq[1],
         ylim=c(min(mean_AUCgaps), max(mean_AUCgaps)),
         ylab="Average AUC gaps (overfitting)", xlab="", 
         main=names(design_gaps_List[[1]])[datID], xaxt="n",
         cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
    axis(1, at=1:ncol(mean_AUCgaps), labels=colnames(mean_AUCgaps), cex.axis=1.3)
    for(i in 2:nrow(mean_AUCgaps)){
      lines(mean_AUCgaps[i, ], type="b", col=colorseq[i])
    }
    legend(x=5.2, y=mean(mean_AUCgaps)+0.1, legend=rownames(mean_AUCgaps),
           col=colorseq[1:nrow(mean_AUCgaps)], lty=rep(1,n), lwd=rep(2.5,n),
           cex=1.3)
    par(mar=c(5, 4, 4, 2) + 0.1)
    dev.off() 
  }
  
}