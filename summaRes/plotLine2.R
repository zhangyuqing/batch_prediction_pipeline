plotLine2 <- function(datmat, outdir, design_name, ylab_text){
  library(RColorBrewer)
  n <- nrow(datmat)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  colorseq = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  colorseq = colorseq[-4]
  
  png(file=paste(outdir, design_name, ".png", sep=""),
      width=800, height=600)
  par(xpd=T, mar=par()$mar+c(1,1,1,12))  
  plot(as.numeric(datmat[1, ]), type="b", col=colorseq[1],
       ylim=c(min(datmat), max(datmat)),
       ylab=ylab_text, xlab="", 
       main=design_name, xaxt="n",
       cex.axis=1.3, cex.lab=1.3, cex.main=1.3)
  axis(1, at=1:ncol(datmat), labels=colnames(datmat), cex.axis=1.3)
  for(i in 2:nrow(datmat)){
    lines(as.numeric(datmat[i, ]), type="b", col=colorseq[i])
  }
  legend(x=5.2, y=mean(as.matrix(datmat)), legend=rownames(datmat),
         col=colorseq[1:nrow(datmat)], lty=rep(1,n), lwd=rep(2.5,n),
         cex=1.3)
  par(mar=c(5, 4, 4, 2) + 0.1)
  dev.off() 
}
