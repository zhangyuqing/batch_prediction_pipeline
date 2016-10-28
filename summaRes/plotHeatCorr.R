meanMat <- function(matList){
  sumMat <- matrix(rep(0, nrow(matList[[1]])*ncol(matList[[1]])), 
                   nrow=nrow(matList[[1]]))
  count.valid <- matrix(rep(length(matList), nrow(matList[[1]])*ncol(matList[[1]])),
                        nrow=nrow(matList[[1]]))
  for(i in 1:length(matList)){
    for(a in 1:nrow(matList[[i]])){
      for(b in 1:ncol(matList[[i]])){
        if(is.na(matList[[i]][a, b])){
          print("NA value detected")
          count.valid[a, b] <- count.valid[a, b] - 1
          matList[[i]][a, b] <- 0
        }
      }
    }
    sumMat <- sumMat + matList[[i]]
  }
  sumMat <- sumMat / count.valid
  return(sumMat)
}



plotHeatCorr <- function(datList, expID){
  
  #### Average correlation matrices over 20 simulations ####
  train_corr <- test_corr <- list()
  for(i in 1:length(datList)){
    train_corr[[i]] <- meanMat(datList[[i]]$trn)
    test_corr[[i]] <- meanMat(datList[[i]]$tst)
  }
  
  #### Draw Heatmap ####
  breaks_seq <- c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3,
                  0.35, 0.4, 0.45, 0.5, 0.6, 0.8, 1)
  #breaks_seq <- seq(from=0, to=1, by=0.05)
  pal<- colorRampPalette(c("white", "yellow", "red"))(n = length(breaks_seq)-1)
  design_names <- c("balanced", "trn_tst_portion", "batch_portion", "batch_size_portion", "all")
  combat_names <- names(datList)
  library(gplots)
  library(ClassDiscovery)
  
  for(i in 1:length(datList)){    # i for baseline/batch/sep-mod/cmb-mod...
    # train
    png(file=paste("../heatmaps/", paste(design_names[expID], 
                                         combat_names[i], 
                                         "train", sep="_"), 
                   ".png", sep=""),
        width=900, height=700)
    heatmap.2(train_corr[[i]],
              scale='none',dendrogram='none',Rowv=NA,Colv=NA,
              trace='none',col=pal,
              breaks=breaks_seq,
              margins=c(3,3),lwid=c(0.7,4),lhei=c(0.7,4), key=TRUE,
              keysize=3)
    dev.off()
    
    # test
    png(file=paste("../heatmaps/", paste(design_names[expID], 
                                         combat_names[i], 
                                         "test", sep="_"), 
                   ".png", sep=""),
        width=900, height=700)
    heatmap.2(test_corr[[i]],
              scale='none',dendrogram='none',Rowv=NA,Colv=NA,
              trace='none',col=pal,
              breaks=breaks_seq,
              margins=c(3,3),lwid=c(0.7,4),lhei=c(0.7,4), key=TRUE,
              keysize=3)
    dev.off()
  }
}