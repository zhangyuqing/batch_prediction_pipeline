rm(list=ls())
setwd("C:/Users/zhang/Dropbox/Work/EvanJohnson/13_realdata_101816/boxplots/AUC/")

file_lst <- dir()[grep(".csv", dir())]
median_test_mat <- list()
for(i in 3:5){
  part_files <- file_lst[grep(i, file_lst)]
  readin <- list()
  for(j in 1:length(part_files)){
    readin[[j]] <- read.csv(part_files[j], header=T, row.names="X")
    readin[[j]] <- readin[[j]]["median-test", c(5,3,4,6)]
    colnames(readin[[j]]) <- c("1-step mod", "2-step mod", "2-step hybrid", "1-step null")
  }
  names(readin) <- sub('......$','',part_files)
  median_test_mat[[i-2]] <- do.call(rbind, readin)
}

designname <- c("batch_portion", "batch_portion_size", "all")

for(i in 1:length(median_test_mat)){
  write.csv(round(median_test_mat[[i]],3), 
            file=paste("AUCtest_median_bydata_", designname[i], ".csv", sep=""))
}