rm(list=ls())
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")

cmap_design <- read.csv("data/cmap_design.csv", as.is=TRUE, header=TRUE)
drugname <- c("valproic acid", "vorinostat", "trichostatin A") 
names(drugname) <- c("VPA", "SAHA", "TSA")

datadir <- "/restricted/projectnb/combat/cMAP/cMAP_Steve/"
cmap_fileslst <- dir(datadir)
cmap_fileslst <- cmap_fileslst[grep(".txt", cmap_fileslst)]

dname <- "VPA"


########################  VPA  ########################  
VPA_design <- cmap_design[grep(drugname[dname], cmap_design$cmap_name), -c(4, 13:15)]


#### Clean tags
case_tags <- VPA_design$perturbation_scan_id
case_tags <- gsub("'", "", case_tags)
case_tags <- case_tags[-21] ## cannot find this: 5500024017802120306174.E05 1163
ctrl_tags <- VPA_design$vehicle_scan_id
ctrl_tags <- ctrl_tags[-21]
case_tmp <- strsplit(case_tags[grep(".", ctrl_tags, fixed=TRUE)], ".", fixed=TRUE)
ctrl_tmp <- strsplit(ctrl_tags[grep(".", ctrl_tags, fixed=TRUE)], ".", fixed=TRUE)
ctrl_tmp <- lapply(ctrl_tmp, function(tmp){
  return(tmp[-1])
})
new_ctrl_tags <- c()
for(i in 1:length(ctrl_tmp)){
  new_ctrl_tags <- c(new_ctrl_tags, paste(case_tmp[[i]][1], ctrl_tmp[[i]], sep="."))
}
new_ctrl_tags <- c(ctrl_tags[setdiff(1:length(ctrl_tags), grep(".",ctrl_tags,fixed=TRUE))], new_ctrl_tags)
new_ctrl_tags <- unique(new_ctrl_tags)
## check tags
for(i in 1:length(case_tags)){if(length(grep(case_tags[i], cmap_fileslst))!=1){print(i)}}


#### Match tags with files
VPA_case_mat <- VPA_ctrl_mat <- list()
for(i in 1:length(case_tags)){
  VPA_case_mat[[i]] <- read.table(paste(datadir, cmap_fileslst[grep(case_tags[i], cmap_fileslst)], sep=""),
                                  header=TRUE)
}
for(i in 1:length(new_ctrl_tags)){
  VPA_ctrl_mat[[i]] <- read.table(paste(datadir, cmap_fileslst[grep(new_ctrl_tags[i], cmap_fileslst)], sep=""),
                                  header=TRUE)
}


#### Form expression matrix
VPA_case_mat <- do.call(cbind, VPA_case_mat)
VPA_ctrl_mat <- do.call(cbind, VPA_ctrl_mat)

VPA_set <- as.matrix(cbind(VPA_case_mat, VPA_ctrl_mat))
VPA_condition <- c(rep(1, ncol(VPA_case_mat)), rep(0, ncol(VPA_ctrl_mat)))

tmp <- strsplit(colnames(VPA_case_mat), "__")
batch_case <- sapply(tmp, function(fname){
  return(sub('^.....', '', fname[6]))
})
batch_case == VPA_design$batch_id[-21]

tmp <- strsplit(colnames(VPA_ctrl_mat), "__")
batch_ctrl <- sapply(tmp, function(fname){
  return(sub('^.....', '', fname[3]))
})
batch_ctrl[1] <- "2"
  
batch <- c(batch_case, batch_ctrl)
length(unique(batch))


#### Match case with controls
tb_case <- table(batch_case)[unique(VPA_design$batch_id[-21])]
tb_ctrl <- table(batch_ctrl)[unique(VPA_design$batch_id[-21])]

match_case_ind <- match_ctrl_ind <- c()
for(i in 1:length(tb_case)){
  match_case_ind <- c(match_case_ind, rep(i, tb_case[i]))
  match_ctrl_ind <- c(match_ctrl_ind, rep(i, tb_ctrl[i]))
}


#### Save data 
save(VPA_set, VPA_condition, batch,
     match_case_ind, match_ctrl_ind,
     file=paste("data/cmap_", dname, ".RData", sep=""))



########################  SAHA  ########################  

########################  TSA  ########################  


