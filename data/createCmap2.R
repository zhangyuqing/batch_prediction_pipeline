rm(list=ls())
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/")

cmap_design <- read.csv("data/cmap_design.csv", as.is=TRUE, header=TRUE)
cmap_design <- cmap_design[-which(cmap_design$instance_id==1163), ]
drugname <- c("valproic_acid", "vorinostat", "trichostatin_A") 
names(drugname) <- c("VPA", "SAHA", "TSA")

datadir <- "/restricted/projectnb/combat/cMAP/cMAP_Steve/"
cmap_fileslst <- dir(datadir)
cmap_fileslst <- cmap_fileslst[grep(".txt", cmap_fileslst)]



#### Whole dataset ####
## Form expression matrix 
cmap_mat <- list()
for(i in 1:length(cmap_fileslst)){
  cmap_mat[[i]] <- read.table(paste(datadir, cmap_fileslst[i], sep=""), header=TRUE)
}
cmap_mat <- do.call(cbind, cmap_mat)

## Extract condition (case / control) info 
cmap_condition <- rep(0, ncol(cmap_mat))
case_ind <- grep("Perturbed", colnames(cmap_mat))
ctrl_ind <- grep("Vehicle", colnames(cmap_mat))
sum(c(case_ind,ctrl_ind)!=1:7055)
cmap_condition[case_ind] <- rep(1, length(case_ind))
sum(cmap_condition) == nrow(cmap_design)

## Extract batch info 
names_lst <- strsplit(colnames(cmap_mat), "__")
cmap_batch <- sapply(names_lst, function(fname){
  batch_item <- fname[grep("Batch", fname)]
  return(sub('^.....', '', batch_item))
})
# sanity check
tmp_batch <- cmap_batch[which(cmap_condition==1)]
sum(table(tmp_batch)!=table(cmap_design$batch_id))



#### Extract VPA ####
## case (perturbation)
VPA_case_ind <- grep(drugname["VPA"], colnames(cmap_mat))
## ctrl (vehicle)
VPA_design <- cmap_design[grep("valproic acid", cmap_design$cmap_name), -c(4, 13:15)]
case_tags <- VPA_design$perturbation_scan_id
case_tags <- gsub("'", "", case_tags) 
ctrl_tags <- VPA_design$vehicle_scan_id
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
VPA_ctrl_ind <- c()
for(i in 1:length(new_ctrl_tags)){
  VPA_ctrl_ind <- c(VPA_ctrl_ind, grep(new_ctrl_tags[i], colnames(cmap_mat)))
}



#### Extract SAHA ####
SAHA_case_ind <- grep(drugname["SAHA"], colnames(cmap_mat))
SAHA_design <- cmap_design[grep("vorinostat", cmap_design$cmap_name), -c(4, 13:15)]
case_tags <- SAHA_design$perturbation_scan_id
case_tags <- gsub("'", "", case_tags) 
ctrl_tags <- SAHA_design$vehicle_scan_id
case_tmp <- strsplit(case_tags, ".", fixed=TRUE)
ctrl_tmp <- strsplit(ctrl_tags, ".", fixed=TRUE)
ctrl_tmp <- lapply(ctrl_tmp, function(tmp){
  return(tmp[-1])
})
new_ctrl_tags <- c()
for(i in 1:length(ctrl_tmp)){
  new_ctrl_tags <- c(new_ctrl_tags, paste(case_tmp[[i]][1], ctrl_tmp[[i]], sep="."))
}
new_ctrl_tags <- unique(new_ctrl_tags)
SAHA_ctrl_ind <- c()
for(i in 1:length(new_ctrl_tags)){
  SAHA_ctrl_ind <- c(SAHA_ctrl_ind, grep(new_ctrl_tags[i], colnames(cmap_mat)))
}



#### Extract TSA ####
TSA_case_ind <- grep(drugname["TSA"], colnames(cmap_mat))
TSA_design <- cmap_design[grep("trichostatin A", cmap_design$cmap_name), -c(4, 13:15)]
case_tags <- TSA_design$perturbation_scan_id
case_tags <- gsub("'", "", case_tags) 
ctrl_tags <- TSA_design$vehicle_scan_id
ctrl_tags <- gsub("'", "", ctrl_tags) 

ctrl_tags_group1ind <- which(nchar(ctrl_tags)==20 | nchar(ctrl_tags)==24)
ctrl_tags_group2ind <- setdiff(1:length(ctrl_tags), ctrl_tags_group1ind)
# sanity check
ctrl_tags_group2ind==which(nchar(ctrl_tags)!=20&nchar(ctrl_tags)!=24)
intersect(ctrl_tags_group1ind, ctrl_tags_group2ind)
ctrl_tags[ctrl_tags_group1ind]
ctrl_tags[ctrl_tags_group2ind]

case_tmp <- strsplit(case_tags[ctrl_tags_group1ind], ".", fixed=TRUE)
ctrl_tmp <- strsplit(ctrl_tags[ctrl_tags_group1ind], ".", fixed=TRUE)
ctrl_tmp <- lapply(ctrl_tmp, function(tmp){
  return(tmp[-1])
})
new_ctrl_tags <- c()
for(i in 1:length(ctrl_tmp)){
  new_ctrl_tags <- c(new_ctrl_tags, paste(case_tmp[[i]][1], ctrl_tmp[[i]], sep="."))
}
new_ctrl_tags <- c(ctrl_tags[ctrl_tags_group2ind], new_ctrl_tags)
new_ctrl_tags <- unique(new_ctrl_tags)
TSA_ctrl_ind <- c()
for(i in 1:length(new_ctrl_tags)){
  TSA_ctrl_ind <- c(TSA_ctrl_ind, grep(new_ctrl_tags[i], colnames(cmap_mat)))
}



#### Save cmap whole dataset ####
save(cmap_mat, cmap_condition, cmap_batch, 
     VPA_case_ind, VPA_ctrl_ind,
     SAHA_case_ind, SAHA_ctrl_ind,
     TSA_case_ind, TSA_ctrl_ind,
     file="data/cmap_whole.RData")