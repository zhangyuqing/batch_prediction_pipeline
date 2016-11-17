rm(list=ls())
#setwd("C:/Users/zhang/Documents/Work/Evan/predictive-models/100616-refine-pipeline/results")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/results/meta/")

source("../summaRes/plotLine2.R")

experiments_ID <- c("200_100_200_100_2_2_50_50_50_50_50_50_50_50", # balanced
                    "200_40_200_140_2_2_20_80_20_80_70_30_70_30", # trn/tst %
                    "200_100_200_100_2_2_30_70_70_30_20_80_80_20", # batch %
                    "200_100_200_100_2_2_20_60_80_40_30_90_70_10", # batch size & %
                    "200_40_200_140_2_2_8_144_32_16_42_54_98_6") # all
combat_ID <- c("FALSE_mean_none_none", "TRUE_mean_none_none",
               "TRUE_mean_separate_mod", "TRUE_mean_separate_null",
               "TRUE_mean_combined_mod", "TRUE_mean_combined_null")
method_names <- c("Lasso", "Elnet", "NaiveBayes", "SVM",
                  "kNN", "RandForest", "NeuralNets", "Mas-o-menos","WeightedVoting")
design_names <- c("balanced", "trn_tst", "batch_portion", "batch_portion_size", "all")
combat_names <- c("base", "batch", "sep_mod",
                  "sep_hyb", "cmb_mod", "cmp_null")


#### plot AUC against combat data ####
expID=5
design_regex <- experiments_ID[expID]
file_lst <- dir()[grep(design_regex, dir())]
length(file_lst) == length(combat_ID)
  
stats_train_lst <- stats_test_lst <- list()
AUC_train <- AUC_test <- list()
for(combat_ind in 1:length(combat_names)){
  load(file_lst[grep(combat_ID[combat_ind], file_lst)])
  # Lasso
  AUC_lasso_train <- sapply(res_lasso$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  lasso_tst_stats <- append(res_lasso$stats_test, list(Integrate=res_lasso$meta_stat))
  AUC_lasso_test <- sapply(lasso_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # Elnet
  AUC_elnet_train <- sapply(res_elnet$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  elnet_tst_stats <- append(res_elnet$stats_test, list(Integrate=res_elnet$meta_stat))
  AUC_elnet_test <- sapply(elnet_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # NaiveBayes
  AUC_nb_train <- sapply(res_nb$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  nb_tst_stats <- append(res_nb$stats_test, list(Integrate=res_nb$meta_stat))
  AUC_nb_test <- sapply(nb_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # SVM
  AUC_svm_train <- sapply(res_svm$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  svm_tst_stats <- append(res_svm$stats_test, list(Integrate=res_svm$meta_stat))
  AUC_svm_test <- sapply(svm_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # kNN
  AUC_knn_train <- sapply(res_knn$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  knn_tst_stats <- append(res_knn$stats_test, list(Integrate=res_knn$meta_stat))
  AUC_knn_test <- sapply(knn_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # RandForest
  AUC_rf_train <- sapply(res_rf$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  rf_tst_stats <- append(res_rf$stats_test, list(Integrate=res_rf$meta_stat))
  AUC_rf_test <- sapply(rf_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # NeuralNets
  AUC_nnet_train <- sapply(res_nnet$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  nnet_tst_stats <- append(res_nnet$stats_test, list(Integrate=res_nnet$meta_stat))
  AUC_nnet_test <- sapply(nnet_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # Mas-o-menos
  AUC_mas_train <- sapply(res_mas$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  mas_tst_stats <- append(res_mas$stats_test, list(Integrate=res_mas$meta_stat))
  AUC_mas_test <- sapply(mas_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # WeightedVoting
  AUC_vote_train <- sapply(res_vote$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  vote_tst_stats <- append(res_vote$stats_test, list(Integrate=res_vote$meta_stat))
  AUC_vote_test <- sapply(vote_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  AUC_train[[combat_ind]] <- rbind(AUC_lasso_train, AUC_elnet_train,
                     AUC_nb_train, AUC_svm_train,
                     AUC_knn_train, AUC_rf_train,
                     AUC_nnet_train, AUC_mas_train, AUC_vote_train)
  AUC_test[[combat_ind]] <- rbind(AUC_lasso_test, AUC_elnet_test,
                    AUC_nb_test, AUC_svm_test,
                    AUC_knn_test, AUC_rf_test,
                    AUC_nnet_test, AUC_mas_test, AUC_vote_test)
  rownames(AUC_train[[combat_ind]]) <- rownames(AUC_test[[combat_ind]]) <- method_names
}
names(AUC_train) <- names(AUC_test) <- combat_names

for(method_row in 1:length(method_names)){
  tmp_train <- lapply(AUC_train, function(AUC_trn){
    return(AUC_trn[method_row, ])
  })
  AUC_train_mat <- t(do.call(rbind, tmp_train))
  tmp_test <- lapply(AUC_test, function(AUC_tst){
    return(AUC_tst[method_row, ])
  })
  AUC_test_mat <- t(do.call(rbind, tmp_test))
  AUC_gap_mat <- AUC_train_mat - AUC_test_mat[1:3, ]
  plotLine2(AUC_train_mat, outdir="../../figures/meta/bydata/AUC_train/", 
            design_name=paste(method_names[method_row], design_names[expID], "train", sep="_"),
            ylab_text="AUC Train")
  plotLine2(AUC_test_mat, outdir="../../figures/meta/bydata/AUC_test/", 
            design_name=paste(method_names[method_row], design_names[expID], "test", sep="_"),
            ylab_text="AUC Test")
  plotLine2(AUC_gap_mat, outdir="../../figures/meta/bydata/AUC_gap/", 
            design_name=paste(method_names[method_row], design_names[expID], "gap", sep="_"),
            ylab_text="AUC Train - AUC Test")
}



#### plot AUC against design ####
datID=6
combat_regex <- combat_ID[datID]
file_lst <- dir()[grep(combat_regex, dir())]
length(file_lst) == length(design_names)

stats_train_lst <- stats_test_lst <- list()
AUC_train <- AUC_test <- list()
for(design_ind in 1:length(design_names)){
  load(file_lst[grep(experiments_ID[design_ind], file_lst)])
  # Lasso
  AUC_lasso_train <- sapply(res_lasso$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  lasso_tst_stats <- append(res_lasso$stats_test, list(Integrate=res_lasso$meta_stat))
  AUC_lasso_test <- sapply(lasso_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # Elnet
  AUC_elnet_train <- sapply(res_elnet$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  elnet_tst_stats <- append(res_elnet$stats_test, list(Integrate=res_elnet$meta_stat))
  AUC_elnet_test <- sapply(elnet_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # NaiveBayes
  AUC_nb_train <- sapply(res_nb$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  nb_tst_stats <- append(res_nb$stats_test, list(Integrate=res_nb$meta_stat))
  AUC_nb_test <- sapply(nb_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # SVM
  AUC_svm_train <- sapply(res_svm$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  svm_tst_stats <- append(res_svm$stats_test, list(Integrate=res_svm$meta_stat))
  AUC_svm_test <- sapply(svm_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # kNN
  AUC_knn_train <- sapply(res_knn$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  knn_tst_stats <- append(res_knn$stats_test, list(Integrate=res_knn$meta_stat))
  AUC_knn_test <- sapply(knn_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # RandForest
  AUC_rf_train <- sapply(res_rf$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  rf_tst_stats <- append(res_rf$stats_test, list(Integrate=res_rf$meta_stat))
  AUC_rf_test <- sapply(rf_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # NeuralNets
  AUC_nnet_train <- sapply(res_nnet$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  nnet_tst_stats <- append(res_nnet$stats_test, list(Integrate=res_nnet$meta_stat))
  AUC_nnet_test <- sapply(nnet_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # Mas-o-menos
  AUC_mas_train <- sapply(res_mas$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  mas_tst_stats <- append(res_mas$stats_test, list(Integrate=res_mas$meta_stat))
  AUC_mas_test <- sapply(mas_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  # WeightedVoting
  AUC_vote_train <- sapply(res_vote$stats_train, function(stat_train){
    return(stat_train$AUC)
  })
  vote_tst_stats <- append(res_vote$stats_test, list(Integrate=res_vote$meta_stat))
  AUC_vote_test <- sapply(vote_tst_stats, function(stat_test){
    return(stat_test$AUC)
  })
  AUC_train[[design_ind]] <- rbind(AUC_lasso_train, AUC_elnet_train,
                                   AUC_nb_train, AUC_svm_train,
                                   AUC_knn_train, AUC_rf_train,
                                   AUC_nnet_train, AUC_mas_train, AUC_vote_train)
  AUC_test[[design_ind]] <- rbind(AUC_lasso_test, AUC_elnet_test,
                                  AUC_nb_test, AUC_svm_test,
                                  AUC_knn_test, AUC_rf_test,
                                  AUC_nnet_test, AUC_mas_test, AUC_vote_test)
  rownames(AUC_train[[design_ind]]) <- rownames(AUC_test[[design_ind]]) <- method_names
}
names(AUC_train) <- names(AUC_test) <- design_names

for(method_row in 1:length(method_names)){
  tmp_train <- lapply(AUC_train, function(AUC_trn){
    return(AUC_trn[method_row, ])
  })
  AUC_train_mat <- t(do.call(rbind, tmp_train))
  tmp_test <- lapply(AUC_test, function(AUC_tst){
    return(AUC_tst[method_row, ])
  })
  AUC_test_mat <- t(do.call(rbind, tmp_test))
  AUC_gap_mat <- AUC_train_mat - AUC_test_mat[1:3, ]
  plotLine2(AUC_train_mat, outdir="../../figures/meta/bydesign/AUC_train/", 
            design_name=paste(method_names[method_row], combat_names[datID], "train", sep="_"),
            ylab_text="AUC Train")
  plotLine2(AUC_test_mat, outdir="../../figures/meta/bydesign/AUC_test/", 
            design_name=paste(method_names[method_row], combat_names[datID], "test", sep="_"),
            ylab_text="AUC Test")
  plotLine2(AUC_gap_mat, outdir="../../figures/meta/bydesign/AUC_gap/", 
            design_name=paste(method_names[method_row], combat_names[datID], "gap", sep="_"),
            ylab_text="AUC Train - AUC Test")
}
