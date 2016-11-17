rm(list=ls())
#setwd("C:/Users/zhang/Documents/Work/Evan/predictive-models/100616-refine-pipeline/results")
setwd("/restricted/projectnb/johnsonlab/yuqingz/batch_prediction_pipeline/results/")

source("../summaRes/loadStat.R")
source("../summaRes/plotBox.R")
source("../summaRes/loadCorr.R")
source("../summaRes/plotHeatCorr.R")
source("../summaRes/loadGaps.R")
source("../summaRes/plotHist.R")
source("../summaRes/plotLine.R")

# design
experiments_ID <- c("5_5_5_5_5_5", # balanced
                    "2_7_5_5_5_5", # trn/tst %
                    "5_5_3_7_2_8", # batch %
                    "5_5_2_6_3_9", # batch size & %
                    "2_7_2_9_3_9") # all

# prediction models
method_names <- c("Lasso", "Elnet", "NaiveBayes", "SVM",
                  "kNN", "RandForest", "NeuralNets", "Mas-o-menos","WeightedVoting")


#### Boxplot of performance statistics ####
for(expID in 1:length(experiments_ID)){
  statList <- loadStat(experiments_ID[expID]) ## all performance matrices for one design
  
  plotBox(datList=statList, method_names=method_names, statname="AUC", expID=expID)
  plotBox(datList=statList, method_names=method_names, statname="sensitivity", expID=expID)
  plotBox(datList=statList, method_names=method_names, statname="specificity", expID=expID)
}  
rm(statList)

  
#### Heatmap of correlations between patients ####
for(expID in 1:length(experiments_ID)){
  corrList <- loadCorr(experiments_ID[expID]) ## all datasets (expression matrices for one design)
  
  plotHeatCorr(datList=corrList, expID=expID)
}
rm(corrList)


#### two sample t-test for AUC gaps ####
ttest_table <- list()
for(expID in 1:length(experiments_ID)){
  gapsList <- loadGaps(experiments_ID[expID]) ## all AUC gaps matrices for one design
  
  ttest_table[[expID]] <- plotHist(gapsList=gapsList, method_names=method_names, expID=expID)
}  
save(ttest_table, file="../ttest_table.RData")


#### plot the gaps against imbalance increase
design_gaps_List <- list()
for(expID in 1:length(experiments_ID)){
  design_gaps_List[[expID]] <- loadGaps(experiments_ID[expID]) ## all AUC gaps matrices for one design
}
names(design_gaps_List) <- c("balanced", "trn/tst %", "batch %", "batch size & %", "all")
plotLine(design_gaps_List)
