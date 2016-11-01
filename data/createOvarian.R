rm(list=ls())

library(curatedOvarianData)

source(system.file("extdata", "patientselection.config",package="curatedOvarianData"))
keep.common.only <- TRUE
strict.checking <- TRUE
impute.missing <- TRUE
quantile.cutoff <- 0.6
combat <- FALSE
meta.required <- c("days_to_death", "vital_status", "batch") 
min.number.of.genes <- 1000
min.number.of.events <- 15
min.sample.size <- 100

## create a list of ExpressionSets
source(system.file("extdata", "createEsetList.R", package="curatedOvarianData"))

## remove samples with 0 time to event
esets <- lapply(esets, function(eset){
  if(length(which(pData(eset)$y[, 1]==0)) > 0){
    return(eset[, -which(pData(eset)$y[, 1]==0)])
  }else{
    return(eset)
  }
})

save(esets, file="data/ovarian.RData")
