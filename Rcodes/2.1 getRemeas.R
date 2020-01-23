# get the remeasured data from shared folder
rm(list = ls())
library(haven)
library(dplyr)
library(data.table)
datapath <- "//orbital/S63016/!Workgrp/Inventory/GrowthYield/PSP/Contract_Deliverables/Data/Source_PreJan2019Data"
allfolder <- dir(datapath)
needfolder <- allfolder[as.numeric(substr(allfolder, 1, 4)) >= 2012]
needfolder <- needfolder[!is.na(needfolder)]
needfolder <- needfolder[needfolder != "2012_MPB___GYS"]
needfolder <- needfolder[needfolder != "2013_DMH___GYS"]
needfolder <- needfolder[!(needfolder %in% 2014:2018)]
accesstotal <- data.table()
plottotal <- data.table()
sampletotal <- data.table()
sp_meastotal <- data.table()
speciestotal <- data.table()
tr_meastotal <- data.table()
treestotal <- data.table()

  for(dd in 2014:2018){
  allfolder <- dir(file.path(datapath, dd))
  needfolder <- c(needfolder,
                      file.path(dd, allfolder))
  }

needfolder <- needfolder[needfolder != "2014/2013_Sunshine__GYS"]
needfolder <- needfolder[needfolder != "2014/2014_BCTS__GYS"]
needfolder <- needfolder[needfolder != "2014/2014_Cranbrook_KL"]
needfolder <- needfolder[needfolder != "2014/2014_DKA_A_EAST__GYS"]
needfolder <- needfolder[needfolder != "2015/Proponent"]
needfolder <- needfolder[needfolder != "2016/DCR"]
needfolder <- needfolder[needfolder != "2017/Fraser"]
needfolder <- needfolder[needfolder != "2017/OT18FHQ250_North_Island_Alder"]
needfolder <- needfolder[needfolder != "2018/2018_Carson_entered"]
needfolder <- needfolder[needfolder != "2018/KH_2009_recovery_new_2018-01-11"]
needfolder <- needfolder[needfolder != "2018/Sample 13-68-7"]

for(indifolder in needfolder){
  access <- read_sas(file.path(datapath, indifolder, "access.sas7bdat")) %>%
    data.table
  names(access) <- toupper(names(access))
  accesstotal <- rbindlist(list(accesstotal, access), fill = TRUE)
  
  sampleplot <- read_sas(file.path(datapath, indifolder, "plot.sas7bdat")) %>%
    data.table
  names(sampleplot) <- toupper(names(sampleplot))
  plottotal <- rbindlist(list(plottotal, sampleplot), fill = TRUE)
  
  samples <- read_sas(file.path(datapath, indifolder, "sample.sas7bdat")) %>%
    data.table
  names(samples) <- toupper(names(samples))
  sampletotal <- rbindlist(list(sampletotal, samples), fill = TRUE)
  
  sp_meas <- read_sas(file.path(datapath, indifolder, "sp_meas.sas7bdat")) %>%
    data.table
  names(sp_meas) <- toupper(names(sp_meas))
  sp_meastotal <- rbindlist(list(sp_meastotal, sp_meas), fill = TRUE)
  
  allsasfiles <- c( "species.sas7bdat", 
                   "tr_meas.sas7bdat", "tree.sas7bdat")
  
  species <- read_sas(file.path(datapath, indifolder, "species.sas7bdat")) %>%
    data.table
  names(species) <- toupper(names(species))
  speciestotal <- rbindlist(list(speciestotal, species), fill = TRUE)
  
  tr_meas <- read_sas(file.path(datapath, indifolder, "tr_meas.sas7bdat")) %>%
    data.table
  names(tr_meas) <- toupper(names(tr_meas))
  tr_meastotal <- rbindlist(list(tr_meastotal, tr_meas), fill = TRUE)
  
  trees <- read_sas(file.path(datapath, indifolder, "tree.sas7bdat")) %>%
    data.table
  names(trees) <- toupper(names(trees))
  treestotal <- rbindlist(list(treestotal, trees), fill = TRUE)
}

outputpath <- "C:/F/GitHub/tree_growth-climate_change-relationships/data/bc_remeasurements"
saveRDS(accesstotal, file.path(outputpath, "access.rds"))
saveRDS(plottotal, file.path(outputpath, "plot.rds"))
saveRDS(sampletotal, file.path(outputpath, "samples.rds"))
saveRDS(sp_meastotal, file.path(outputpath, "sample_meas.rds"))
saveRDS(speciestotal, file.path(outputpath, "species.rds"))
saveRDS(tr_meastotal, file.path(outputpath, "tree_meas.rds"))
saveRDS(treestotal, file.path(outputpath, "trees.rds"))

