rm(list = ls())
library(dplyr); library(SpaDES); library(nlme); library(data.table);library(MuMIn)
library(parallel)
if(as.character(Sys.info()[6]) == "yonluo"){
  workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
} else {
  workPath <- file.path("", "home", "yonluo","Species-function-Tree-growth-Climate-change")
}
analysesData <- fread(file.path(workPath, "data", "finalData.csv"))

analysesData <- analysesData[allCensusLiveTree == "yes" & positiveGrowthTree == "yes",]

unique(analysesData$species)
# [1] "jack pine"        "trembling aspen"  "black spruce"    
# [4] "balsam poplar"    "tamarack larch"   "white spruce"    
# [7] "white birch"      "balsam fir"       "western redcedar"
# [10] "white oak"        "red pine"         "white elm"   

#### assign wood density by species
# source
# http://cfs.nrcan.gc.ca/pubwarehouse/pdfs/11744.pdf
# ovendry wood density is used
# wood density unit: kg/m3
analysesData[species == "balsam fir", woodDensity := 367]
analysesData[species == "tamarack larch", woodDensity := 530]
analysesData[species == "jack pine", woodDensity := 451]
analysesData[species == "red pine", woodDensity := 419]
analysesData[species == "black spruce", woodDensity := 457]
analysesData[species == "white spruce", woodDensity := 404]
analysesData[species == "white birch", woodDensity := 607]
analysesData[species == "white elm", woodDensity := 617]
analysesData[species == "white oak", woodDensity := 775]
analysesData[species == "balsam poplar", woodDensity := 409]
analysesData[species == "trembling aspen", woodDensity := 424]
analysesData[species == "western redcedar", woodDensity := 338]


a <- analysesData[,.(meanBiomassGR = mean(BiomassGR), meanBAGR = mean(BAGR),
                     meanWD = mean(woodDensity)), by = species]

analysesData[,':='(logBiomassGR = log(BiomassGR), 
                   logBasalAreaGR = log(BAGR), 
                  logDBHctd = log(IniDBH)-mean(log(IniDBH)), 
                  Yearctd = Year-mean(Year),
                  logHctd = log(H)-mean(log(H)),
                  logSActd = log(IniFA+2.5)-mean(log(IniFA+2.5)),
                  logWD = log(woodDensity)-mean(log(woodDensity)))]
source(file.path(workPath, "Rcodes", "Rfunctions", "mixedModelSelection.R"))
BiomassTempoutput <- mixedModelSelection(DV = "logBiomassGR", 
                                  IDV = c("logDBHctd", "Yearctd", "logHctd", 
                                          "logWD", "logSActd"),
                                  maxInteraction = 3,
                                  ICTerm = "AIC",
                                  ICCut = 2,
                                  data = analysesData,
                                  random = ~1|PlotID/uniTreeID, 
                                  control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))
BiomassBestFormu <- as.formula(paste("logY~", 
                                     paste(BiomassTempoutput$bestIDV, 
                                           collapse = "+")))
BiomassBestModel <- lme(fixed = BiomassBestFormu,
                 data = analysesData,
                 random = ~1|PlotID/uniTreeID, 
                 control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))

BasalAreaTempoutput <- mixedModelSelection(DV = "logBasalAreaGR", 
                                         IDV = c("logDBHctd", "Yearctd", "logHctd", 
                                                 "logWD", "logSActd"),
                                         maxInteraction = 3,
                                         ICTerm = "AIC",
                                         ICCut = 2,
                                         data = analysesData,
                                         random = ~1|PlotID/uniTreeID, 
                                         control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))
BasalAreaBestFormu <- as.formula(paste("logY~", 
                                     paste(BasalAreaTempoutput$bestIDV, 
                                           collapse = "+")))
BasalAreaBestModel <- lme(fixed = BasalAreaBestFormu,
                        data = analysesData,
                        random = ~1|PlotID/uniTreeID, 
                        control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))



