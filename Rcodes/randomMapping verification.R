rm(list=ls())
library(data.table);library(ggplot2); library(dplyr); library(nlme)
library(SpaDES); library(parallel); library(raster);library(maptools)
library(rgeos)
library(spatstat, lib = file.path("~/R/R-3.3.2/library"))
if(as.character(Sys.info()[6]) == "yonluo"){
  workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
} else {
  workPath <- file.path("", "home", "yonluo",
                        "~/Github/Species-function-Tree-growth-Climate-change")
}
inputDataOrg <- fread(file.path(workPath, "data", "forcompetitionIndex.csv"))

plotinfor <- fread(file.path(workPath, "data", "BCselectedPlot.csv"))
plotinforNosubPlot <- plotinfor[no_plots == 1 & sampletype == "G"]
plotinforNosubPlot <- unique(plotinforNosubPlot[,.(SAMP_ID, plotSize = area_pm*10000)],
                             by = "SAMP_ID")
inputData <- setkey(inputDataOrg, SAMP_ID)[setkey(plotinforNosubPlot, SAMP_ID),
                                           nomatch = 0]
inputData[, plotShape:="circle"]

setnames(inputData, c("stem_map_bearing", "stem_map_slope_distance", "SAMP_ID", "uniTreeID",
                      "IniBiomass", "species"),
         c("Angle", "Distance", "PlotNumber", "TreeNumber", "Biomass", "Species"))


oneplot <- unique(inputData$PlotNumber)[1]

oneplotonecensustest <- inputData[PlotNumber == oneplot,]
onecensus <- unique(oneplotonecensustest$Year)[1]
oneplotonecensustest <- oneplotonecensustest[Year == onecensus,]
source(file.path(workPath, "Rcodes", "Rfunctions", "HeghyiCICalculationModified.R"))
CIbasedonObs <- HeghyiCICalculation(data = oneplotonecensustest,
                              maxRadius = 12.62,
                              sizeIndex = "Biomass",
                              distanceWeight = 1, 
                              sizeWeight = 0,
                              assymetricScale = "Rescale")

names(CIbasedonObs)[4:6] <- c("ObsH", "ObsIntra", "ObsInter")

source(file.path(workPath, "Rcodes", "Rfunctions", "randomStemMapping.R"))

source(file.path(workPath, "Rcodes", "Rfunctions", "HIndexGenerator.R"))

for(i in 1:10){
  alltreemapping <- randomStemMapping(onePlotoneCensusdata = oneplotonecensustest, 
                                              SizeCutLength = 5,
                                              plotSize = oneplotonecensustest$plotSize[1])
  alltreemapping[, orgTreeNumber:=TreeNumber]
  alltreemapping[, TreeNumber:=1:length(Biomass)]
  
  CIbasedonRandom <- HIndexGenerator(data = alltreemapping,
                                     maxRadius = 12.62,
                                     sizeIndex = "Biomass",
                                     distanceWeight = 1, 
                                     sizeWeight = 0,
                                     assymetricScale = "Rescale")
  names(CIbasedonRandom)[4:6] <- c("RanH", "RanIntra", "RanInter")
  
  CIbasedonRandomModify <- setkey(CIbasedonRandom, 
                                  TreeNumber)[setkey(alltreemapping[,.(x, y, TreeNumber, orgTreeNumber)],
                                                                      TreeNumber),
                                                               nomatch = 0]
  CIbasedonRandomModify[,TreeNumber:=orgTreeNumber]
  
  CIbasedonRandomModify <- CIbasedonRandomModify[abs(x)<=(50-12.62) &
                                                   abs(y)<=(50-12.62),]
  CIbasedonRandomModify[,':='(x = NULL, y = NULL, orgTreeNumber = NULL)]
  if(i == 1){
    allCIrandom <- CIbasedonRandomModify
  } else {
    allCIrandom <- rbind(allCIrandom, CIbasedonRandomModify)
  }
}
atree <- unique(allCIrandom$TreeNumber)[3]
hist(allCIrandom[TreeNumber == atree,]$RanH)

allConbined <- setkey(allCIrandom, PlotNumber, Year,
                      TreeNumber)[setkey(CIbasedonObs, PlotNumber,
                                         Year, TreeNumber),
                                  nomatch = 0]
plot(RanH~ObsH, data = allConbined)
allConbinedshort <- allConbined[,.(ObsH = mean(ObsH),
                                   RanHmean = exp(mean(log(RanH)))),
                                by = c("PlotNumber", "Year", "TreeNumber")]
plot(RanHmean~ObsH, data = allConbinedshort)
summary(lm(RanHmean~ObsH, data = allConbinedshort))

abline(1,1)
