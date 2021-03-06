rm(list=ls())
library(data.table);library(ggplot2); library(dplyr); library(nlme)
library(SpaDES); library(parallel)
if(as.character(Sys.info()[6]) == "yonluo"){
  workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
} else {
  workPath <- file.path("", "home", "yonluo",
                        "~/Github/Species-function-Tree-growth-Climate-change")
}
inputDataOrg <- fread(file.path(workPath, "data", "forcompetitionIndex.csv"))

plotinfor <- fread(file.path(workPath, "data", "BCselectedPlot.csv"))
plotinforNosubPlot <- plotinfor[SampeType == "G" & StemMapped == "Y"]
plotinforNosubPlot <- unique(plotinforNosubPlot[,.(PlotNumber, plotSize = PlotSize*10000)],
                             by = "PlotNumber")
inputData <- setkey(inputDataOrg, PlotNumber)[setkey(plotinforNosubPlot, PlotNumber),
                                        nomatch = 0]
inputData[, plotShape:="circle"]

setnames(inputData, c("IniYear", "IniBiomass", "species"),
         c("Year", "Biomass", "Species"))

inputData[, ':='(minAngle = min(coordX),
                           maxAngle = max(coordX),
                           angleTime = length(unique(coordX))),
                    by = uniTreeID]


print(unique(inputData$angleTime)) # 1 2

noMappingTrees <- unique(inputData[is.na(minAngle) & angleTime == 1,]$uniTreeID)
print(length(noMappingTrees)) # 20 trees
# remove these trees in from the dataset
inputData <- inputData[!(uniTreeID %in% noMappingTrees), ]


# check tree had different angle but all had values
print(setkey(inputData[minAngle != maxAngle,.(uniTreeID, Year, 
                                                        coordX, coordY)],
             uniTreeID, Year))

# 0 row
source(file.path(workPath, "Rcodes", "Rfunctions", "HeghyiCICalculationModified.R"))

sizeWeightList <- list("sizeWeight1" = seq(0, 2, by = 0.1),
                       "sizeWeight2" = seq(2.1, 4, by = 0.1),
                       "sizeWeight3" = seq(4.1, 6, by = 0.1),
                       "sizeWeight4" = seq(6.1, 8, by = 0.1),
                       "sizeWeight5" = seq(8.1, 10, by = 0.1))


for(k in 1:5){
  sizeWeight <-  sizeWeightList[[k]]
  disWeight  <-  seq(0, 2, by = 0.1)
  processCIdata <- data.table::copy(inputData)
  CIdata <- HeghyiCICalculation(data = processCIdata,
                                maxRadius = 12.62,
                                sizeIndex = "Biomass",
                                distanceWeight = disWeight, 
                                sizeWeight = sizeWeight,
                                assymetricScale = "Rescale")
  dd <- data.table::copy(CIdata)
  dd[, ':='(uniTreeID = paste(PlotNumber, "_", TreeNumber, sep = ""),
            IniYear = Year)][,':='(PlotNumber = NULL, TreeNumber = NULL, Year = NULL)]
  dd <- unique(dd, by = c("uniTreeID", "IniYear"))
  names(dd) <- gsub("\\.", "_", names(dd))
  
  for(i in sizeWeight){
    for(j in disWeight){
      newCIdata <- data.table::copy(dd)
      indicombweight <- paste(c("H_", "IntraH_", "InterH_"), paste("DW", j, "_SW", i, sep = ""), sep = "")
      indicombweight <- gsub("\\.", "_", indicombweight)
      setnames(newCIdata, indicombweight, c("H", "IntraH", "InterH"))
      newCIdata <- newCIdata[,.(uniTreeID, IniYear, H, IntraH, InterH)]
      write.csv(newCIdata, file.path(workPath, "data", "AllCompetitioinData_BC",
                                     paste("CompetitionData_DW", j, "_SW", i, ".csv", sep = "")),
                row.names = FALSE)
    }
  }
}







