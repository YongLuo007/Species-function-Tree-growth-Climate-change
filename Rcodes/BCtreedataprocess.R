rm(list = ls())
library(data.table);library(ggplot2);library(gridExtra);library(raster)
library(dplyr); library(SpaDES)
workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
allPSP <- fread(file.path(workPath, "data", "BCtrees.csv"))
unique(allPSP$LiveDead)
# "L"  "I"  "DU" "DP" "V"  "C"
cutPlots <- unique(allPSP[LiveDead == "C", ]$PlotNumber)
print(cutPlots)
# "59071 R000108" "70030 G000008" had cut trees, the plot should be removed
allPSP <- allPSP[!(PlotNumber %in% cutPlots), ]

# check whether the dead status of a tree was the last observation of that tree
deadtrees <- allPSP[LiveDead == "DU",][, .(firstDeadYear=min(MeasureYear)), by = uniTreeID]
livetrees <- allPSP[LiveDead != "DU", ][,.(lastLiveYear = max(MeasureYear)), by = uniTreeID]
deadtrees <- setkey(deadtrees, uniTreeID)[setkey(livetrees, uniTreeID),
                                          nomatch = 0]
deadBeforeLiveTrees <- deadtrees[lastLiveYear >= firstDeadYear, ]$uniTreeID
print(length(deadBeforeLiveTrees)) # 2 good

deadtrees[uniTreeID == deadBeforeLiveTrees[1]]
print(allPSP[uniTreeID == deadBeforeLiveTrees[1],.(MeasureYear, LiveDead)])
#    MeasureYear LiveDead
# 1:        1996       DU
# 2:        1986        L
# 3:        2007       DP
# 4:        2013       DP
allPSP[uniTreeID == deadBeforeLiveTrees[1], LiveDead:="DP"]

print(allPSP[uniTreeID == deadBeforeLiveTrees[2],.(MeasureYear, DBH, LiveDead)][order(MeasureYear),])
allPSP[uniTreeID == deadBeforeLiveTrees[2], LiveDead:="DP"]

# select live trees
livetrees <- allPSP[LiveDead != "DU", ]

# check whether there was missing measurement
range(livetrees$MeasureYear) # 1926 to 2013
# hist(livetrees$meas_yr)
allplots <- unique(livetrees$PlotNumber)



myFunction <- function(plotdata){
  missingMeasureTrees <- NULL
  plotCensus <- sort(unique(plotdata$MeasureYear))
  treeIDs <- unique(plotdata$uniTreeID)
  for(inditree in treeIDs){
    treedata <- plotdata[uniTreeID == inditree,]
    treecensus <- sort(treedata$MeasureYear)
    if(length(treecensus) != length(plotCensus[plotCensus <= max(treecensus) &
                                               plotCensus >= min(treecensus)])){
      missingMeasureTrees <- c(missingMeasureTrees, inditree)
    }
  }
  return(missingMeasureTrees)
}

livetreesList <- list()
for(indiplot in allplots){
  livetreesList[[indiplot]] <- livetrees[PlotNumber == indiplot]
}

cl <- parallel::makeCluster(parallel::detectCores()-1)
parallel::clusterExport(cl, c("myFunction"))
allmissingMearsureTrees <- lapply(livetreesList, function(s) myFunction(plotdata = s))
parallel::stopCluster(cl)

allmissingMearsureTrees <- unlist(allmissingMearsureTrees)
length(allmissingMearsureTrees) # 0 good

# species inspection
livetrees[, speciesLength:=length(unique(Species)), by = uniTreeID]
unique(livetrees$speciesLength) # 1 good
livetrees[, speciesLength:=NULL]
livetrees[, orgSpecies:=Species]
livetrees[, species:=NULL]
livetrees[orgSpecies == "SB", species := "black spruce"]
livetrees[orgSpecies == "LT", species := "tamarack larch"]
livetrees[orgSpecies == "SW", species := "white spruce"]
livetrees[orgSpecies == "EP", species := "white birch"]
livetrees[orgSpecies == "PLI", species := "lodgepole pine"]
livetrees[orgSpecies == "ACB", species := "balsam poplar"]
livetrees[orgSpecies == "S", species := "black spruce"]
# S for spruce in original doc. assume S is black spruce
livetrees[orgSpecies == "PL", species := "lodgepole pine"]
livetrees[orgSpecies == "ACT", species := "black cottonwood"]
livetrees[orgSpecies == "AT", species := "trembling aspen"]
livetrees[orgSpecies == "E", species := "white birch"]
# E for birch in original doc. assume S is white birch
# livetrees[orgSpecies == "XC", species := "unknow conifer"]
livetrees[orgSpecies == "BL", species := "alpine fir"]
livetrees[orgSpecies == "EA", species := "white birch"]
# EA is alaka paper birch, assume it is white birch here
livetrees[orgSpecies == "AC", species := "balsam poplar"]
# livetrees[orgSpecies == "X", species := "unknown"]
livetrees[orgSpecies == "B", species := "fir"]
livetrees[orgSpecies == "W", species := "willow"]
livetrees[orgSpecies == "DR", species := "red alder"]
livetrees[orgSpecies == "DM", species := "mountain alder"]
livetrees[orgSpecies == "PJ", species := "jack pine"]
livetrees[orgSpecies == "ZH", species := "hardwood"]
livetrees[orgSpecies == "SXW", species := "white spruce"]
# SXW is hybrid between engelmann and white spruce
livetrees[orgSpecies == "XH", species := "hardwood"]
livetrees[orgSpecies == "SX", species := "white spruce"]
# SX is hybrid
livetrees[orgSpecies == "A", species := "trembling aspen"]
# A is aspen cottonwood and poplar, assume it is trembling aspen
livetrees[orgSpecies == "P", species := "lodgepole pine"]
# P is pine orgSpecies, assume it is lodgepole pine
livetrees[orgSpecies == "MR", species := "red maple"]
# MR could not find, assume it is red maple
livetrees[orgSpecies == "SE", species := "engelmann spruce"]
livetrees[orgSpecies == "FD", species := "douglas-fir"] 
livetrees[orgSpecies == "BA", species := "amabalis fir"] 
livetrees[orgSpecies == "CW", species := "western redcedar"]
livetrees[orgSpecies == "HW", species := "western hemlock"]
livetrees[orgSpecies == "FDI", species := "douglas-fir"]
# livetrees[orgSpecies == "SXE", species := "unknown"]
livetrees[orgSpecies == "D", species := "red alder"]
# D is alder, assume red alder
livetrees[orgSpecies == "H", species := "western hemlock"]
livetrees[orgSpecies == "MV", species := "vine maple"] 
livetrees[orgSpecies == "HM", species := "mountain hemlock"] 
livetrees[orgSpecies == "EXP", species := "white birch"]
livetrees[orgSpecies == "WS", species := "scoulers willow"]
livetrees[orgSpecies == "AX", species := "trembling aspen"]

# 
nrow(livetrees[is.na(species),]) # 0 good

# assign biomass
source(file.path(workPath, "Rcodes",  "Rfunctions", "biomassCalculation.R"))
livetrees$biomass <- biomassCalculation(species = livetrees$species,
                                        DBH = livetrees$DBH,
                                        paperSource = "Ung2008")


# manipulate live tree data
livetrees <- livetrees[order(uniTreeID, MeasureYear),]
setnames(livetrees, c("MeasureYear", "DBH", "biomass"), c("IniYear", "IniDBH", "IniBiomass"))
livetrees <- livetrees[,c("FinYear", "FinDBH", "FinBiomass", "tempuniTreeID") 
                 := data.table::shift(x = livetrees[,.(IniYear, IniDBH, IniBiomass, uniTreeID)],
                                      n = 1, fill = NA, type = "lead", give.names = FALSE)]

livetrees <- livetrees[tempuniTreeID == uniTreeID,][,tempuniTreeID := NULL]
livetrees <- livetrees[IniYear != FinYear,]
livetrees[, measInterval:=FinYear-IniYear]
range(livetrees$measInterval) # 2 to 35 years



livetrees[, BiomassGR:=(FinBiomass-IniBiomass)/measInterval]

plotCensusMinDBH <- livetrees[,.(MinDBH = min(IniDBH)), by = c("PlotNumber", "IniYear")]

livetrees4 <- livetrees[IniDBH>=4,]
livetrees4[, plotMeasureTime:=length(unique(IniYear)), by = PlotNumber]
livetrees4[, treeMeasureTime:=length(unique(IniYear)), by = PlotNumber]

livetrees4[, allCensusLiveTree:="no"]
livetrees4[plotMeasureTime == treeMeasureTime, allCensusLiveTree := "yes"]

livetrees4[, treeMinGR:=min(BiomassGR), by = uniTreeID]

livetrees4[,positiveGrowthTree:="no"]
livetrees4[treeMinGR>0, positiveGrowthTree := "yes"]

allcensuspositive4 <- livetrees4[allCensusLiveTree == "yes" & 
                                     positiveGrowthTree == "yes",]
length(unique(allcensuspositive4$uniTreeID)) # 32108
length(unique(allcensuspositive4$PlotNumber)) # 213

write.csv(livetrees4, file.path(workPath, "data", "forcompetitionIndex.csv"),
          row.names = FALSE)

write.csv(livetrees4, file.path(workPath, "data", "finalBCdata.csv"), 
          row.names = FALSE)

treeinfor <- unique(allcensuspositive4, by = "uniTreeID")
speciesinfor <- treeinfor[,.(NumberOfTree=length(FinDBH)), by = species]

#            species NumberOfTree
# 1:  lodgepole pine         8747
# 2:    white spruce         8201
# 3:      alpine fir         1709
# 4:          willow          448
# 5:   balsam poplar           50
# 6:     white birch         1406
# 7:             fir         1491
# 8:    black spruce         2581
# 9: western hemlock            4
# 10:     douglas-fir          278
# 11: trembling aspen         7192
# 12:       red alder            1





