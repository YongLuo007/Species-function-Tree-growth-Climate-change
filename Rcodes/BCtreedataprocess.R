rm(list = ls())
library(data.table);library(ggplot2);library(gridExtra);library(raster)
library(dplyr); library(SpaDES)
workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
allPSP <- fread(file.path(workPath, "data", "BCtrees.csv"))
unique(allPSP$ld)
# "L"  "I"  "DU" "DP" "V"  "C"
cutPlots <- unique(allPSP[ld == "C", ]$SAMP_ID)
print(cutPlots)
# "70030 G000009" had cut trees, the plot should be removed
allPSP <- allPSP[!(SAMP_ID %in% cutPlots), ]

# check whether the dead status of a tree was the last observation of that tree
deadtrees <- allPSP[ld == "DU",][, .(firstDeadYear=min(meas_yr)), by = uniTreeID]
livetrees <- allPSP[ld != "DU", ][,.(lastLiveYear = max(meas_yr)), by = uniTreeID]
deadtrees <- setkey(deadtrees, uniTreeID)[setkey(livetrees, uniTreeID),
                                          nomatch = 0]
deadBeforeLiveTrees <- deadtrees[lastLiveYear >= firstDeadYear, ]$uniTreeID
print(length(deadBeforeLiveTrees)) # 0 good

# select live trees
livetrees <- allPSP[ld != "DU", ]

# check whether there was missing measurement
range(livetrees$meas_yr) # 1926 to 2013
# hist(livetrees$meas_yr)
allplots <- unique(livetrees$uniPlotID)



myFunction <- function(plotdata){
  missingMeasureTrees <- NULL
  plotCensus <- sort(unique(plotdata$meas_yr))
  treeIDs <- unique(plotdata$uniTreeID)
  for(inditree in treeIDs){
    treedata <- plotdata[uniTreeID == inditree,]
    treecensus <- sort(treedata$meas_yr)
    if(length(treecensus) != length(plotCensus[plotCensus <= max(treecensus) &
                                               plotCensus >= min(treecensus)])){
      missingMeasureTrees <- c(missingMeasureTrees, inditree)
    }
  }
  return(missingMeasureTrees)
}

livetreesList <- list()
for(indiplot in allplots){
  livetreesList[[indiplot]] <- livetrees[uniPlotID == indiplot]
}

cl <- parallel::makeCluster(parallel::detectCores()-1)
parallel::clusterExport(cl, c("myFunction"))
allmissingMearsureTrees <- lapply(livetreesList, function(s) myFunction(plotdata = s))
parallel::stopCluster(cl)

allmissingMearsureTrees <- unlist(allmissingMearsureTrees)
length(allmissingMearsureTrees) # 0 good

# species inspection
livetrees[, speciesLength:=length(unique(species)), by = uniTreeID]
unique(livetrees$speciesLength) # 1 good
livetrees[, speciesLength:=NULL]
livetrees[, orgSpecies:=species]
livetrees[, species:=NA]
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
                                        DBH = livetrees$dbh,
                                        paperSource = "Ung2008")


# manipulate live tree data
livetrees <- livetrees[order(uniTreeID, meas_yr),]
setnames(livetrees, c("meas_yr", "dbh", "biomass"), c("Year", "IniDBH", "IniBiomass"))
livetrees <- livetrees[,c("FinYear", "FinDBH", "FinBiomass", "tempuniTreeID") 
                 := data.table::shift(x = livetrees[,.(Year, IniDBH, IniBiomass, uniTreeID)],
                                      n = 1, fill = NA, type = "lead", give.names = FALSE)]

livetrees <- livetrees[tempuniTreeID == uniTreeID,][,tempuniTreeID := NULL]
livetrees <- livetrees[Year != FinYear,]
livetrees[, measInterval:=FinYear-Year]
range(livetrees$measInterval) # 2 to 35 years
livetrees[, BiomassGR:=(FinBiomass-IniBiomass)/measInterval]

plotCensusMinDBH <- livetrees[,.(MinDBH = min(IniDBH)), by = c("uniPlotID", "Year")]

livetrees91 <- livetrees[IniDBH>=9.1,]
livetrees91[, plotMeasureTime:=length(unique(Year)), by = uniPlotID]
livetrees91[, treeMeasureTime:=length(unique(Year)), by = uniTreeID]

livetrees91[, allCensusLiveTree:="no"]
livetrees91[plotMeasureTime == treeMeasureTime, allCensusLiveTree := "yes"]

livetrees91[, treeMinGR:=min(BiomassGR), by = uniTreeID]

livetrees91[,positiveGrowthTree:="no"]
livetrees91[treeMinGR>0, positiveGrowthTree := "yes"]

allcensuspositive91 <- livetrees91[allCensusLiveTree == "yes" & 
                                     positiveGrowthTree == "yes",]
length(unique(allcensuspositive91$uniTreeID)) # 11606
length(unique(allcensuspositive91$uniPlotID)) # 132

write.csv(livetrees91, file.path(workPath, "data", "forcompetitionIndex.csv"),
          row.names = FALSE)

write.csv(livetrees91, file.path(workPath, "data", "finalBCdata.csv"), 
          row.names = FALSE)

treeinfor <- unique(allcensuspositive91, by = "uniTreeID")
speciesinfor <- treeinfor[,.(NumberOfTree=length(FinDBH)), by = species]

#            species NumberOfTree
# 1:    white spruce         1761
# 2:      alpine fir          266
# 3:  lodgepole pine         5720
# 4:             fir          155
# 5:    black spruce         1259
# 6:     white birch          209
# 7: western hemlock            1
# 8: trembling aspen         2130
# 9:   balsam poplar          103
# 10:     douglas-fir            2





