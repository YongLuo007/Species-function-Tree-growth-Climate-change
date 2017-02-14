
# get the BC data
rm(list = ls())
library(data.table);library(ggplot2);library(gridExtra);library(raster)
library(dplyr); library(spatstat); library(SpaDES)
workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
BCPlotInfor <- fread(file.path(workPath,"data", "BC", "StandInformation.csv"))
print(length(unique(BCPlotInfor$SAMP_ID)))
# 3793 plots totally

########################
# PLOT LEVEL SELECTION #
########################

# select the plots that regenerated from fire or natural
print(unique(BCPlotInfor$stnd_org))
#  ""  "N" "P" "F" "R"
selectedPlots <- BCPlotInfor[stnd_org %in% c("F", "N"),]
print(length(unique(selectedPlots$SAMP_ID)))
# 1558 plots left

# select the plots that were untreated
print(unique(selectedPlots$treatment))
# "UNTREATED" "THINNED"
selectedPlots <- selectedPlots[treatment == "UNTREATED",]
print(length(unique(selectedPlots$SAMP_ID)))
# 1526 plots left

selectedPlots <- selectedPlots[dbhlimit_tag<=4,]
print(length(unique(selectedPlots$SAMP_ID))) # 430
# # select the plot that trees were mapped
# print(unique(selectedPlots$stem_mapped_ind))
# # "N" "Y" "" 
# selectedPlots <- selectedPlots[stem_mapped_ind == "Y",]
# print(length(unique(selectedPlots$SAMP_ID)))
# # 198 plots left

# select the plot that had been measured at least for three times
selectedPlots[, Meas_time:=length(unique(meas_yr)), by = SAMP_ID]

print(sort(unique(selectedPlots$Meas_time)))
# 1  2  3  4  8  9 10 12
selectedPlots <- selectedPlots[Meas_time>=3,]
print(length(unique(selectedPlots$SAMP_ID)))
# 304 plots left


selectedPlots[,locTime:=length(unique(utm_zone)), by = SAMP_ID]
unique(selectedPlots$locTime) #1
selectedPlots <- selectedPlots[!is.na(utm_zone),]
print(length(unique(selectedPlots$SAMP_ID))) # 225

unique(selectedPlots$sampletype) #  "G" "R" "I"
########################
# TREE LEVEL SELECTION #
########################

# select the plots that had location information


unselectedPlots <- BCPlotInfor[!(SAMP_ID %in% unique(selectedPlots$SAMP_ID)),]

length(unique(unselectedPlots$SAMP_ID)) # 3660
unselectedPlots_fixed <- unselectedPlots[plot_typ == "F"]
length(unique(unselectedPlots_fixed$SAMP_ID)) # 2154
unselectedPlots_fixed <- unselectedPlots_fixed[stnd_org %in% c("F", "N"),]
length(unique(unselectedPlots_fixed$SAMP_ID)) # 1425


unselectedPlots_fixed[,areaLength:=length(unique(area_pm)), by = SAMP_ID]

unique(unselectedPlots_fixed$areaLength) #

unselectedPlots_fixed <- unselectedPlots_fixed[no_meas>=2,]
length(unique(unselectedPlots_fixed$SAMP_ID)) # 686

unselectedPlots_fixed <- unselectedPlots_fixed[!is.na(utm_zone),]
length(unique(unselectedPlots_fixed$SAMP_ID)) # 590


oneplot <- unique(selectedPlots$SAMP_ID)[1]
oneplotInFor <- selectedPlots[SAMP_ID == oneplot, ]
oneplotInForSize <- unique(oneplotInFor$area_pm)*10000
BCtreedata <- fread(file.path(workPath,"data",
                              "BC", "Trees.csv"))

# bctreedataWithoutMapping <- BCtreedata[SAMP_ID %in% unique(unselectedPlots_fixed$SAMP_ID)]
bctreedataWithoutMapping <- BCtreedata[SAMP_ID == oneplot,]
bctreedataWithoutMapping[, uniTreeID:=paste(SAMP_ID, tree_no, sep = "")]
bctreedataWithoutMapping <- bctreedataWithoutMapping[ld != "DU",]

# species inspection
bctreedataWithoutMapping[, speciesLength:=length(unique(species)), by = uniTreeID]
unique(bctreedataWithoutMapping$speciesLength) # 1 good
bctreedataWithoutMapping[, speciesLength:=NULL]
bctreedataWithoutMapping[, orgSpecies:=species]
bctreedataWithoutMapping[, species:=NA]
bctreedataWithoutMapping[orgSpecies == "SB", species := "black spruce"]
bctreedataWithoutMapping[orgSpecies == "LT", species := "tamarack larch"]
bctreedataWithoutMapping[orgSpecies == "SW", species := "white spruce"]
bctreedataWithoutMapping[orgSpecies == "EP", species := "white birch"]
bctreedataWithoutMapping[orgSpecies == "PLI", species := "lodgepole pine"]
bctreedataWithoutMapping[orgSpecies == "ACB", species := "balsam poplar"]
bctreedataWithoutMapping[orgSpecies == "S", species := "black spruce"]
# S for spruce in original doc. assume S is black spruce
bctreedataWithoutMapping[orgSpecies == "PL", species := "lodgepole pine"]
bctreedataWithoutMapping[orgSpecies == "ACT", species := "black cottonwood"]
bctreedataWithoutMapping[orgSpecies == "AT", species := "trembling aspen"]
bctreedataWithoutMapping[orgSpecies == "E", species := "white birch"]
# E for birch in original doc. assume S is white birch
# bctreedataWithoutMapping[orgSpecies == "XC", species := "unknow conifer"]
bctreedataWithoutMapping[orgSpecies == "BL", species := "alpine fir"]
bctreedataWithoutMapping[orgSpecies == "EA", species := "white birch"]
# EA is alaka paper birch, assume it is white birch here
bctreedataWithoutMapping[orgSpecies == "AC", species := "balsam poplar"]
# bctreedataWithoutMapping[orgSpecies == "X", species := "unknown"]
bctreedataWithoutMapping[orgSpecies == "B", species := "fir"]
bctreedataWithoutMapping[orgSpecies == "W", species := "willow"]
bctreedataWithoutMapping[orgSpecies == "DR", species := "red alder"]
bctreedataWithoutMapping[orgSpecies == "DM", species := "mountain alder"]
bctreedataWithoutMapping[orgSpecies == "PJ", species := "jack pine"]
bctreedataWithoutMapping[orgSpecies == "ZH", species := "hardwood"]
bctreedataWithoutMapping[orgSpecies == "SXW", species := "white spruce"]
# SXW is hybrid between engelmann and white spruce
bctreedataWithoutMapping[orgSpecies == "XH", species := "hardwood"]
bctreedataWithoutMapping[orgSpecies == "SX", species := "white spruce"]
# SX is hybrid
bctreedataWithoutMapping[orgSpecies == "A", species := "trembling aspen"]
# A is aspen cottonwood and poplar, assume it is trembling aspen
bctreedataWithoutMapping[orgSpecies == "P", species := "lodgepole pine"]
# P is pine orgSpecies, assume it is lodgepole pine
bctreedataWithoutMapping[orgSpecies == "MR", species := "red maple"]
# MR could not find, assume it is red maple
bctreedataWithoutMapping[orgSpecies == "SE", species := "engelmann spruce"]
bctreedataWithoutMapping[orgSpecies == "FD", species := "douglas-fir"] 
bctreedataWithoutMapping[orgSpecies == "BA", species := "amabalis fir"] 
bctreedataWithoutMapping[orgSpecies == "CW", species := "western redcedar"]
bctreedataWithoutMapping[orgSpecies == "HW", species := "western hemlock"]
bctreedataWithoutMapping[orgSpecies == "FDI", species := "douglas-fir"]
# bctreedataWithoutMapping[orgSpecies == "SXE", species := "unknown"]
bctreedataWithoutMapping[orgSpecies == "D", species := "red alder"]
# D is alder, assume red alder
bctreedataWithoutMapping[orgSpecies == "H", species := "western hemlock"]
bctreedataWithoutMapping[orgSpecies == "MV", species := "vine maple"] 
bctreedataWithoutMapping[orgSpecies == "HM", species := "mountain hemlock"] 
bctreedataWithoutMapping[orgSpecies == "EXP", species := "white birch"]
bctreedataWithoutMapping[orgSpecies == "WS", species := "scoulers willow"]
bctreedataWithoutMapping[orgSpecies == "AX", species := "trembling aspen"]

# 
nrow(bctreedataWithoutMapping[is.na(species),]) # 0 good
bctreedataWithoutMapping <- bctreedataWithoutMapping[!is.na(species),]
# assign biomass
source(file.path(workPath, "Rcodes",  "Rfunctions", "biomassCalculation.R"))
bctreedataWithoutMapping$biomass <- biomassCalculation(species = bctreedataWithoutMapping$species,
                                        DBH = bctreedataWithoutMapping$dbh,
                                        paperSource = "Ung2008")



# oneplotonecensusdata <- BCtreedata[SAMP_ID == oneplot,.(SAMP_ID, dbh, meas_yr, tree_no)]


oneplotonecensusdata <- bctreedataWithoutMapping[meas_yr == 1971,]


radiusOne <- sqrt(oneplotInForSize/pi)
points360 <- seq(0, 2 * pi, length.out = 360)


theplot <- Polygons(list(Polygon(cbind(radiusOne * sin(points360),
                                              radiusOne * cos(points360)))),
                           ID = 1)
theplot <- SpatialPolygons(list(theplot))
DBHCutLength <- 5
oneplotonecensusdata <- oneplotonecensusdata[order(dbh),]
oneplotonecensusdata[,DBHrank:=1:length(dbh)]
oneplotonecensusdata[, DBHclass:=cut(DBHrank, seq(min(DBHrank), max(DBHrank),
                                                  length = DBHCutLength),
                                     labels = paste("DBH", 
                                                    1:(DBHCutLength-1), sep = ""),
                                     include.lowest = TRUE)]
for(i in 1:(DBHCutLength)){
  indiDBHclass <- oneplotonecensusdata[DBHclass == paste("DBH", i, sep = "")]
  NofTree <- round(nrow(indiDBHclass)*10000/oneplotInForSize)
  pts <- data.table(data.frame(rpoint(NofTree), stringsAsFactors = FALSE))
  pts[,':='(x = 100*x-50, y = 100*y-50,
            tree_no = sample(unique(indiDBHclass$tree_no), size = length(x),
                               replace = TRUE))]
  ptss <- setkey(pts, tree_no)[setkey(indiDBHclass[,.(tree_no, dbh, 
                                                      meas_yr, SAMP_ID)],
                                      tree_no),
                                nomatch = 0]
  if(i == 1){
    alltreemapping <- ptss
  } else {
    alltreemapping <- rbind(alltreemapping, ptss)
  }
}

alltreemapping1 <- alltreemapping[,.(x, y, OrgTreeNumber = tree_no,
                                    TreeNumber = 1:length(x), Year = meas_yr,
                                    DBH = dbh,
                                    PlotNumber = SAMP_ID, 
                                    Species = "Jack pine",
                                    Biomass = runif(n = length(x), min = 3, max = 5))]
source(file.path(workPath, "Rcodes", "Rfunctions", "HIndexGenerator.R"))
CIdata <- HIndexGenerator(data = alltreemapping1,
                              maxRadius = 12.62,
                              sizeIndex = "Biomass",
                              distanceWeight = 1, 
                              sizeWeight = 1,
                              assymetricScale = "Rescale")
b <- setkey(alltreemapping1, TreeNumber)[setkey(a[,.(TreeNumber, H = H_DW1_SW1)])]

theplotArea <- fortify(theplot, region = "ID") %>% data.table



figure <- ggplot(data = alltreemapping, aes(x = x, y = y))+
  geom_point(aes(col = dbh))+
  geom_path(data = theplotArea, 
            aes(x = long, y = lat, group = group), col = "blue", linetype = 1)





plot(pts)


