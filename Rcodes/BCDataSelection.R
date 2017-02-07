# get the BC data
rm(list = ls())
library(data.table);library(ggplot2)
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


# select the plot that trees were mapped
print(unique(selectedPlots$stem_mapped_ind))
# "N" "Y" "" 
selectedPlots <- selectedPlots[stem_mapped_ind == "Y",]
print(length(unique(selectedPlots$SAMP_ID)))
# 198 plots left

# select the plot that had been measured at least for three times
print(sort(unique(selectedPlots$no_meas)))
# 1  2  3  4  8  9 10 12
selectedPlots <- selectedPlots[no_meas>=3,]
print(length(unique(selectedPlots$SAMP_ID)))
# 139 plots left

unique(selectedPlots$sampletype) #  "G" "R" "I"
########################
# TREE LEVEL SELECTION #
########################

BCtreedata <- fread(file.path(workPath,"data",
                              "BC", "Trees.csv"))

treesInSelctedPlots <- BCtreedata[SAMP_ID %in% unique(selectedPlots$SAMP_ID),]

treesInSelctedPlots[,uniTreeID:=paste(SAMP_ID, "_", tree_no, sep = "")]

length(unique(treesInSelctedPlots$uniTreeID)) # 32290 trees

# overall view of the tree data information
# stem mapping for a single plot
range(treesInSelctedPlots$x_coord) # NA
range(treesInSelctedPlots$x_coord, na.rm = T)
# 0 369
# Tree level inspection for angle
treesInSelctedPlots[, ':='(minAngle = min(x_coord),
                           maxAngle = max(x_coord),
                           angleTime = length(unique(x_coord))),
                    by = uniTreeID]

print(unique(treesInSelctedPlots$angleTime)) # 1 2

noMappingTrees <- unique(treesInSelctedPlots[is.na(minAngle) & angleTime == 1,]$uniTreeID)
print(length(noMappingTrees)) # 3054 trees
# remove these trees in from the dataset
treesInSelctedPlots <- treesInSelctedPlots[!(uniTreeID %in% noMappingTrees), ]


# check tree had different angle but all had values
print(setkey(treesInSelctedPlots[minAngle != maxAngle,.(uniTreeID, meas_yr, 
                                                        x_coord, y_coord)],
      uniTreeID, meas_yr))

# uniTreeID meas_yr    x_coord    y_coord
# 1: 69018 G000010_01_0041_1    1996 -3.1761477 -0.3899819
# 2: 69018 G000010_01_0041_1    2013 -3.0868185 -0.3790137
# 3: 69018 G000010_01_0092_1    1976 -5.2610208 -0.5529556
# 4: 69018 G000010_01_0092_1    1986 -5.2610208 -0.5529556
# 5: 69018 G000010_01_0092_1    1996 -5.2610208 -0.5529556
# 6: 69018 G000010_01_0092_1    2013 -5.1450564 -1.0936155
# 7: 69018 G000010_01_0097_1    1976 -7.4223208  2.5557100
# 8: 69018 G000010_01_0097_1    1986 -7.4223208  2.5557100
# 9: 69018 G000010_01_0097_1    1996 -7.4223208  2.5557100
# 10: 69018 G000010_01_0097_1    2013 -7.4087303  2.4072424
# 11: 69018 G000013_01_0021_1    1976  1.9997646  1.9311503
# 12: 69018 G000013_01_0021_1    1986  1.9997646  1.9311503
# 13: 69018 G000013_01_0021_1    1996  1.9997646  1.9311503
# 14: 69018 G000013_01_0021_1    2013  2.0357316  1.9658832
# 15: 69018 G000013_01_0041_1    1976  0.8809511 -0.7392058
# 16: 69018 G000013_01_0041_1    1986  0.8809511 -0.7392058
# 17: 69018 G000013_01_0041_1    1996  0.8809511 -0.7392058
# 18: 69018 G000013_01_0041_1    2013  0.9115929 -0.7649173
# 19: 69018 G000013_01_0271_1    1976 -1.0793922 -0.1516987
# 20: 69018 G000013_01_0271_1    1986 -1.0793922 -0.1516987
# 21: 69018 G000013_01_0271_1    1996 -1.0793922 -0.1516987
# 22: 69018 G000013_01_0271_1    2013 -1.0100734 -0.1419566

# correct the x_coord and y_coord for these 6 trees
treesInSelctedPlots[uniTreeID == "69018 G000010_01_0041_1",
                    ':='(x_coord = -3.1761477, y_coord = -0.3899819)]
treesInSelctedPlots[uniTreeID == "69018 G000010_01_0092_1",
                    ':='(x_coord = -5.2610208, y_coord = -0.5529556)]
treesInSelctedPlots[uniTreeID == "69018 G000010_01_0097_1",
                    ':='(x_coord = -7.4223208, y_coord = 2.5557100)]
treesInSelctedPlots[uniTreeID == "69018 G000013_01_0021_1",
                    ':='(x_coord = 1.9997646, y_coord = 1.9311503) ]
treesInSelctedPlots[uniTreeID == "69018 G000013_01_0041_1",
                    ':='(x_coord = 0.8809511, y_coord = -0.7392058)]
treesInSelctedPlots[uniTreeID == "69018 G000013_01_0271_1",
                    ':='(x_coord = -1.0793922, y_coord = -0.1419566)]


# for the trees that had 2 angle information and min or max value was NA
# the angle was correct by assign the non-NA value for each tree

treesInSelctedPlots[angleTime == 2,':='(x_coord = min(x_coord, na.rm = T)),
                    by = uniTreeID]

length(unique(treesInSelctedPlots$uniTreeID)) # 29236 == 32290-3054

# backCheck
range(treesInSelctedPlots$x_coord) # -75.51560  65.39016
treesInSelctedPlots[, ':='(angleTime = length(unique(x_coord))),
                    by = uniTreeID]
unique(treesInSelctedPlots$angleTime) # 1
set(treesInSelctedPlots, , c("minAngle", "maxAngle", "angleTime"), NULL)



# same procedure for distance
treesInSelctedPlots[, ':='(minDistance = min(y_coord),
                           maxDistance = max(y_coord),
                           DistanceTime = length(unique(y_coord))),
                    by = uniTreeID]

print(unique(treesInSelctedPlots$DistanceTime)) # 1 2

noMappingTrees <- unique(treesInSelctedPlots[is.na(minDistance) & DistanceTime == 1,]$uniTreeID)
print(length(noMappingTrees)) # 0 trees

# check tree had different Distance but all had values
print(setkey(treesInSelctedPlots[minDistance != maxDistance,
                                 .(uniTreeID, meas_yr, y_coord, tree_err)],
             uniTreeID, meas_yr)) # 0 row


# for the trees that had 2 Distance information and min or max value was NA
# the Distance was correct by assign the non-NA value for each tree

treesInSelctedPlots[DistanceTime == 2,
                    ':='(y_coord = min(y_coord, na.rm = T)),
                    by = uniTreeID]

length(unique(treesInSelctedPlots$uniTreeID)) # 29236 == 32290-3054

# backCheck
range(treesInSelctedPlots$y_coord) # -70.12236  48.51261
# the distance of 96 m did not make sense, need to be further confirmation/correction
treesInSelctedPlots[, ':='(DistanceTime = length(unique(y_coord))),
                    by = uniTreeID]
unique(treesInSelctedPlots$DistanceTime) # 1
set(treesInSelctedPlots, , c("minDistance", "maxDistance", "DistanceTime"), NULL)

mappingCheck <- treesInSelctedPlots[,.(SAMP_ID, plot_no, uniTreeID, 
                                       x_coord, y_coord)]
mappingCheck <- unique(mappingCheck, by = "uniTreeID")

a <- ggplot(data = mappingCheck, aes(x = x_coord, y = y_coord))+
  geom_point()

print(range(treesInSelctedPlots$dbh))
# 4.0 94.3 which were consistent with VRI sampling protocal (trees were defined bigger than 4 cm)


