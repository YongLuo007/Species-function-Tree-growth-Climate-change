## load data from sas files
## select data based on 1) natural stands or fire
##                      2) plot has location
##                      3) trees have been stem-mapped
## save them to data folder in rds format


rm(list = ls())
library(haven); library(data.table); library(dplyr)
library(quickPlot)
devtools::load_all("D:/GitHub/FAIBCompiler")
gys_data_path <- "./data/sasfiles"
rem_data_path <- "./data/bc_remeasurements"
output_datapath <- "./data"

sampledata_gys <- read_sas(file.path(gys_data_path, "sample.sas7bdat")) %>%
  data.table
names(sampledata_gys) <- toupper(names(sampledata_gys))

sampledata_gys <- sampledata_gys[,.(SAMP_ID, EST_YR = ESTABLISHMENT_YR, 
                                    SAMP_STATUS = SAMPLE_STATUS_CODE, 
                                    SAMP_METHOD = SAMPLING_METHOD_CODE,
                                    STAND_ORG = STAND_ORIGIN_CODE,
                                    SAMPLE_TYPE_CODE, 
                                    UTM_ZONE, UTM_EASTING, UTM_NORTHING,
                                    TIEPOINT_UTM_EASTING, TIEPOINT_UTM_NORTHING,
                                    STEM_MAPPED_IND, SOIL_MOIST_REGIME_DOM,
                                    SOIL_MOIST_REGIME_RANGE, SOIL_MOIST_REGIME_SUB_DOM,
                                    SOIL_NUTRIENT_REGIME_SINGLE,
                                    TREATED_STAND_PLANTATION_YR)]


sampledata_rem <- readRDS(file.path(rem_data_path, "samples.rds")) %>%
  data.table
names(sampledata_rem) <- toupper(names(sampledata_rem))


sampledata_rem[, samp_len := length(DATA_SRCE), by = "SAMP_ID"]
unique(sampledata_rem[samp_len >1]$SAMP_ID)

sampledata_rem <- sampledata_rem[,.(SAMP_ID, EST_YR = YR_EST, 
                                    SAMP_STATUS = SAMP_STS, 
                                    SAMP_METHOD = SAMP_MTD,
                                    STAND_ORG = STND_ORG,
                                    SAMPLE_TYPE_CODE = NA,
                                    UTM_ZONE, UTM_EASTING, UTM_NORTHING,
                                    TIEPOINT_UTM_EASTING = NA, TIEPOINT_UTM_NORTHING = NA,
                                    STEM_MAPPED_IND = NA, SOIL_MOIST_REGIME_DOM = NA,
                                    SOIL_MOIST_REGIME_RANGE = NA, SOIL_MOIST_REGIME_SUB_DOM = NA,
                                    SOIL_NUTRIENT_REGIME_SINGLE = NA,
                                    TREATED_STAND_PLANTATION_YR = TREATMENT_TYPE_CODE)]
sampledata_rem <- unique(sampledata_rem)
nrow(sampledata_rem) == length(unique(sampledata_rem$SAMP_ID))


sampledata_rem_add <- sampledata_rem[!(SAMP_ID %in% unique(sampledata_gys$SAMP_ID)),]

sampledata <- rbind(sampledata_gys, sampledata_rem_add)

saveRDS(sampledata, file.path(".", "data", "samples_all.rds"))




# 4. select stands that have minimum 2 measurements
sample_meas_gys <- read_sas(file.path(gys_data_path, "sample_measurement.sas7bdat")) %>%
  data.table
names(sample_meas_gys) <- toupper(names(sample_meas_gys))
sample_meas_gys[, MEAS_YR := as.numeric(substr(MEAS_DATE, 1, 4))]

sample_meas_gys_test <- unique(sample_meas_gys[,.(SAMP_ID, MEAS_NO, MEAS_DATE, POLYGON_NO)],
                               by = c("SAMP_ID", "MEAS_NO", "MEAS_DATE"))

sample_meas_gys_test[, obslen1 := length(POLYGON_NO),
                     by = c("SAMP_ID", "MEAS_NO")]

sample_meas_gys_test[, obslen2 := length(POLYGON_NO),
                     by = c("SAMP_ID", "MEAS_DATE")]
sample_meas_gys_test[obslen1 > 1 | obslen2 > 1]
#          SAMP_ID MEAS_NO  MEAS_DATE POLYGON_NO obslen1 obslen2
# 1: 62014 G000520      02 1993-09-01         NA       1       2
# 2: 62014 G000520      03 1993-09-01          0       1       2
# 3: 03005CT000602      04 2003-12-01          0       1       2
# 4: 03005GT000645      04 2003-11-25          0       1       2
# 5: 03005CT000602      00 2003-12-01       1754       1       2
# 6: 03005GT000645      00 2003-11-25          0       1       2


sample_meas_gys <- unique(sample_meas_gys, 
                          by = c("SAMP_ID", "MEAS_NO"))
sample_meas_gys[, samp_meas := paste0(SAMP_ID, "_", MEAS_NO)]


# true
sample_meas_rem <- readRDS(file.path(rem_data_path, "sample_meas.rds")) %>%
  data.table
names(sample_meas_rem) <- toupper(names(sample_meas_rem))



sample_meas_rem[, MEAS_NO := measNumCorrect_PSP(sampleID = SAMP_ID,
                                                measureNumber = MEAS_NO)]
sample_meas_rem[, samp_meas := paste0(SAMP_ID, "_", MEAS_NO)]
sample_meas_both <- merge(sample_meas_gys[,.(samp_meas, MEAS_DATE_GYS = gsub("-", "", MEAS_DATE))],
                          sample_meas_rem[,.(samp_meas, MEAS_DATE_REM = MEAS_DTX)],
                          by = "samp_meas")
sample_meas_both[MEAS_DATE_GYS != MEAS_DATE_REM,]

sample_meas_rem_test <- unique(sample_meas_rem[,.(SAMP_ID, MEAS_NO, MEAS_DATE = MEAS_DTX,
                                                  YEAR_INTERVAL)],
                               by = c("SAMP_ID", "MEAS_NO", "MEAS_DATE"))

sample_meas_rem_test[, obslen1 := length(YEAR_INTERVAL),
                     by = c("SAMP_ID", "MEAS_NO")]

sample_meas_rem_test[, obslen2 := length(YEAR_INTERVAL),
                     by = c("SAMP_ID", "MEAS_DATE")]
sample_meas_rem_test[obslen1 > 1 | obslen2 > 1]
#          SAMP_ID MEAS_NO MEAS_DATE YEAR_INTERVAL obslen1 obslen2
# 1: 55015 G000001      01  20130611            17       2       1
# 2: 55015 G000001      01  20130919            18       2       1

sample_meas_rem[SAMP_ID == "55015 G000001" & MEAS_NO == "01"]

sample_meas_rem <- sample_meas_rem[!(samp_meas == "55015 G000001_01" & MEAS_DTX == "20130919"),]

sample_meas_rem <- unique(sample_meas_rem, by = "samp_meas")

sample_meas_rem <- sample_meas_rem[!(samp_meas %in% sample_meas_gys$samp_meas),]


sample_meas <- rbind(sample_meas_gys[,.(SAMP_ID, MEAS_DATE = as.character(MEAS_DATE),
                                        MEAS_NO, SAMPLE_BREAK_POINT_IND, 
                                        SAMPLE_BREAK_POINT, SAMPLE_MIN_DBH,
                                        STAND_HIST_DISTURBANCE_CD, REMARKS)],
                     sample_meas_rem[,.(SAMP_ID, MEAS_DATE = MEAS_DTX, MEAS_NO,
                                        SAMPLE_BREAK_POINT_IND = NA,
                                        SAMPLE_BREAK_POINT = NA,
                                        SAMPLE_MIN_DBH = NA,
                                        STAND_HIST_DISTURBANCE_CD = STAND_PEST,
                                        REMARKS = MEMO)])
nrow(sample_meas) == nrow(unique(sample_meas, by = c("SAMP_ID", "MEAS_NO")))

sample_meas[, MEAS_YR := as.numeric(substr(MEAS_DATE, 1, 4))]
range(sample_meas$MEAS_YR)
sample_meas[MEAS_YR > 2018,]
sample_meas[SAMP_ID == "63036 G000007" & MEAS_YR == 2103,
            MEAS_YR := 2013]
saveRDS(sample_meas, file.path(output_datapath, "samples_measure_all.rds"))



plotdata <- read_sas(file.path(gys_data_path, "plot.sas7bdat")) %>%
  data.table
names(plotdata) <- toupper(names(plotdata))
plotdata <- plotdata[,.(SAMP_ID, PLOT_NO, SLOPE_POSITION_CODE, PLOT_ASPECT, PLOT_SLOPE,
                       PLOT_ELEVATION, PLOT_RADIUS, PLOT_LENGTH, PLOT_WIDTH)]



plotdata[, NO_PLOT := length(PLOT_LENGTH), 
         by = c("SAMP_ID", "PLOT_NO")]

plotdata[NO_PLOT > 1]
plotdata[, NO_PLOT := NULL]

saveRDS(plotdata, file.path(output_datapath, "plots.rds"))



## what is subplot data, what difference between subplot and plot
subplotdata <- read_sas(file.path(gys_data_path, "subplot.sas7bdat")) %>%
  data.table
names(subplotdata) <- toupper(names(subplotdata))
subplotdata <- subplotdata[,.(SAMP_ID, MEAS_NO, PLOT_NO, SUBPLOT_RADIUS)]

saveRDS(subplotdata, file.path(output_datapath, "subplot_all.rds"))

treedata_gys <- read_sas(file.path(gys_data_path, "tree_for_yong.sas7bdat")) %>% data.table
names(treedata_gys) <- toupper(names(treedata_gys))
treedata_gys <- treedata_gys[,.(SAMP_ID, PLOT_NO, TREE_NO, SPECIES = SPECIES_CODE,
                       OUT_OF_PLOT_IND, TREE_STEM_MAP_BEARING, TREE_STEM_MAP_SLOPE_DISTANCE,
                       TREE_STEM_MAP_HORIZ_DISTANCE, NATURAL_OR_PLANTED)]
nrow(treedata_gys) == nrow(unique(treedata_gys, by = c("SAMP_ID", "PLOT_NO", "TREE_NO")))


treedata_rem <- readRDS(file.path(rem_data_path, "trees.rds")) %>% data.table
treedata_rem <- treedata_rem[,.(SAMP_ID, PLOT_NO, TREE_NO, SPECIES_rem = SPECIES,
                       TREE_STEM_MAP_BEARING_rem = STEM_MAP_BEARING,
                       TREE_STEM_MAP_SLOPE_DISTANCE_rem = STEM_MAP_SLOPE_DISTANCE)]
nrow(treedata_rem) == nrow(unique(treedata_rem, by = c("SAMP_ID", "PLOT_NO", "TREE_NO")))
treedata_rem <- treedata_rem[order(SAMP_ID, PLOT_NO, TREE_NO),]
treedata_rem <- unique(treedata_rem, 
                       by = c("SAMP_ID", "PLOT_NO", "TREE_NO"))


treedata <- merge(treedata_gys, treedata_rem, 
                  by = c("SAMP_ID", "PLOT_NO", "TREE_NO"),
                  all = TRUE)
treedata[!is.na(SPECIES_rem),
         ':='(SPECIES = SPECIES_rem,
              TREE_STEM_MAP_BEARING = TREE_STEM_MAP_BEARING_rem,
              TREE_STEM_MAP_SLOPE_DISTANCE = TREE_STEM_MAP_SLOPE_DISTANCE_rem)]
unique(treedata$SPECIES)
# [1] "SS"  "HW"  "BA"  "CW"  "PL"  "B"   "S"   "DR" 
# [9] "TW"  "W"   "BG"  "FD"  "HM"  "FDC" "H"   "YC" 
# [17] "PW"  "AT"  "VB"  "XC"  "L"   "LW"  "MB"  "XH" 
# [25] "KC"  "AC"  "EP"  "E"   "GP"  "RA"  "ZH"  "DG" 
# [33] "BL"  "UP"  "DM"  "MV"  "PLC" "SE"  "PY"  "JR" 
# [41] "MR"  "ES"  "SX"  "EW"  "SW"  "MS"  "2M"  "EE" 
# [49] "WS"  "WB"  "BC"  "PA"  "WD"  "WP"  "AD"  "FDI"
# [57] "SXW" "SB"  "V"   "4.2" "M"   "Bl"  "ACT" "LT" 
# [65] "PJ" 
treedata[,':='(SPECIES_rem = NULL,
              TREE_STEM_MAP_BEARING_rem = NULL,
              TREE_STEM_MAP_SLOPE_DISTANCE_rem = NULL)]
treedata[!(TREE_STEM_MAP_BEARING %in% c(0, NA)) & 
                              !(TREE_STEM_MAP_SLOPE_DISTANCE %in% c(0, NA)),
         MAPPED := TRUE]
treedata[is.na(MAPPED), MAPPED := FALSE]

saveRDS(treedata, file.path(output_datapath, "treedata_all.rds"))





treemeasdata_gys0 <- read_sas(file.path(gys_data_path, "tree_meas_for_yong.sas7bdat")) %>% data.table
names(treemeasdata_gys0) <- toupper(names(treemeasdata_gys0))
unique(treemeasdata_gys0$TREE_CLASS_CODE)
## assign TREE_CLASS_CODE as "1", as it is not dead

treemeasdata_gys0[TREE_CLASS_CODE == "", TREE_CL := "1"]

treemeasdata_gys <- treemeasdata_gys0[,.(SAMP_ID, PLOT_NO, TREE_NO, MEAS_NO, 
                               CROWN_CLASS_CODE,
                               TREE_CLASS_CODE,
                               HT_MEASUREMENT_STATUS_CODE, 
                               MEAS_HT,  
                               DBH = DIAM_AT_13M, 
                               SUB_PLOT_TREE_IND, 
                               DAM_AGENT1 = DAMAGE_AGENT_TYPE_CODE1, 
                               DAM_AGENT2 = DAMAGE_AGENT_TYPE_CODE2,
                               BORING_AGE,
                               BORING_HT,
                               AGE_CORRECTION,
                               TOTAL_AGE)]

nrow(treemeasdata_gys) == nrow(unique(treemeasdata_gys, 
                                      by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO")))



treemeasdata_rem0 <- readRDS(file.path(rem_data_path, "tree_meas.rds")) %>%
  data.table
names(treemeasdata_rem0) <- toupper(names(treemeasdata_rem0))
unique(treemeasdata_rem0[TR_CLASS == "0",]$DEAD_OR_BROKEN_TOP) # is not dead
treemeasdata_rem0[TR_CLASS == "0", TR_CLASS := "1"]

treemeasdata_rem0[, MEAS_NO := measNumCorrect_PSP(sampleID = SAMP_ID,
                                                 measureNumber = MEAS_NO)]
nrow(treemeasdata_rem0) == nrow(unique(treemeasdata_rem0, 
                                      by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO")))



treemeasdata_rem <- treemeasdata_rem0[,
                             .(SAMP_ID, PLOT_NO, TREE_NO, MEAS_NO, 
                               CROWN_CLASS_CODE = CROWN,
                               TREE_CLASS_CODE = TR_CLASS,
                               HT_MEASUREMENT_STATUS_CODE = NA, 
                               MEAS_HT = HT_MEAS, 
                               DBH, SUB_PLOT_TREE_IND = NA,
                               DAM_AGENT1 = DAMAGE_AGENT_SPECIES,
                               DAM_AGENT2 = DAMAGE_AGENT2,
                               BORING_AGE, BORING_HT = BORING_HEIGHT,
                               AGE_CORRECTION = AGE_CORRECT,
                               TOTAL_AGE = AGE_TOT)]

treemeasdata_rem <- unique(treemeasdata_rem,
                           by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO"))

nrow(treemeasdata_rem) == nrow(unique(treemeasdata_rem, by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO")))
treemeasdata_rem <- treemeasdata_rem[order(SAMP_ID, PLOT_NO, TREE_NO, MEAS_NO, -MEAS_HT),]
treemeasdata_rem <- unique(treemeasdata_rem,
                           by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO"))






treemeasdata <- rbindlist(list(treemeasdata_gys[, GYS := TRUE], treemeasdata_rem[, GYS := FALSE]))
nrow(treemeasdata) == nrow(unique(treemeasdata, 
                                      by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO")))
treemeasdata[, obslen := length(DBH),
             by = c("SAMP_ID", "PLOT_NO", "TREE_NO", "MEAS_NO")]
treemeasdata <- treemeasdata[!(obslen == 2 & GYS == FALSE),]

## based on Rene, tree class code is defined as 1...6
treemeasdata[TREE_CLASS_CODE %in% c("1", "2", "5"), Dead := 0]
treemeasdata[TREE_CLASS_CODE %in% c("3", "4"), Dead := 1]
treemeasdata[TREE_CLASS_CODE %in% c("6"), Dead := 3] # dead due to cut
treemeasdata[, obslen := NULL]


saveRDS(treemeasdata, file.path(output_datapath, "treemeasdata_all.rds"))

