rm(list = ls())
library(data.table);library(ggplot2);library(gridExtra);library(raster)
library(dplyr); library(SpaDES)
devtools::load_all("D:/GitHub/FAIBBase")
minmeaslen <- 3
DBH_threshold <- 9.1
alltreemeas <- readRDS(file.path(".", "data", "treemeasdata_all.rds")) %>% data.table

alltrees <- readRDS(file.path(".", "data", "treedata_all.rds")) %>% data.table


samples <- readRDS(file.path(".", "data", "samples_all.rds")) %>% data.table

unique(samples$STAND_ORG)
# [1] "N" "P" "R" "F" "" 
# 1. select natural (N), fire (F) and R stands 
samples1 <- samples[STAND_ORG %in% c("N", "F", "R"),]

# 2. select untreated stands
samples1 <- samples1[TREATED_STAND_PLANTATION_YR %in% c(NA, 0, ""),]

# 3. select stands have location
samples1 <- samples1[!is.na(UTM_ZONE) & !is.na(UTM_EASTING) & !is.na(UTM_NORTHING),]
nrow(samples1) == length(unique(samples1$SAMP_ID))

unique(substr(samples1$SAMP_ID, 7, 7))
samples1 <- samples1[substr(SAMP_ID, 7, 7) %in% c("R", "G"),]


samp_meas <- readRDS(file.path(".", "data", "samples_measure_all.rds")) %>% data.table

samp_meas1 <- samp_meas[SAMP_ID %in% samples1$SAMP_ID,]

nrow(samp_meas1) == nrow(unique(samp_meas1, 
                                by = c("SAMP_ID", "MEAS_NO")))
# true
samp_meas1[, meas_len := length(MEAS_DATE),
           by = c("SAMP_ID")]

samp_meas2 <- samp_meas1[meas_len >= minmeaslen,]
length(unique(samp_meas2$SAMP_ID)) # 3893

samp_meas2[, samp_meas := paste0(SAMP_ID, "-", MEAS_NO)]

alltreemeas[, samp_meas := paste0(SAMP_ID, "-", MEAS_NO)]

tree_meas1 <- alltreemeas[samp_meas %in% samp_meas2$samp_meas,]

tree_meas1 <- merge(tree_meas1, alltrees[, INTREE := TRUE],
                    by = c("SAMP_ID", "PLOT_NO", "TREE_NO"),
                    all.x = TRUE)
nrow(tree_meas1[is.na(INTREE)]) # 0
tree_meas1[, samp_meas := NULL]
tree_meas1 <- merge(tree_meas1, samp_meas2[,.(SAMP_ID, MEAS_DATE, MEAS_NO,
                                              SAMPLE_BREAK_POINT_IND, SAMPLE_BREAK_POINT,
                                              MEAS_YR)],
                    by = c("SAMP_ID", "MEAS_NO"),
                    all.x = TRUE)


# # true
# mapped_smry_plot <- treedata[,.(Total_Tree = length(SPECIES),
#                                 Mapped_tree = sum(MAPPED)),
#                              by = c("SAMP_ID", "PLOT_NO")]
# mapped_smry_plot[, Mapped_Percent := Mapped_tree/Total_Tree]
# mapped_smry_plot[Mapped_Percent >= 0.80, Mapped_Plot := TRUE]
# mapped_smry_plot[is.na(Mapped_Plot), Mapped_Plot := FALSE]
# treedata <- merge(treedata, mapped_smry_plot[,.(SAMP_ID, PLOT_NO, Mapped_Percent, Mapped_Plot)],
#                   by = c("SAMP_ID", "PLOT_NO"),
#                   all.x = TRUE)


agedata <- tree_meas1[,.(SAMP_ID, PLOT_NO, TREE_NO, SPECIES, 
                         MEAS_NO, MEAS_DATE, BORING_AGE, BORING_HT,
                         AGE_CORRECTION, TOTAL_AGE)]
agedata <- agedata[!(BORING_AGE %in% c(NA, 0) & 
                       BORING_HT %in% c(NA, 0) &
                       AGE_CORRECTION %in% c(NA, 0) & 
                       TOTAL_AGE %in% c(NA, 0)),]

allPSP <- data.table::copy(tree_meas1)
allPSP[, unitreeid := paste0(SAMP_ID, "_", PLOT_NO, "_", TREE_NO)]
allPSP[Dead == 3, Dead := 1]
allPSP[is.na(Dead), Dead := 0]

checkld_results <- checkLD_remeas(subjectID = allPSP$unitreeid,
                                  measNo = allPSP$MEAS_NO,
                                  LDStatus = allPSP$Dead,
                                  liveCode = 0,
                                  deadCode = 1)

ldissuetrees <- allPSP[unitreeid %in% checkld_results[pass == FALSE,]$subjectID,
                       .(unitreeid, MEAS_NO, DBH, Dead)]
nrow(checkld_results[pass == FALSE,]) # 194 trees

## assume the first dead is the tree dead
deadtrees <- allPSP[Dead == 1,]
deadtrees <- deadtrees[,.(minmeasno = min(MEAS_NO)),
                       by = "unitreeid"]
allPSP <- merge(allPSP, deadtrees,
                by = "unitreeid",
                all.x = TRUE)

allPSP <- allPSP[is.na(minmeasno) | MEAS_NO <= minmeasno,]
checkld_results <- checkLD_remeas(subjectID = allPSP$unitreeid,
                                  measNo = allPSP$MEAS_NO,
                                  LDStatus = allPSP$Dead,
                                  liveCode = 0,
                                  deadCode = 1)
nrow(checkld_results[pass == FALSE,]) # 0 trees

## select the main plot tree
sort(unique(allPSP[is.na(SAMPLE_BREAK_POINT),]$MEAS_YR))
# [1] 2012 2013 2014 2015 2016 2017 2018 2019
allPSP[is.na(SAMPLE_BREAK_POINT), SAMPLE_BREAK_POINT := 4]


allPSP[, maxBreakPoint := max(SAMPLE_BREAK_POINT), by = "SAMP_ID"]
sample_threshold_smry <- unique(allPSP[,.(SAMP_ID, maxBreakPoint)],
                                by = "SAMP_ID")
data.frame(table(sample_threshold_smry$maxBreakPoint))
#   Var1 Freq
# 1    2    2
# 2    4 1280
# 3  6.5  150
# 4  7.5  550
# 5  9.1 1878

# remove the samples that have threshold bigger than DBH_threshold

allPSP <- allPSP[maxBreakPoint <= DBH_threshold, ]

# select all the trees with DBH bigger than DBH_threshold
allPSP_org <- data.table::copy(allPSP)
allPSP <- data.table::copy(allPSP_org)


allPSP_selected_tmp <- allPSP[DBH >= DBH_threshold,.(unitreeid, MEAS_YR)]
allPSP_selected_tmp <- allPSP_selected_tmp[, .(minMeasYR = min(MEAS_YR)), by = "unitreeid"]

allPSP <- merge(allPSP, allPSP_selected_tmp,
                         by = "unitreeid",
                         all.x = TRUE)
## DBH when it is first seen bigger than 9.1 
allPSP_selected <- allPSP[MEAS_YR >= minMeasYR, ]
rm(allPSP_selected_tmp)

allPSP_selected[, maxDBH := max(DBH, na.rm = TRUE),
                by = "unitreeid"]

allPSP_selected[maxDBH == -Inf,.(unitreeid, MEAS_YR, DBH, maxDBH)]
allPSP_selected[,':='(minmeasno = NULL,
                      maxBreakPoint = NULL,
                      minMeasYR = NULL,
                      maxDBH = NULL)]


range(allPSP_selected$DBH) 
# NA NA


allPSP_selected[, obslen := length(MAPPED),
       by = c("unitreeid", "MEAS_YR")]
allPSP_selected[obslen > 1,]

allPSP_selected <- allPSP_selected[!(SAMP_ID == "29025 G000501" &
                     MEAS_NO == "02"),]
allPSP_selected[, obslen := length(MAPPED),
       by = c("unitreeid", "MEAS_YR")]
allPSP_selected[obslen > 1,] # 0

## select samples that been measured more than 1 time
allPSP_selected[, myrlen := length(unique(MEAS_YR)),
       by = "SAMP_ID"]
allPSP_selected[myrlen == 1]

allPSP_selected <- allPSP_selected[myrlen != 1,]



## look at missing rate and ingrowth rate
allsamples <- unique(allPSP_selected$SAMP_ID)
obssmry <- data.table(SAMP_ID = character(),
                      MEAS_YR_INI = numeric(),
                      MEAS_YR_FIN = numeric(),
                      NO_TREE_INI = numeric(),
                      NO_TREE_FIN = numeric(),
                      NO_MISSING_TREE = numeric(),
                      NO_INGROWTH_TREE = numeric(),
                      MISSING_RATE = numeric(),
                      INGROWTH_RATE= numeric())

for (indisample in allsamples) {
  indisampledata <- allPSP_selected[SAMP_ID == indisample,]
  allmeasyr <- sort(unique(indisampledata$MEAS_YR))
  for(i in (1:(length(allmeasyr)-1))){
    indisamp_prev <- indisampledata[MEAS_YR == allmeasyr[i],]
    prevtrees <- unique(indisamp_prev$unitreeid)
    indisamp_curt <- indisampledata[MEAS_YR == allmeasyr[(i+1)],]
    curttrees <- unique(indisamp_curt$unitreeid)
    missingtrees <- prevtrees[!(prevtrees %in% curttrees)]
    ingrowthtrees <- curttrees[!(curttrees %in% prevtrees)]
    output <- data.table(SAMP_ID = indisample,
                         MEAS_YR_INI = allmeasyr[i],
                         MEAS_YR_FIN = allmeasyr[(i+1)],
                         NO_TREE_INI = length(prevtrees),
                         NO_TREE_FIN = length(curttrees),
                         NO_MISSING_TREE = length(missingtrees),
                         NO_INGROWTH_TREE = length(ingrowthtrees))
    output[, ':='(MISSING_RATE = 100*NO_MISSING_TREE/NO_TREE_INI,
                  INGROWTH_RATE = 100*NO_INGROWTH_TREE/NO_TREE_FIN)]
    obssmry <- rbind(obssmry, output)
    rm(indisamp_prev, prevtrees, indisamp_curt, curttrees, missingtrees, 
       ingrowthtrees, output, i)
  }
}


target_samples <- obssmry[MISSING_RATE <= 60 & INGROWTH_RATE <= 60]
target_samples[, obslen := length(NO_TREE_INI),
               by = "SAMP_ID"]
target_samples <- target_samples[obslen >1,]
target_sample_id <- unique(target_samples$SAMP_ID)

for (indisample in target_sample_id) {
  output <- data.table(SAMP_ID = indisample,
                       MEAS_YR = unique(c(target_samples[SAMP_ID == indisample,]$MEAS_YR_INI,
                                          target_samples[SAMP_ID == indisample,]$MEAS_YR_FIN)))
  if(indisample == target_sample_id[1]){
    alloutput <- output
    rm(output)
  } else {
    alloutput <- rbind(alloutput, output)
    rm(output)
  }
}
rm(indisample)

allPSP_selected <- merge(alloutput,
                         allPSP_selected,
                         by = c("SAMP_ID", "MEAS_YR"),
                         all.x = TRUE)



allsamples <- unique(allPSP_selected$SAMP_ID)

## check the missing measurement
for (indisample in allsamples) {
  indisampledata <- allPSP_selected[SAMP_ID == indisample, ]
  
  cat(indisample, "\n")
  indisample_missing <- checkMissing_remeas(subjectID = indisampledata$unitreeid,
                                            measNo = as.numeric(indisampledata$MEAS_YR),
                                            intendedMeasNo = as.numeric(unique(indisampledata$MEAS_YR)),
                                            deadCode = 1,
                                            LDStatus = indisampledata$Dead)
  
  if(indisample == allsamples[1]){
    sample_missing <- indisample_missing
    rm(indisample_missing)
  } else {
    sample_missing <- rbind(sample_missing, indisample_missing)
    rm(indisample_missing)
  }
}



missing <- sample_missing[pass == FALSE,]
unique(missing$missingReason)

pass <- sample_missing[pass == TRUE,]

missingmidd <- missing[missingReason == "missing middle",
                       .(unitreeid = subjectID, missingMeasNo, 
                         action = "added due to missing middle")]
missinglast <- missing[missingReason == "missing tail"]

missinglast[, firstmeasno := min(missingMeasNo),
            by = "subjectID"]
missinglast <- missinglast[missingMeasNo == firstmeasno,
                           .(unitreeid = subjectID, missingMeasNo,
                             action = "added due to missing tail")]
missingmidd <- merge(missingmidd, unique(allPSP_selected, by = "unitreeid"),
                     by = "unitreeid",
                     all.x = TRUE)
missinglast <- merge(missinglast, unique(allPSP_selected, by = "unitreeid"),
                     by = "unitreeid",
                     all.x = TRUE)
missingall <- rbindlist(list(missingmidd[,Dead := 0], missinglast[, Dead := 1]))

missingall[,':='(MEAS_YR = missingMeasNo,
                 CROWN_CLASS_CODE = NA,
                 HEIGHT_SOURCE_CODE = NA,
                 HT_DIAMETER_CURVE_USE_CODE = NA,
                 HT_MEASUREMENT_STATUS_CODE = NA,
                 TREE_CLASS_CODE = NA,
                 DBH = NA,
                 MEAS_HT = NA,
                 MEAS_TIME = NA)]
missingall[, missingMeasNo := NULL]

allPSP_selected[, action := as.character(NA)]
sort(names(missingall)) == sort(names(allPSP_selected))
missingall <- missingall[, names(allPSP_selected), with = FALSE]

allPSP_selected <- rbindlist(list(allPSP_selected, missingall))
allPSP_selected <- allPSP_selected[order(SAMP_ID, PLOT_NO, TREE_NO, MEAS_YR),]

allsamples <- unique(missingall$SAMP_ID)
for (indisample in allsamples) {
  indisampledata <- allPSP_selected[SAMP_ID == indisample, ]
  indisample_missing <- checkMissing_remeas(subjectID = indisampledata$unitreeid,
                                            measNo = as.numeric(indisampledata$MEAS_YR),
                                            intendedMeasNo = as.numeric(unique(indisampledata$MEAS_YR)),
                                            deadCode = 1,
                                            LDStatus = indisampledata$Dead)
  
  if(indisample == allsamples[1]){
    sample_missing <- indisample_missing
    rm(indisample_missing)
  } else {
    sample_missing <- rbind(sample_missing, indisample_missing)
    rm(indisample_missing)
  }
}

missing <- sample_missing[pass == FALSE,]

rm(allmeasyr, alloutput, pass, obssmry, missing, missingall, missinglast, missingmidd)


save.image("backup1.rdata")

rm(list = ls())
load("backup1.rdata")



sizecheck <- checkSize_remeas(subjectID = allPSP_selected$unitreeid,
                              measTime = allPSP_selected$MEAS_YR,
                              size = allPSP_selected$DBH,
                              maxChangeRate = 3,
                              tolerance = 1)
table(sizecheck[pass == FALSE,]$reason)




growthrates <- allPSP_selected[unitreeid %in% allPSP_selected[is.na(DBH) & Dead != 1,]$unitreeid,
                               .(unitreeid, MEAS_YR, DBH, Dead)]

## for the missing dbh just before last observed dead trees, the missing dbh is likely caused by 
## its dead status, therefore, change the status from alive to dead 
growthrates1 <- growthrates[Dead != 1,]
growthrates1[, maxYr := max(MEAS_YR), by = "unitreeid"]
growthrates1 <- growthrates1[MEAS_YR == maxYr & is.na(DBH),
                             .(unitreeid, MEAS_YR, Dead_new = 1)]

allPSP_selected2 <- allPSP_selected[!(unitreeid %in% growthrates1$unitreeid & Dead == 1), ]
allPSP_selected2 <- merge(allPSP_selected2, growthrates1,
                         by = c("unitreeid", "MEAS_YR"),
                         all.x = TRUE)
allPSP_selected2[!is.na(Dead_new), ':='(Dead = Dead_new,
                                        DBH_action = "Use last DBH, Dead changed to 1",
                                        action = "added due to missing tail")]
allPSP_selected2[, Dead_new := NULL]


growthrates <- growthrates[!(unitreeid %in% growthrates1$unitreeid),]
growthrates[, minYR := min(MEAS_YR), by = "unitreeid"]
growthrates[MEAS_YR == minYR, baseDBH := DBH]
growthrates[, baseDBH := max(baseDBH, na.rm = TRUE), by = "unitreeid"]
growthrates[, ':='(growth = DBH - baseDBH,
                   interval = MEAS_YR - minYR)]
growthrates <- growthrates[interval != 0,]
growthrates[, growthrate := growth/interval]
growthrates[, growthrate := mean(growthrate, na.rm = TRUE),
            by = "unitreeid"]
growthrates[is.na(DBH), DBH_new := interval*growthrate+baseDBH]
growthrates <- growthrates[is.na(DBH),.(unitreeid, MEAS_YR, DBH_new)]




allPSP_selected3 <- merge(allPSP_selected2, growthrates,
                         by = c("unitreeid", "MEAS_YR"), all.x = TRUE)

allPSP_selected3[!is.na(DBH_new),
                ':='(DBH = DBH_new, DBH_action = "Assigned based on growth rate")]
allPSP_selected3[, DBH_new := NULL]
unique(allPSP_selected3[is.na(DBH)]$Dead)
unique(allPSP_selected3[is.na(DBH) & Dead == 1,]$action)

rm(growthrates, growthrates1)

## for dead trees, all DBH is assigned as last alive dbh
deadtrees <- allPSP_selected3[unitreeid %in% allPSP_selected3[Dead == 1,]$unitreeid,
                               .(unitreeid, MEAS_YR, DBH, Dead)]
deadtrees <- deadtrees[Dead == 0, ]
deadtrees[, maxYR := max(MEAS_YR), by = "unitreeid"]
deadtrees <- deadtrees[MEAS_YR == maxYR,.(unitreeid, DBH_new = DBH)]

allPSP_selected4 <- merge(allPSP_selected3, deadtrees,
                         by = c("unitreeid"),
                         all.x = TRUE)
allPSP_selected4[Dead == 1 & !is.na(DBH_new),
                ':='(DBH = DBH_new, DBH_action = "Assigned as last alive DBH")]
allPSP_selected4[,DBH_new := NULL]
nrow(allPSP_selected4) == nrow(unique(allPSP_selected4, by = c("unitreeid", "MEAS_YR")))

## check size change again
sizecheck2 <- checkSize_remeas(subjectID = allPSP_selected4$unitreeid,
                              measTime = allPSP_selected4$MEAS_YR,
                              size = allPSP_selected4$DBH,
                              maxChangeRate = 3,
                              tolerance = 1)
table(sizecheck2[pass == FALSE,]$reason)
# abnormal change rate      break tolerance 
#                    6                  247 

allPSP_selected4[unitreeid %in% sizecheck2[reason == "abnormal change rate", ]$subjectID, 
                 .(unitreeid, MEAS_YR, DBH, Dead)]
#                unitreeid MEAS_YR   DBH Dead
# 1: 07010 R000503_01_0146    1956  46.2    0
# 2: 07010 R000503_01_0146    1963  52.8    0
# 3: 07010 R000503_01_0146    1967  55.1    0
# 4: 07010 R000503_01_0146    1971  70.7    0
# 5: 07010 R000503_01_0146    1976  72.6    0
# 6: 07010 R000503_01_0146    1980  75.3    0
# 7: 07010 R000503_01_0146    1986  77.3    0
# 8: 07010 R000503_01_0146    1993  79.1    0
# 9: 14061 G000014_01_0001    1964  14.9    0
# 10: 14061 G000014_01_0001    1974  16.4    0
# 11: 14061 G000014_01_0001    1983  18.6    0
# 12: 14061 G000014_01_0001    1994  19.8    0
# 13: 14061 G000014_01_0001    2015 227.0    0
# 14: 27027 G000016_01_0264    1992  12.4    0
# 15: 27027 G000016_01_0264    2003  13.0    0
# 16: 27027 G000016_01_0264    2007 124.0    0
# 17: 53015 R000015_01_0007    1947  38.2    0
# 18: 53015 R000015_01_0007    1952  40.6    0
# 19: 53015 R000015_01_0007    1968  50.9    0
# 20: 53015 R000015_01_0007    1978  62.5    0
# 21: 53015 R000015_01_0007    1988  68.9    0
# 22: 53015 R000015_01_0007    1997  74.0    0
# 23: 53015 R000015_01_0007    2007 106.5    0
# 24: 56053 R000317_01_0080    1996  26.0    0
# 25: 56053 R000317_01_0080    2001   2.6    0
# 26: 56053 R000317_01_0080    2006  26.0    0
# 27: 63022 R000508_01_0322    1968  12.7    0
# 28: 63022 R000508_01_0322    1972  25.5    0
# 29: 63022 R000508_01_0322    1977  28.0    0
# 30: 63022 R000508_01_0322    1982  29.8    0
# 31: 63022 R000508_01_0322    1987  30.8    0

allPSP_selected4[unitreeid %in% sizecheck2[reason == "break tolerance", ]$subjectID, 
                 .(unitreeid, MEAS_YR, DBH, Dead)]


range(allPSP_selected4[!(unitreeid %in% sizecheck2[pass == FALSE,]$subjectID), ]$DBH)


save.image("backup2.rdata")

load("backup2.rdata")

saveRDS(agedata, file.path(".", "data", "agedata_raw.rds"))
saveRDS(allPSP_selected4, file.path(".", "data", paste0("treelist_meas", minmeaslen, "_DBH", DBH_threshold, ".rds")))
saveRDS(target_samples[,.(SAMP_ID, MEAS_YR_INI, MEAS_YR_FIN)],
        file.path(".", "data", "sample_meas_final.rds"))

