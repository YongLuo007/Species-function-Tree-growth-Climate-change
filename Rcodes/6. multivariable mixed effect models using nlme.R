rm(list = ls())
library(data.table)
library(nlme)

source(file.path(".", "Rcodes", "Rfunctions", "selectData1.R"))

allstanddata <- readRDS(file.path(".", "data", "finalBC_stand_all.rds"))
alltreedata <- readRDS(file.path(".", "data", "finalBC_tree_data_wide.rds")) 
minNoTree_firstMeas <- 20
maxMeasInterval <- 10

inputdata_all <- selectData1(allstanddata = allstanddata,
                         alltreedata = alltreedata,
                         minNoTree_firstMeas = minNoTree_firstMeas,
                         maxMeasInterval = maxMeasInterval)
inputdata <- inputdata_all$selectedStandData
inputdata_tree <- inputdata_all$selectedTreeData




saveRDS(inputdata, file.path(".", "data", "final_BC_stand.rds"))
saveRDS(inputdata_tree, file.path(".", "data", "final_BC_trees.rds"))




inputdata <- inputdata[, .(SAMP_ID, PLOT_NO, MEAS_YR_INI, MEAS_YR_FIN,
                           Year_mid, SA, SA_est, Richness_est,
                           FOREST_TYPE_est,
                           AGB = Biomass_AnnChange,
                 AGB_GI = Biomass_AnnGrowth + Biomass_AnnIngrow,
                 AGB_M = Biomass_AnnDead)]
inputdata_long <- rbindlist(list(inputdata[,.(SAMP_ID, PLOT_NO, MEAS_YR_INI, MEAS_YR_FIN,
                                          Year_mid, SA, SA_est, Richness_est,
                                          FOREST_TYPE_est, trait = "AGB",
                                          value = AGB)],
                             inputdata[,.(SAMP_ID, PLOT_NO, MEAS_YR_INI, MEAS_YR_FIN,
                                          Year_mid, SA, SA_est, Richness_est,
                                          FOREST_TYPE_est, trait = "AGB_GI",
                                          value = AGB_GI)],
                             inputdata[,.(SAMP_ID, PLOT_NO, MEAS_YR_INI, MEAS_YR_FIN,
                                          Year_mid, SA, SA_est, Richness_est,
                                          FOREST_TYPE_est, trait = "AGB_M",
                                          value = AGB_M)]))
inputdata_long[,':='(lnSActd = log(SA) - mean(log(SA)),
                Yearctd = Year_mid - mean(Year_mid))]

ctrl <- lmeControl(opt='optim')
inputdata_long[, trait := as.factor(trait)]
lmeDBC <- lme(value ~ trait/lnSActd*Yearctd,
              random = ~(trait-1)|SAMP_ID,
              data = inputdata_long,
              control = ctrl)
results_all <- summary(lmeDBC)
coefs <- data.frame(results_all$tTable)
coefs$Variable <- row.names(coefs)
coefs$model <- "all species"
coefs$NoPlots <- length(unique(inputdata_long$SAMP_ID))

foresttypes <- unique(inputdata_long$FOREST_TYPE_est)

for (indisptype in foresttypes) {
  indidata <- inputdata_long[FOREST_TYPE_est == indisptype,]
  lmeDBC_indi <- lme(value ~ trait/lnSActd*Yearctd,
                random = ~(trait-1)|SAMP_ID,
                data = indidata,
                control = ctrl)
  results_indi <- summary(lmeDBC_indi)
  coefs_indi <- data.frame(results_indi$tTable)
  coefs_indi$Variable <- row.names(coefs_indi)
  coefs_indi$model <- indisptype
coefs_indi$NoPlots <- length(unique(indidata$SAMP_ID))
  coefs <- rbind(coefs, coefs_indi)
}

coefs <- data.table(coefs)
coefs[, Variable := gsub("trait", "", Variable)]

coefs[, DependentV := unlist(lapply(strsplit(Variable, ":"), function(s){s[1]}))]
coefs[DependentV %in% c("(Intercept)", "Yearctd"), DependentV := "AGB"]

coefs[Variable %in% c("(Intercept)", "AGB_GI", "AGB_M"),
      IndependentV := "Intercept"]
coefs[Variable %in% c("Yearctd"),
      IndependentV := "Yearctd"]
coefs[Variable %in% c("AGB:lnSActd", "AGB_GI:lnSActd",
                      "AGB_M:lnSActd", "AGB_GI:Yearctd","AGB_M:Yearctd"),
      IndependentV := unlist(lapply(strsplit(Variable, ":"), function(s){s[2]}))]

coefs[is.na(IndependentV),
      IndependentV := paste0(unlist(lapply(strsplit(Variable, ":"), function(s){s[2]})),
                             "*",
                             unlist(lapply(strsplit(Variable, ":"), function(s){s[3]})))]
coefs[, Variable := NULL]

coefs <- coefs[order(model, DependentV, IndependentV),
               .(model, NoPlots, DependentV, IndependentV, Value,
                 Std.Error, DF, t.value, p.value)]
write.csv(coefs, file.path(".", "results", paste0("multivariAnalyses_MinTree", minNoTree_firstMeas,
                                                  "MaxInterval", maxMeasInterval, ".csv")),
          row.names = FALSE)






