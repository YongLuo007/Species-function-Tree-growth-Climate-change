rm(list = ls())
library(data.table)
library(ggplot2)
devtools::load_all("D:/GitHub/FAIBCompiler")
dominant_threshold <- 0.75

tree_meas <- readRDS(file.path(".", "data", "finalBC_tree_data_wide.rds")) 
stand_meas <- readRDS(file.path(".", "data", "finalBC_stand_data.rds"))

## calculate the species diversity
richness <- tree_meas[,.(Richness = length(unique(species))),
                      by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI")]
richness[, meas_min := min(MEAS_YR_INI),
         by = c("SAMP_ID", "PLOT_NO")]
richness[meas_min == MEAS_YR_INI, Richness_est := Richness]
richness[, Richness_est := min(Richness_est, na.rm = TRUE),
         by = c("SAMP_ID", "PLOT_NO")]
stand_meas <- merge(stand_meas, richness,
                    by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI"),
                    all.x = TRUE)

## calculate the species composition by species
tree_meas[species_type == "D", species_group := "Deciduous species"]
tree_meas[species_type == "C", species_group := "Other coniferous species"]

tree_meas[species %in% c("black spruce", "white spruce", "sitka spruce",
                         "engelmann spruce"),
          species_group := "Spruce"]

tree_meas[species %in% c("lodgepole pine"),
          species_group := "lodgepole pine"]

tree_meas[species %in% c("douglas-fir"),
          species_group := "douglas-fir"]

tree_meas[species %in% c("western hemlock"),
          species_group := "western hemlock"]

spcomp <- tree_meas[,.(BA_sp = sum(BA_INI)),
                    by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI", "species_group")]
spcomp[, BA_tot := sum(BA_sp),
       by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI")]
spcomp[, sp_percent := BA_sp/BA_tot]
spcomp_domi <- spcomp[sp_percent >= dominant_threshold,
                      .(SAMP_ID, PLOT_NO, MEAS_YR_INI, FOREST_TYPE = species_group)]
nrow(unique(spcomp_domi, by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI"))) == nrow(spcomp_domi)

stand_meas <- merge(stand_meas, spcomp_domi,
                    by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI"),
                    all.x = TRUE)
stand_meas[is.na(FOREST_TYPE), FOREST_TYPE := "Mixed"]
stand_meas[meas_min == MEAS_YR_INI, FOREST_TYPE_est := FOREST_TYPE]
stand_meas[, FOREST_TYPE_est := unique(FOREST_TYPE_est)[1],
           by = c("SAMP_ID", "PLOT_NO")]
unique(stand_meas$FOREST_TYPE_est)

saveRDS(stand_meas, file.path(".", "data", "finalBC_stand_all.rds"))












