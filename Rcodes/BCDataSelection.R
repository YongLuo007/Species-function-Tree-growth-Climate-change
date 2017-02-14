
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

# 225

unique(selectedPlots$sampletype) #  "G" "R"

selectedPlots <- selectedPlots[no_plots == 1,]
print(length(unique(selectedPlots$SAMP_ID))) # 215

set(selectedPlots, ,c("samp_id_meas", "no_plots", "proj_id", "samp_no", "type_cd", 
                      "sel_lgd", "treatment", "trt_code", 
                      "owner", "own_sched", "region", "compt", "compltr", 
                      "inst", "sampleno", 
                      "bcalb_x", "bcalb_y", "map_tile", "polygon_no", 
                      "opening_no", "tsa", "mgmt_unit", "project", "bec_source", "bgc_zone", 
                      "beclabel", "beclabel_grd", "bgc_ss_grd",  "landsat_disturb_yr", 
                      "buffer_rad", "fiz",  
                      "baf_pm", "phf_pm", "baf_ps", "phf_ps", 
                      "meas_yr_first", "meas_yr_last", "meas_first", "meas_last", 
                      "meas_no_orig",  "recon_yr", 
                      "period", "tot_period", "dbhlimit_breakpt",  
                      "min_dbh", "max_dbh", "dbhavg_L", "dbhavg_LI", "dbhavg_V", "dbhavg_c250", 
                      "dbhavg_DP", "dbhavg_DU", "dbhavg_C", "dbhq_pre", "dbhq_L", "dbhq_LI", 
                      "dbhq_V", "dbhq_c250", "dbhq_DP", "dbhq_DU", "dbhq_C", "baha_pre", 
                      "baha_L", "baha_LI", "baha_V", "baha_c250", "baha_DP", "baha_DU", 
                      "baha_C", "baha_ibdrid", "stemsha_pre", "stemsha_L", "stemsha_LI", 
                      "stemsha_V", "stems_LV", "stems_LIV", "stemsha_c250", "stemsha_DP", 
                      "stemsha_DU", "stemsha_C", "wsvha_pre", "wsvha_L", "wsvha_LI", 
                      "wsvha_V", "wsv_LV", "wsv_LIV", "wsvha_c250", "wsvha_DP", "wsvha_DU", 
                      "wsvha_C", "mv10ha_L", "mv10ha_LI", "mv10ha_V", "mv10ha_LV", 
                      "mv10ha_LIV", "mv10ha_c250", "mv10ha_DP", "mv10ha_DU", "mv10ha_C", 
                      "nmv10ha_L", "nmv10ha_LI", "nmv10ha_V", "nmv10ha_LV", "nmv10ha_LIV", 
                      "nmv10ha_c250", "nmv10ha_DP", "nmv10ha_DU", "nmv10ha_C", "htavg_L", 
                      "htavg_V", "htlorey_L", "htavg_c250", "htavg_DP", "htavg_DU", 
                      "htavg_C", "spc_live1", "spc_live2", "spcper_live1", "spcper_live2", 
                      "spc_label_live", "spc_label_dead", "pai_baha_l", "pai_baha_li", 
                      "pai_baha_dp", "pai_baha_du", "pai_stemsha_l", "pai_stemsha_li", 
                      "pai_stemsha_dp", "pai_stemsha_du", "pai_wsvha_l", "pai_wsvha_li", 
                      "pai_wsvha_dp", "pai_wsvha_du", "mai_wsvha_l", "maxn_htop", "lead_si1", 
                      "lead_si2", "lead_si3", "lead_si4", "secd_si4", "src_si4", "lead_siref1", 
                      "lead_siref2", "lead_siref3", "lead_age_bh1", "lead_age_bh2", 
                      "lead_age_bh3", "lead_age_tot1", "lead_age_tot2", "lead_age_tot3", 
                      "secd_age_tot3", "bh_stand_age", "stand_age_source", 
                      "lead_htop1", "lead_htop2", "lead_htop3", "lead_n_age1", "lead_n_age2", 
                      "lead_n_age3", "lead_n_htop1", "lead_n_htop2", "lead_n_htop3", 
                      "secd_htop3", "rating", "cell_key1", "rank_psp1", "rank_all1", 
                      "max_psp1", "max_all1", "adj_id", "tot_num_trees", "changephf", 
                      "chgcrncls", "chglayer", "chgspc", "chgtrcls", "dbh3cmyr", "dead2live", 
                      "duptreeno", "extrhtdbh", "htc2myr", "htmdrop", "misaphf", "mismdbh", 
                      "mismeasno", "mismhtc", "mismphf", "mismspc", "mismtage", "mismyr", 
                      "missdbh", "missphf", "mistagsec", "chgtagsec", "chgsitsec", 
                      "negbakage", "extrdbh", "assumedf", "misdecml", "sitsecfil", 
                      "sitsecmis", "tagsitcon", "noborebh", "nocrndata", "lead_per_aged1", 
                      "lead_per_aged2", "lead_per_aged3", "mis_si1", "mis_si2", "mis_si3", 
                      "mis_bec", "mis_subz", "mis_becss", "mis_utm", "dup_measyr", 
                      "mis_standage", "pai_dbhq2cm", "pai_baha10m2", "pai_htavg2m", 
                      "pai_wsv100m3", "nsec_ltmax", "nsec_gtmax", "no_sitesect", "no_treesect", 
                      "mistrdata", "samp_id_dup", "sampletype_dup", "bgc_zone_dup", 
                      "meas_last_dup", "meas_no_dup", "spc_live1_dup", "tot_stand_age_dup", 
                      "locTime"), NULL)

setnames(selectedPlots, c("SAMP_ID", "stnd_org", "stnd_str", "sampletype", "utm_source", 
                          "utm_zone", "utm_easting", "utm_northing", "samp_sts", "stem_mapped_ind", 
                          "plot_typ", "area_pm", "area_ps", "no_meas", "yr_est", "meas_no", 
                          "meas_dt", "meas_yr", "dbhlimit_tag", "dbhlimit_compile", "tot_stand_age", 
                          "Meas_time"),
         c("PlotNumber",  "StandOrig", "StandStr", "SampeType", 
                          "utmsource", "utmZone", "utmEasting", "utmNorthing",
                          "PlotStatus", 
                          "StemMapped", "plotType", "PlotSize", "subPlotSize",
                          "NumberOfMeas", 
                          "YearEstab", "MeasNumber", "MeasDate", "MeasureYear",
                          "minDBH", "minDBHCompile",
                          "StandAge", "MeasTime"))
write.csv(selectedPlots, file.path(workPath, "data", "BCSelectedPlot.csv"), row.names = FALSE)





########################
# TREE LEVEL SELECTION #
########################


BCtreedata <- fread(file.path(workPath,"data",
                              "BC", "Trees.csv"))

setnames(BCtreedata, c("SAMP_ID", "meas_yr"), c("PlotNumber", "MeasureYear"))

aa <- data.table::copy(BCtreedata)
selectedPlotsShort <- selectedPlots[,.(selectedPlot = "yes"), by = c("PlotNumber", "MeasureYear")]
treesInSelctedPlots <- dplyr::left_join(aa, selectedPlotsShort, by = c("PlotNumber", "MeasureYear")) %>%
  data.table
treesInSelctedPlots <- treesInSelctedPlots[selectedPlot == "yes",]

treesInSelctedPlots[,uniTreeID:=paste(PlotNumber, "_", tree_no, sep = "")]

length(unique(treesInSelctedPlots$uniTreeID)) # 47902 trees



set(treesInSelctedPlots, ,c("dumcoord", "meas_no", "meas_no_orig", 
                            "meas_first", "meas_last", "tagging_sector_no", "site_sector_no", 
                            "site_sector_orig", "phf_tree", "phf_src", "sub_plot_tree", 
                            "near_tree_no",  "crown_cls", 
                            "ht_meas", "ht_est", "ht_calc", "height", "ht_source", 
                            "ht_to_break", "liv_crn_ht", "liv_crn_per", "batree", "baha", 
                            "volwsv", "volcu10m", "volcu15m", "wsvha", "mv10ha", "mv15ha", 
                            "nmv10ha", "nmv15ha", "ht_suit", "si_suit", "bore_ht", 
                            "dbh_sec_order", "dbh_sam_order", 
                            "si_meth1", "si_type_meth1", "si_eqn_meth1", "si_bhage_meth1", 
                            "si_htop_meth1", "si_meth2", "si_type_meth2", "si_eqn_meth2", 
                            "si_bhage_meth2", "si_htop_meth2", "dam_agent_spc1", 
                            "dam_agent_type1", "dam_agent_svty1", "dam_agent_spc2", "dam_agent_type2", 
                            "dam_agent_svty2", "dam_1", "dam_2", "path_ind", "dam_frk", "dam_scr", 
                            "dam_dtop", "dam_mis", "dam_frs", "dam_btop", "dam_cnk", "dam_rot", 
                            "dam_bcnk", "risk_grp", "pct_dcy", "pct_wst", "pct_brk", "dead_standing_or_down", 
                            "dead_tree_no_certainty", "dead_tree_species_certainty", "dead_tree_wildlife_appearance", 
                            "tree_err", "tree_err_1stpass", "ld_dup", "species_forpivot", 
                            "subPlotLength", "subPlot"),
    NULL)
set(treesInSelctedPlots, , c("plot_no", "selectedPlot"), NULL)

setnames(treesInSelctedPlots, c("PlotNumber", "MeasureYear", "tree_no", "species", "tree_cls", 
                                "ld", "dbh", "dbh_orig", "age_boreht", "age_bh", "age_tot", "age", 
                                "agetyp", "x_coord", "y_coord", "stem_map_bearing", "stem_map_slope_percent", 
                                "stem_map_slope_distance", "uniTreeID"),
         c("PlotNumber", "MeasureYear", "TreeNumber", "Species", "TreeClass", 
           "LiveDead", "DBH", "DBHOrig", "ageBoreHt", "AgeBreastHt", "AgeTotal", "Age", 
           "AgeType", "coordX", "coordY", "Angle", "SlopePercent", 
           "Distance", "uniTreeID"))

write.csv(treesInSelctedPlots, file.path(workPath, "data", "BCtrees.csv"), row.names = FALSE)


