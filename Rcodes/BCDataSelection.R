# get the BC data
rm(list = ls())
library(data.table);library(ggplot2);library(gridExtra);library(raster)
library(dplyr)
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

# select the plots that had location information

selectedPlots[,locTime:=length(unique(utm_zone)), by = SAMP_ID]
unique(selectedPlots$locTime) #1
selectedPlots <- selectedPlots[!is.na(utm_zone),]




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


unique(treesInSelctedPlots$sub_plot_tree)# "N" "Y" "" 
#check by plot level
treesInSelctedPlots[,uniPlotID:=paste(SAMP_ID, "_", plot_no, sep = "")]
treesInSelctedPlots[, ':='(subPlotLength = length(unique(sub_plot_tree)),
                           subPlot = paste(sort(unique(sub_plot_tree)), collapse = "+")),
                    by = uniPlotID]
unique(treesInSelctedPlots$subPlotLength)

mappingCheck <- treesInSelctedPlots[,.(SAMP_ID, plot_no, sub_plot_tree, uniTreeID, 
                                       x_coord, y_coord)]

mappingCheck <- unique(mappingCheck, by = "uniTreeID")

mappingCheck[, subplottrees:=factor(sub_plot_tree, levels = c("N", "Y", ""),
                                    labels = c("In main plot", "In sub plot", "Unknown"))]
points360 <- seq(0, 2 * pi, length.out = 360)
treeplot <- Polygons(list(Polygon(cbind(5.64 * sin(points360),
                                        5.64 * cos(points360)))),
                     ID = 1)
treeplot <- SpatialPolygons(list(treeplot))
treeplot <- fortify(treeplot, region = "ID") %>% data.table


ecologicalplot <- Polygons(list(Polygon(cbind(10 * sin(points360),
                                              10 * cos(points360)))),
                           ID = 1)
ecologicalplot <- SpatialPolygons(list(ecologicalplot))
ecologicalplot <- fortify(ecologicalplot, region = "ID") %>% data.table

size800Radius <- sqrt(800/pi)
circle800 <- Polygons(list(Polygon(cbind(size800Radius * sin(points360),
                                                size800Radius * cos(points360)))),
                           ID = 1)
circle800 <- SpatialPolygons(list(circle800))
circle800 <- fortify(circle800, region = "ID") %>% data.table



print(range(treesInSelctedPlots$dbh))
# 4.0 94.3 which were consistent with VRI sampling protocal (trees were defined bigger than 4 cm)

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
write.csv(treesInSelctedPlots, file.path(workPath, "data", "BCtrees.csv"), row.names = FALSE)
set(selectedPlots, ,c("samp_id_meas", "proj_id", "samp_no", "type_cd", 
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
                      "secd_age_tot3", "bh_stand_age", "tot_stand_age", "stand_age_source", 
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
write.csv(selectedPlots, file.path(workPath, "data", "BCSelectedPlot.csv"), row.names = FALSE)

treesInSelctedPlots[, NumberOfPlot:=length(unique(plot_no)), by = SAMP_ID]
mutiplesubplots <- unique(treesInSelctedPlots[NumberOfPlot>1,]$SAMP_ID)
oneplots <- unique(treesInSelctedPlots[NumberOfPlot == 1,]$SAMP_ID)
allMultiPlots <- list()
for(i in 1:length(mutiplesubplots)){
  allMultiPlots[[i]] <- ggplot(data = mappingCheck[SAMP_ID == mutiplesubplots[i],], 
                               aes(x = x_coord, y = y_coord))+
    geom_point(aes(col = plot_no))+
    geom_path(data = treeplot, aes(x = long, y = lat, group = group), col = "blue", linetype = 1)+
    scale_x_continuous(name = "x")+
    scale_y_continuous(name = "y")+
    annotate("text", x = -Inf, y = Inf, label = mutiplesubplots[i], hjust = -0.5,
             vjust = 1.5)+
    theme_bw()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.position = "none")
}

plotlayout <- rbind(c(1,NA), c(2, 3), c(4, 5))
b <- grid.arrange(allMultiPlots[[1]], allMultiPlots[[2]], allMultiPlots[[3]],
                  allMultiPlots[[4]],allMultiPlots[[5]],
                  layout_matrix = plotlayout)
ggsave(file.path(workPath, "tableFigures", "sampleIDhadsubplots.png"),
       b, height = 10, width = 7)

size1000Radius <- sqrt(1000/pi)
circle1000 <- Polygons(list(Polygon(cbind(size1000Radius * sin(points360),
                                          size1000Radius * cos(points360)))),
                      ID = 1)
circle1000 <- SpatialPolygons(list(circle1000))
circle1000 <- fortify(circle1000, region = "ID") %>% data.table

unique(selectedPlots$sampletype) # "G" "R" "I"

for(j in c("G", "R", "I")){
  thePlots <- unique(selectedPlots[sampletype == j]$SAMP_ID)
  GrowthSamplingplots <- ggplot(data = mappingCheck[SAMP_ID %in% thePlots,], 
                                aes(x = x_coord, y = y_coord))+
    geom_point(data = mappingCheck[SAMP_ID %in% thePlots &
                                     subplottrees == "In main plot",],
               aes(col = subplottrees))+
    geom_point(data = mappingCheck[SAMP_ID %in% thePlots & 
                                     subplottrees == "In sub plot",],
               aes(col = subplottrees))+
    geom_point(data = mappingCheck[SAMP_ID %in% thePlots & 
                                     subplottrees == "Unknown",],
               aes(col = subplottrees))+
    geom_path(data = circle800, aes(x = long, y = lat, group = group),
              col = "blue", linetype = 1)+
    geom_path(data = circle1000, aes(x = long, y = lat, group = group),
              col = "blue", linetype = 2)+
    scale_color_manual(name = "Trees location", values = c("gray", "red", "green"))+
    scale_x_continuous(name = "x", limits = c(-60, 60))+
    scale_y_continuous(name = "y", limits = c(-60, 60))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(colour = "black"),
          legend.position = c(0.9, 0.9))
  ggsave(file.path(workPath, "tableFigures", paste("alltreemapping_",
                                                   j, ".png", sep = "")),
         GrowthSamplingplots, height = 9, width = 9)
}

for(j in c("G", "R", "I")){
  print(length(unique(selectedPlots[sampletype == j]$SAMP_ID)))
  print(unique(selectedPlots[sampletype == j]$area_pm))
}

Gplots <- unique(selectedPlots[sampletype == "G"]$SAMP_ID)



for(i in Gplots){
  sizeRadius <- sqrt((10000*unique(selectedPlots[SAMP_ID == i]$area_pm))/pi)
  fixedCircle <- Polygons(list(Polygon(cbind(sizeRadius * sin(points360),
                                            sizeRadius * cos(points360)))),
                         ID = 1)
  fixedCircle <- SpatialPolygons(list(fixedCircle))
  fixedCircle <- fortify(fixedCircle, region = "ID") %>% data.table
  GrowthSamplingplots <- ggplot(data = mappingCheck[SAMP_ID == i,], 
                                aes(x = x_coord, y = y_coord))+
    geom_point(col = "gray")+
    geom_path(data = fixedCircle, aes(x = long, y = lat, group = group),
              col = "blue", linetype = 1)+
    annotate("text", x = -Inf, y = Inf, label = i, hjust = -0.5,
             vjust = 1.5)+
    scale_x_continuous(name = "x", limits = c(-30, 30))+
    scale_y_continuous(name = "y", limits = c(-30, 30))+
    theme_bw()+
    theme(panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(colour = "black"),
          legend.position = "none")
  ggsave(file.path(workPath, "tableFigures", paste("Gplots_",
                                                   i, ".png", sep = "")),
         GrowthSamplingplots, height = 9, width = 9)
}

canadaMap <- readRDS(file.path(workPath, "data", "canadamap.RDS"))

BCProvince <- canadaMap[canadaMap@data$NAME  == "British Columbia", ]
names(selectedPlots)

plotsLocation <- unique(selectedPlots[!is.na(utm_zone),.(SAMP_ID, utm_source, utm_zone, 
                                         utm_easting, utm_northing, sampletype)], 
                        by = "SAMP_ID")

plotsLocations <- SpatialPoints(plotsLocation[,.(Easting = utm_easting, Northing = utm_northing)],
                               proj4string = CRS("+proj=utm +zone=10 datum=NAD83"))

plotsLocations <- spTransform(plotsLocations, crs(canadaMap))
plotsLocations <- as.data.table(as.data.frame(plotsLocations@coords))
plotsLocations[,':='(SAMP_ID = plotsLocation$SAMP_ID,
                     sampletype = plotsLocation$sampletype)]
BCProvince <- fortify(BCProvince, region = "NAME") %>%
  data.table
allplots <- unique(BCPlotInfor[!is.na(utm_zone),.(SAMP_ID, utm_zone, utm_easting, utm_northing)],
                   by = "SAMP_ID")
allplotsLoc <- SpatialPoints(allplots[,.(Easting = utm_easting, Northing = utm_northing)],
                                               proj4string = CRS("+proj=utm +zone=10 datum=NAD83"))

allplotsLoc <- spTransform(allplotsLoc, crs(canadaMap))
allplotsLoc <- as.data.table(as.data.frame(allplotsLoc@coords))
plotssummary <- plotsLocations[,.(NumPlot = unique(length(SAMP_ID))), by = sampletype]
plotssummary[, summary:=paste(sampletype, ": ",  NumPlot, " plots", sep = "")]
plotLoaMap <- ggplot(data = BCProvince, aes(x = long, y = lat))+
  geom_polygon(aes(group = group), fill = "white")+
  geom_path(aes(group = group), col = "black")+
  geom_point(data = allplotsLoc, aes(x = Easting, y = Northing), col = "gray")+
  geom_point(data = plotsLocations, aes(x = Easting, y = Northing,
                                        col = sampletype))+
  scale_color_manual(values = c("red", "blue"), labels = plotssummary$summary)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", size = 1),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.8, 0.9), 
        legend.background = element_rect(colour = "black"))

ggsave(file.path(workPath, "tableFigures", "plotLocationMap.png"), plotLoaMap,
       height = 9, width = 5.2)

