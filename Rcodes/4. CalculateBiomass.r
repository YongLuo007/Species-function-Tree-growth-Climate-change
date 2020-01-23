rm(list = ls())
library(data.table)
library(dplyr)
devtools::load_all("D:/GitHub/FAIBBase")
devtools::load_all("D:/GitHub/FAIBCompiler")
treelist <- readRDS(file.path(".", "data", "treelist_meas3_DBH9.1.rds")) %>% data.table


# species inspection
speciestable <- lookup_species()
speciestable <- speciestable[,.(orgSpecies = SPECIES,
                                species = tolower(DESCRIPTION),
                                species_type = SP_TYPE)]


treelist[, speciesLength:=length(unique(SPECIES)), 
         by = "unitreeid"]
unique(treelist$speciesLength) # 1 good

treelist[, speciesLength:=NULL]
treelist[, orgSpecies := toupper(SPECIES)]
treelist[, SPECIES := NULL]

treelist <- merge(treelist, speciestable,
                  by = "orgSpecies",
                  all.x = TRUE)
unique(treelist$species)

unique(treelist[is.na(species),]$orgSpecies)

# 
# 
# treelist[orgSpecies == "SB", species := "black spruce"]
# treelist[orgSpecies == "LT", species := "tamarack larch"]
# treelist[orgSpecies == "SW", species := "white spruce"]
# treelist[orgSpecies == "EP", species := "white birch"]
# treelist[orgSpecies == "PLI", species := "lodgepole pine"]
# treelist[orgSpecies == "ACB", species := "balsam poplar"]
# treelist[orgSpecies == "S", species := "black spruce"]
# # S for spruce in original doc. assume S is black spruce
# treelist[orgSpecies == "PL", species := "lodgepole pine"]
# treelist[orgSpecies == "ACT", species := "black cottonwood"]
# treelist[orgSpecies == "AT", species := "trembling aspen"]
# treelist[orgSpecies == "E", species := "white birch"]
# # E for birch in original doc. assume S is white birch
# # treelist[orgSpecies == "XC", species := "unknow conifer"]
# treelist[orgSpecies == "BL", species := "alpine fir"]
# treelist[orgSpecies == "EA", species := "white birch"]
# # EA is alaka paper birch, assume it is white birch here
# treelist[orgSpecies == "AC", species := "balsam poplar"]
# # treelist[orgSpecies == "X", species := "unknown"]
# treelist[orgSpecies == "B", species := "fir"]
# treelist[orgSpecies == "W", species := "willow"]
# treelist[orgSpecies == "DR", species := "red alder"]
# treelist[orgSpecies == "DM", species := "mountain alder"]
# treelist[orgSpecies == "PJ", species := "jack pine"]
# treelist[orgSpecies == "ZH", species := "hardwood"]
# treelist[orgSpecies == "SXW", species := "white spruce"]
# # SXW is hybrid between engelmann and white spruce
# treelist[orgSpecies == "XH", species := "hardwood"]
# treelist[orgSpecies == "SX", species := "white spruce"]
# # SX is hybrid
# treelist[orgSpecies == "A", species := "trembling aspen"]
# # A is aspen cottonwood and poplar, assume it is trembling aspen
# treelist[orgSpecies == "P", species := "lodgepole pine"]
# # P is pine orgSpecies, assume it is lodgepole pine
# treelist[orgSpecies == "MR", species := "red maple"]
# # MR could not find, assume it is red maple
# treelist[orgSpecies == "SE", species := "engelmann spruce"]
# treelist[orgSpecies == "FD", species := "douglas-fir"] 
# treelist[orgSpecies == "BA", species := "amabalis fir"] 
# treelist[orgSpecies == "CW", species := "western redcedar"]
# treelist[orgSpecies == "HW", species := "western hemlock"]
# treelist[orgSpecies == "FDI", species := "douglas-fir"]
# # treelist[orgSpecies == "SXE", species := "unknown"]
# treelist[orgSpecies == "D", species := "red alder"]
# # D is alder, assume red alder
# treelist[orgSpecies == "H", species := "western hemlock"]
# treelist[orgSpecies == "MV", species := "vine maple"] 
# treelist[orgSpecies == "HM", species := "mountain hemlock"] 
# treelist[orgSpecies == "EXP", species := "white birch"]
# treelist[orgSpecies == "WS", species := "scoulers willow"]
# treelist[orgSpecies == "AX", species := "trembling aspen"]

nrow(treelist[is.na(species),]) # 0 good





# assign biomass
treelist$biomass <- biomassCalculator(species = treelist$species,
                                        DBH = treelist$DBH,
                                      heightIncluded = FALSE,
                                        paperSource = "Ung2008")
uncalculatedSpecies <- unique(treelist[is.na(biomass),]$species)


# [1] "poplar"     
treelist[species %in% c("poplar"), species := "balsam poplar"]

# [2] "balsam"             
# [3] "amabalis fir"       
# [4] "grand fir"
treelist[species %in% c("balsam", "amabilis fir", "grand fir", "amabalis fir"),
         species := "balsam fir"]


# [6] "sitka alder"        
# [7] "mountain alder"  
treelist[species %in% c("sitka alder", "mountain alder"),
         species := "red alder"]

# [8] "birch"              
# [9] "paper birch"        
# [10] "silver birch"       
# [11] "water birch"
treelist[species %in% c("birch", "paper birch", "silver birch", "water birch"),
         species := "white birch"]

# [12] "coastal douglas-fir"
treelist[species %in% c("coastal douglas-fir"),
         species := "douglas-fir"]

# [13] "pacific dogwood"  
treelist[species %in% c("pacific dogwood"),
         species := "hardwood"]
# [14] "hemlock"            
# [15] "mountain hemlock"   
treelist[species %in% c("hemlock", "mountain hemlock"),
         species := "western hemlock"]

# [16] "rocky mtn. juniper" 
treelist[species %in% c("rocky mtn. juniper"),
         species := "softwood"]

# [17] "cascara"   
treelist[species %in% c("cascara"),
         species := "hardwood"]

# [18] "larch"              
# [19] "tamarak"            
# [20] "western larch"
treelist[species %in% c("larch", "tamarack", "western larch", "tamarak"),
         species := "tamarack larch"]

# [21] "bigleaf maple"      
# [22] "douglas maple"      
# [23] "sycamore maple"     
# [24] "vine maple"
treelist[species %in% c("bigleaf maple", "douglas maple", "sycamore maple", "vine maple"),
         species := "sugar maple"]

# [25] "whitebark pine"     
# [26] "shore pine"         
# [27] "western white pine" 
# [28] "ponderosa pine"    
treelist[species %in% c("whitebark pine", "shore pine",
                        "western white pine", "ponderosa pine"),
         species := "lodgepole pine"]

# [29] "arbutus"  
treelist[species %in% c("arbutus"),
         species := "hardwood"]

# [30] "spruce"             
# [31] "spruce hybrid"    
treelist[species %in% c("spruce", "spruce hybrid"),
         species := "black spruce"]

# [32] "pacific yew"        
# [33] "pacific crab apple" 
# [34] "bitter cherry"      
# [35] "willow"             
# [36] "bebb's willow"      
# [37] "scouler's willow"   
# [39] "unknown hardwood"   
# [41] "other hardwood"   
treelist[species %in% c("pacific yew", "pacific crab apple", "willow", "bitter cherry",
                        "bebb's willow", "scouler's willow", "unknown hardwood",
                        "other hardwood"),
         species := "hardwood"]

# [38] "unknown conifer"    
treelist[species %in% c("unknown conifer"),
         species := "softwood"]

# [40] "yellow cedar" 
# [5] "western red cedar"  
treelist[species %in% c("yellow cedar", "western red cedar"),
         species := "western redcedar"]






treelist$biomass <- NULL

treelist$biomass <- biomassCalculator(species = treelist$species,
                                      DBH = treelist$DBH,
                                      heightIncluded = FALSE,
                                      paperSource = "Ung2008")
uncalculatedSpecies <- unique(treelist[is.na(biomass),]$species)
# character(0)

setnames(treelist, "biomass", "biomass_DBH_only")


## for the trees that have both DBH and height, calculate biomass based on both

treelist[!is.na(MEAS_HT), biomass_DBH_HT := biomassCalculator(species = species,
                                                              DBH = DBH,
                                                              heightIncluded = TRUE,
                                                              height = MEAS_HT,
                                                              paperSource = "Ung2008")]
treelist[, BA := pi*(DBH/200)^2]


## assign ingrowth
treelist[, minYR_sample := min(MEAS_YR), by = "SAMP_ID"]
treelist[, minYR_tree := min(MEAS_YR), by = "unitreeid"]

treelist[minYR_sample != minYR_tree & minYR_tree == MEAS_YR, ingrowth := 1]

treelist[is.na(ingrowth), ingrowth := 0]

treelist <- treelist[!(ingrowth == 1 & Dead == 1),]

treelist[,':='(obslen = NULL,
               myrlen = NULL,
               minYR_sample = NULL,
               minYR_tree = NULL)]
treelist <- treelist[,.(unitreeid, SAMP_ID, PLOT_NO,
                        TREE_NO, Species_org = orgSpecies,
                        species, species_type,
                        MEAS_YR, CROWN_CLASS_CODE,
                        HT = MEAS_HT, DBH, DAM_AGENT1, DAM_AGENT2,
                        Dead, Ingrowth = ingrowth, TREE_STEM_MAP_BEARING, TREE_STEM_MAP_SLOPE_DISTANCE,
                        TREE_STEM_MAP_HORIZ_DISTANCE, MEAS_DATE, action, DBH_action,
                        biomass_DBH_only, biomass_DBH_HT, BA)]


saveRDS(treelist, file.path(".", "data", "finalBCdata_long.rds"))

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


samples_tree_meas <- treelist[order(unitreeid, MEAS_YR)]
samples_tree_meas[, ':='(MEAS_YR_FIN = shift(MEAS_YR, type = "lead"),
                         DBH_FIN = shift(DBH, type = "lead"),
                         HT_FIN = shift(HT, type = "lead"),
                         BIOMASS_FIN = shift(biomass_DBH_only, type = "lead"),
                         BA_FIN = shift(BA, type = "lead"),
                         Dead = shift(Dead, type = "lead")),
         by = "unitreeid"]

samples_tree_meas <- samples_tree_meas[!is.na(BA_FIN),
                                       .(SAMP_ID, PLOT_NO, TREE_NO, unitreeid,
                                         Species_org, species, species_type, TREE_STEM_MAP_BEARING,
                                         TREE_STEM_MAP_SLOPE_DISTANCE, 
                                         MEAS_YR_INI = MEAS_YR,
                                         MEAS_YR_FIN, 
                                         DBH_INI = DBH,
                                         DBH_FIN,
                                         HT_INI = HT,
                                         HT_FIN,
                                         BIOMASS_INI = biomass_DBH_only,
                                         BIOMASS_FIN,
                                         BA_INI = BA,
                                         BA_FIN,
                                         Ingrowth, Dead)]

samples_tree_meas_ingr <- samples_tree_meas[Ingrowth == 1,
                                            .(SAMP_ID, PLOT_NO, TREE_NO, unitreeid,
                                              Species_org, species, species_type, TREE_STEM_MAP_BEARING,
                                              TREE_STEM_MAP_SLOPE_DISTANCE, 
                                              MEAS_YR_FIN = MEAS_YR_INI, 
                                              DBH_INI = 0,
                                              DBH_FIN = DBH_INI,
                                              HT_INI = 0,
                                              HT_FIN = HT_INI,
                                              BIOMASS_INI = 0,
                                              BIOMASS_FIN = BIOMASS_INI,
                                              BA_INI = 0,
                                              BA_FIN = BA_INI,
                                              Ingrowth, Dead = 0)]
samples_tree_meas_ingr <- merge(samples_tree_meas_ingr, unique(samples_tree_meas[,.(SAMP_ID, PLOT_NO, MEAS_YR_INI, MEAS_YR_FIN)]),
                                by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_FIN"),
                                all.x = TRUE)
samples_tree_meas[, Ingrowth := 0]

samples_tree_meas <- rbindlist(list(samples_tree_meas, samples_tree_meas_ingr),
                               use.names = TRUE)

samples_tree_meas <- samples_tree_meas[order(SAMP_ID, PLOT_NO, TREE_NO, MEAS_YR_INI),]


#########################
## summarise at stand level
#########################
## 1. prepare plot area for each plot
plots_org <- readRDS(file.path(".", "data", "plots.rds")) %>% data.table
plots <- data.table::copy(plots_org)
plots[!is.na(PLOT_RADIUS), plot_area := pi*(PLOT_RADIUS^2)]
plots[is.na(plot_area) | plot_area == 0, plot_area := PLOT_LENGTH*PLOT_WIDTH]

nrow(unique(plots, by = c("SAMP_ID", "PLOT_NO"))) == nrow(plots)
plots[, plot_area := plot_area/10000]

samples_org <- haven::read_sas(file.path(".", "data", "sasfiles", 
                                         "psp_sumry_all.sas7bdat")) %>%
  data.table

names(samples_org) <- toupper(names(samples_org))

samples_org_area <- samples_org[NO_PLOTS == 1,
                                        .(SAMP_ID, AREA_PM)]
samples_org_area <- samples_org_area[!(AREA_PM %in% c(0, NA)),]
samples_org_area <- unique(samples_org_area, by = "SAMP_ID")

plots <- merge(plots, samples_org_area,
               by = "SAMP_ID",
               all.x = TRUE)

## check the whether plot area is consistent between two data sources
plots[plot_area %in% c(NA, 0),
      plot_area := AREA_PM]

plots[plot_area %in% c(NA, 0),]


nrow(plots) == nrow(unique(plots, by = c("SAMP_ID", "PLOT_NO")))



sample_age <- samples_org[,.(SAMP_ID, MEAS_DT, MEAS_YR, 
                             TOT_STAND_AGE, TOT_STAND_AGE_DUP)]

sample_age[, MEAS_YR_new := as.numeric(substr(MEAS_DT, 1, 4))]

sample_age[is.na(TOT_STAND_AGE) | is.na(TOT_STAND_AGE_DUP),]

sample_age[, age_dif := TOT_STAND_AGE - TOT_STAND_AGE_DUP]
sample_age[is.na(TOT_STAND_AGE) & is.na(TOT_STAND_AGE_DUP), age_dif := 0]
range(sample_age$age_dif)
sample_age <- sample_age[!is.na(TOT_STAND_AGE),]

sample_age[, baseyear := min(MEAS_YR),
           by = "SAMP_ID"]
sample_age[baseyear == MEAS_YR, basesa := TOT_STAND_AGE]
sample_age[, basesa := max(basesa, na.rm = TRUE), by = "SAMP_ID"]

sample_age[, stand_age_new := MEAS_YR - baseyear + basesa]
sample_age[TOT_STAND_AGE != stand_age_new,]

sample_age <- unique(sample_age[,.(SAMP_ID, MEAS_DT, MEAS_YR, TOT_STAND_AGE, 
                                   baseyear)],
                     by = c("SAMP_ID", "MEAS_YR"))
sample_age[, basesa := TOT_STAND_AGE - (MEAS_YR - baseyear)]
sample_age[, basesa_mean := round(mean(basesa)),
           by = "SAMP_ID"]
sample_age <- unique(sample_age[,.(SAMP_ID, baseyear, basesa_mean)],
                     by = "SAMP_ID")
plots <- merge(plots, sample_age,
               by = "SAMP_ID",
               all.x = TRUE)
plots[, AREA_PM := NULL]


saveRDS(plots, file.path(".", "data", "plots_final.rds"))





samples_stand_ini <- samples_tree_meas[Ingrowth == 0,
                                       .(NoTree_INI = length(species),
                                           Biomass_INI = sum(BIOMASS_INI),
                                           BA_INI = sum(BA_INI)),
                                        by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI", "MEAS_YR_FIN")]
samples_stand_fin <- samples_tree_meas[Dead == 0,
                                       .(NoTree_FIN = length(species),
                                           Biomass_FIN = sum(BIOMASS_FIN),
                                           BA_FIN = sum(BA_FIN)),
                                        by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_FIN")]
samples_stand_meas <- merge(samples_stand_ini, samples_stand_fin,
                            by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_FIN"),
                            all.x = TRUE)
rm(samples_stand_ini)

samples_stand_grow <- samples_tree_meas[Dead == 0 & Ingrowth == 0,
                                    .(NoTree_GROW = length(species),
                                      Biomass_GROW = sum(BIOMASS_FIN - BIOMASS_INI),
                                      BA_GROW = sum(BA_FIN - BA_INI)),
                                    by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI")]
samples_stand_meas <- merge(samples_stand_meas, samples_stand_grow,
           by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI"),
           all.x = TRUE)
rm(samples_stand_grow)

samples_stand_ingro <- samples_tree_meas[Ingrowth == 1,
                                .(NoTree_INGR = length(species),
                                  Biomass_INGR = sum(BIOMASS_FIN),
                                  BA_INGR = sum(BA_FIN)),
                                by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI")]
samples_stand_meas <- merge(samples_stand_meas, samples_stand_ingro,
                            by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI"),
                            all.x = TRUE)
samples_stand_meas[is.na(NoTree_INGR), NoTree_INGR := 0]
samples_stand_meas[is.na(Biomass_INGR), Biomass_INGR := 0]
samples_stand_meas[is.na(BA_INGR), BA_INGR := 0]
rm(samples_stand_ingro)

samples_stand_dead <- samples_tree_meas[Dead == 1,
                                .(NoTree_DEAD = length(species),
                                  Biomass_DEAD = sum(BIOMASS_FIN),
                                  BA_DEAD = sum(BA_FIN)),
                                by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI")]
samples_stand_meas <- merge(samples_stand_meas, samples_stand_dead,
                            by = c("SAMP_ID", "PLOT_NO", "MEAS_YR_INI"),
                            all.x = TRUE)
samples_stand_meas[is.na(NoTree_DEAD), NoTree_DEAD := 0]
samples_stand_meas[is.na(Biomass_DEAD), Biomass_DEAD := 0]
samples_stand_meas[is.na(BA_DEAD), BA_DEAD := 0]
rm(samples_stand_dead)



samples_stand_meas <- merge(samples_stand_meas,
                            plots,
                            by = c("SAMP_ID", "PLOT_NO"),
                            all.x = TRUE)

samples_stand_meas[, MEAS_INTERVAL := MEAS_YR_FIN - MEAS_YR_INI]

## select the stands that been selected initially
samples_meas <- readRDS(file.path(".", "data", "sample_meas_final.rds"))
samples_meas[, samp_meas := paste0(SAMP_ID, "_", MEAS_YR_INI, "_", MEAS_YR_FIN)]
samples_stand_meas[, samp_meas := paste0(SAMP_ID, "_", MEAS_YR_INI, "_", MEAS_YR_FIN)]
samples_stand_meas <- samples_stand_meas[samp_meas %in% samples_meas$samp_meas,]



## select interval less than 20 years
range(samples_stand_meas$MEAS_INTERVAL)
# 1 44
samples_stand_meas <- samples_stand_meas[MEAS_INTERVAL <= 20,]

## measured at least three times
samples_stand_meas[, meas_times := length(plot_area),
                   by = c("SAMP_ID", "PLOT_NO")]
samples_stand_meas <- samples_stand_meas[meas_times >= 2,]


## no of trees at first measurement is more than 50 trees
samples_stand_meas[, minyear := min(MEAS_YR_INI),
                   by = c("SAMP_ID", "PLOT_NO")]
samples_stand_meas[MEAS_YR_INI == minyear, 
                   notree_first := NoTree_INI]
samples_stand_meas[, notree_first := max(notree_first, na.rm = TRUE),
                   by = c("SAMP_ID", "PLOT_NO")]
samples_stand_meas <- samples_stand_meas[notree_first >= 50, ]
samples_stand_meas[,':='(meas_times = NULL,
                         minyear = NULL,
                         notree_first = NULL)]

## select the plots that have plot area

samples_without_plot_area <- unique(samples_stand_meas[is.na(plot_area),.(SAMP_ID, PLOT_NO)])
samples_stand_meas <- samples_stand_meas[!is.na(plot_area),]



## scale up to stand level, by dividing the plot area, 
## for the biomass, the unit is changed to T/ha
## for BA the unit is m2/ha
samples_stand_meas[, ':='(Biomass_AnnChange = (Biomass_FIN - Biomass_INI)/(plot_area * MEAS_INTERVAL * 1000),
                          Biomass_AnnGrowth = (Biomass_GROW)/(plot_area * MEAS_INTERVAL * 1000),
                          Biomass_AnnIngrow = (Biomass_INGR)/(plot_area * MEAS_INTERVAL * 1000),
                          Biomass_AnnDead = (Biomass_DEAD)/(plot_area * MEAS_INTERVAL * 1000),
                          BA_AnnChange = (BA_FIN - BA_INI)/(plot_area * MEAS_INTERVAL),
                          BA_AnnGrowth = (BA_GROW)/(plot_area * MEAS_INTERVAL),
                          BA_AnnIngrow = (BA_INGR)/(plot_area * MEAS_INTERVAL),
                          BA_AnnDead = (BA_DEAD)/(plot_area * MEAS_INTERVAL),
                          Year_mid = (MEAS_YR_INI + MEAS_YR_FIN)/2)]

samples_stand_meas[, SA := basesa_mean + (Year_mid - baseyear)]

samples_stand_meas[, SA_est := min(SA),
                   by = "SAMP_ID"]

samples_stand_meas <- samples_stand_meas[SA_est > 0, ]
range(samples_stand_meas$SA_est)


samples_tree_meas[, samp_meas := paste0(SAMP_ID, "_", MEAS_YR_INI, "_", MEAS_YR_FIN)]
samples_tree_meas <- samples_tree_meas[samp_meas %in% unique(samples_stand_meas$samp_meas),]


saveRDS(samples_tree_meas, file.path(".", "data", "finalBC_tree_data_wide.rds"))
saveRDS(samples_stand_meas, file.path(".", "data", "finalBC_stand_data.rds"))




