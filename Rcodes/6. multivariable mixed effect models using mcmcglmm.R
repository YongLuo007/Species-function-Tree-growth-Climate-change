rm(list = ls())
library(data.table)
library(MCMCglmm)

inputdata <- readRDS(file.path(".", "data", "finalBC_stand_data1.rds"))
inputdata[, ':='(AGB = Biomass_AnnChange,
                 AGB_GI = Biomass_AnnGrowth + Biomass_AnnIngrow,
                 AGB_M = -Biomass_AnnDead,
                 Plotnumber = paste0(SAMP_ID, "_", PLOT_NO))]

inputdata[,':='(lnSActd = log(SA) - mean(log(SA)),
                Yearctd = Year_mid - mean(Year_mid),
                wi = (plot_area^0.5)*MEAS_INTERVAL/max((plot_area^0.5)*MEAS_INTERVAL))]

fullmodel1 <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                       random = ~corg(trait):SAMP_ID,
                       rcov = ~corg(trait):units,
                       data = inputdata,
                       family = c("gaussian", "gaussian", "gaussian"))

summary(fullmodel1)
#                                post.mean   l-95% CI   u-95% CI eff.samp  pMCMC    
#   traitAGB                     3.1731152  3.1303518  3.2166789   1000.0 <0.001 ***
#   traitAGB_GI                  4.8010289  4.7563204  4.8401751   1000.0 <0.001 ***
#   traitAGB_M                  -1.5658359 -1.6071606 -1.5223390   1000.0 <0.001 ***
#   traitAGB:lnSActd            -4.2018074 -4.3146128 -4.1001972   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd         -2.9604810 -3.0718797 -2.8546924   1000.0 <0.001 ***
#   traitAGB_M:lnSActd          -0.1043241 -0.2248366  0.0002503   1000.0  0.058 .  
#   traitAGB:Yearctd             0.0003224 -0.0021259  0.0024398   1000.0  0.810    
#   traitAGB_GI:Yearctd          0.0186612  0.0165641  0.0209711   1000.0 <0.001 ***
#   traitAGB_M:Yearctd          -0.0402529 -0.0423491 -0.0379111   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.0159364 -0.0200620 -0.0116817   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd  0.0008739 -0.0032672  0.0050850    985.5  0.688    
#   traitAGB_M:lnSActd:Yearctd  -0.0003974 -0.0039814  0.0037204   1000.0  0.818    


## by forest type
unique(inputdata$FOREST_TYPE_est)
# [1] "western hemlock"          "Other coniferous species"
# [3] "Mixed"                    "douglas-fir"             
# [5] "Deciduous species"        "lodgepole pine"          
# [7] "Spruce"  

# 1. western hemlock dominant forests
length(unique(inputdata[FOREST_TYPE_est == "western hemlock",]$SAMP_ID))
# 419 samples
fullmodel_west_hemlock <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                       random = ~corg(trait):SAMP_ID,
                       rcov = ~corg(trait):units,
                       data = inputdata[FOREST_TYPE_est == "western hemlock",],
                       family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_west_hemlock)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     5.125094  5.017150  5.228539   1125.0 <0.001 ***
#   traitAGB_GI                  7.219529  7.110094  7.323609   1000.0 <0.001 ***
#   traitAGB_M                  -2.023380 -2.132092 -1.910472    781.6 <0.001 ***
#   traitAGB:lnSActd            -5.625159 -5.895252 -5.307530   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd         -4.028218 -4.311056 -3.719188   1000.0 <0.001 ***
#   traitAGB_M:lnSActd          -0.232697 -0.484785  0.025817   1135.4  0.088 .  
#   traitAGB:Yearctd             0.016134  0.009858  0.022118    837.6 <0.001 ***
#   traitAGB_GI:Yearctd          0.036555  0.030319  0.043247    786.5 <0.001 ***
#   traitAGB_M:Yearctd          -0.038278 -0.043928 -0.032710   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.034951 -0.046081 -0.023424   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd  0.002566 -0.007583  0.013242   1000.0  0.622    
#   traitAGB_M:lnSActd:Yearctd  -0.021722 -0.031967 -0.011128   1000.0 <0.001 ***

# 2. Other coniferous species
length(unique(inputdata[FOREST_TYPE_est == "Other coniferous species",]$SAMP_ID))
# 225 samples
fullmodel_west_Other_coniferous_species <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                                   random = ~corg(trait):SAMP_ID,
                                   rcov = ~corg(trait):units,
                                   data = inputdata[FOREST_TYPE_est == "Other coniferous species",],
                                   family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_west_Other_coniferous_species)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     4.464068  4.299946  4.597692   1235.7 <0.001 ***
#   traitAGB_GI                  5.710223  5.544227  5.845965    813.6 <0.001 ***
#   traitAGB_M                  -1.318420 -1.476100 -1.172859   1098.1 <0.001 ***
#   traitAGB:lnSActd            -6.513888 -6.851927 -6.145757   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd         -5.275727 -5.663374 -4.973341    886.4 <0.001 ***
#   traitAGB_M:lnSActd           0.063625 -0.315659  0.406457   1000.0  0.698    
#   traitAGB:Yearctd             0.010471  0.001403  0.017558   1000.0  0.018 *  
#   traitAGB_GI:Yearctd          0.025240  0.017804  0.034156   1105.1 <0.001 ***
#   traitAGB_M:Yearctd          -0.037408 -0.044917 -0.029043   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd     0.018441  0.001879  0.034449    902.5  0.026 *  
#   traitAGB_GI:lnSActd:Yearctd -0.006359 -0.022549  0.008976   1000.0  0.464    
#   traitAGB_M:lnSActd:Yearctd   0.032508  0.017160  0.047579   1000.0 <0.001 ***

# 3. mixed
length(unique(inputdata[FOREST_TYPE_est == "Mixed",]$SAMP_ID))
# 343 samples
fullmodel_mixed <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                                                    random = ~corg(trait):SAMP_ID,
                                                    rcov = ~corg(trait):units,
                                                    data = inputdata[FOREST_TYPE_est == "Mixed",],
                                                    family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_mixed)
#                                post.mean   l-95% CI   u-95% CI eff.samp  pMCMC    
#   traitAGB                     3.1101999  2.9819893  3.2332072   1000.0 <0.001 ***
#   traitAGB_GI                  4.8009444  4.6780192  4.9302119   1000.0 <0.001 ***
#   traitAGB_M                  -1.7068494 -1.8250077 -1.5825813    880.9 <0.001 ***
#   traitAGB:lnSActd            -2.4871497 -2.8204366 -2.1096677   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd         -0.9609146 -1.2898614 -0.5894878   1000.0 <0.001 ***
#   traitAGB_M:lnSActd          -1.0075109 -1.3659206 -0.6371266   1000.0 <0.001 ***
#   traitAGB:Yearctd            -0.0120672 -0.0179984 -0.0050592   1000.0 <0.001 ***
#   traitAGB_GI:Yearctd         -0.0070917 -0.0134690 -0.0005432   1000.0  0.030 *  
#   traitAGB_M:Yearctd          -0.0173167 -0.0235137 -0.0102532   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd     0.0222033  0.0109396  0.0359160   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd  0.0315142  0.0190635  0.0440521   1000.0 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd   0.0059867 -0.0058664  0.0174430   1000.0  0.326   

# 4. douglas-fir
length(unique(inputdata[FOREST_TYPE_est == "douglas-fir",]$SAMP_ID))
# 746 samples
fullmodel_douglas_fir <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                            random = ~corg(trait):SAMP_ID,
                            rcov = ~corg(trait):units,
                            data = inputdata[FOREST_TYPE_est == "douglas-fir",],
                            family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_douglas_fir)
#                               post.mean l-95% CI u-95% CI eff.samp  pMCMC    
#   traitAGB                      3.50161  3.41835  3.58721   1000.0 <0.001 ***
#   traitAGB_GI                   4.90955  4.82752  4.99567   1000.0 <0.001 ***
#   traitAGB_M                   -1.57702 -1.66430 -1.49020    702.0 <0.001 ***
#   traitAGB:lnSActd             -4.64227 -4.91096 -4.39423    972.9 <0.001 ***
#   traitAGB_GI:lnSActd          -4.07810 -4.35356 -3.83636    911.1 <0.001 ***
#   traitAGB_M:lnSActd            1.55718  1.31710  1.84155   1000.0 <0.001 ***
#   traitAGB:Yearctd              0.03810  0.03279  0.04255   1000.0 <0.001 ***
#   traitAGB_GI:Yearctd           0.05112  0.04602  0.05635    802.5 <0.001 ***
#   traitAGB_M:Yearctd           -0.04942 -0.05474 -0.04553   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd     -0.02534 -0.03368 -0.01690   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd  -0.02237 -0.03105 -0.01451   1000.0 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd    0.01963  0.01144  0.02790   1000.0 <0.001 ***

# 5. Deciduous species
# [1] "western hemlock"          "Other coniferous species"
# [3] "Mixed"                    "douglas-fir"             
# [5] "Deciduous species"        "lodgepole pine"          
# [7] "Spruce"  
length(unique(inputdata[FOREST_TYPE_est == "Deciduous species",]$SAMP_ID))
# 159 samples
fullmodel_deciduous_species <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                                  random = ~corg(trait):SAMP_ID,
                                  rcov = ~corg(trait):units,
                                  data = inputdata[FOREST_TYPE_est == "Deciduous species",],
                                  family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_deciduous_species)
#                               post.mean l-95% CI u-95% CI eff.samp  pMCMC    
#   traitAGB                      2.39736  2.09943  2.74144   1000.0 <0.001 ***
#   traitAGB_GI                   3.47834  3.15342  3.81675   1000.0 <0.001 ***
#   traitAGB_M                   -0.46708 -0.79072 -0.16128   1000.0  0.006 ** 
#   traitAGB:lnSActd             -2.66888 -3.36739 -1.94702   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd          -2.14751 -2.84788 -1.41373   1000.0 <0.001 ***
#   traitAGB_M:lnSActd            1.20280  0.54285  1.87400    893.9 <0.001 ***
#   traitAGB:Yearctd             -0.02773 -0.04584 -0.01183   1000.0 <0.001 ***
#   traitAGB_GI:Yearctd           0.02555  0.00977  0.04382   1000.0  0.002 ** 
#   traitAGB_M:Yearctd           -0.08122 -0.09740 -0.06638   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd     -0.11424 -0.13911 -0.09319    900.4 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd  -0.05164 -0.07516 -0.02847    911.1 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd   -0.04263 -0.06269 -0.01987   1000.0 <0.001 ***


# 6. lodgepole pine
# [1] "western hemlock"          "Other coniferous species"
# [3] "Mixed"                    "douglas-fir"             
# [5] "Deciduous species"        "lodgepole pine"          
# [7] "Spruce"  
length(unique(inputdata[FOREST_TYPE_est == "lodgepole pine",]$SAMP_ID))
# 667 samples
fullmodel_lodgepole_pine <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                                        random = ~corg(trait):SAMP_ID,
                                        rcov = ~corg(trait):units,
                                        data = inputdata[FOREST_TYPE_est == "lodgepole pine",],
                                        family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_lodgepole_pine)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     0.996804  0.886183  1.099019     1000 <0.001 ***
#   traitAGB_GI                  2.358173  2.248546  2.471053     1000 <0.001 ***
#   traitAGB_M                  -0.974206 -1.082911 -0.866969     1000 <0.001 ***
#   traitAGB:lnSActd            -5.953847 -6.351302 -5.521021     1000 <0.001 ***
#   traitAGB_GI:lnSActd         -4.084287 -4.493597 -3.666634     1000 <0.001 ***
#   traitAGB_M:lnSActd           0.758855  0.270542  1.189697     1000 <0.001 ***
#   traitAGB:Yearctd            -0.027158 -0.034855 -0.020201     1000 <0.001 ***
#   traitAGB_GI:Yearctd          0.034568  0.028132  0.042914     1000 <0.001 ***
#   traitAGB_M:Yearctd          -0.101049 -0.108399 -0.093441     1000 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.117364 -0.131249 -0.102364     1000 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd -0.015573 -0.029493 -0.001761     1106  0.036 *  
#   traitAGB_M:lnSActd:Yearctd  -0.075602 -0.088864 -0.061317     1000 <0.001 ***


# 6. Spruce
# [1] "western hemlock"          "Other coniferous species"
# [3] "Mixed"                    "douglas-fir"             
# [5] "Deciduous species"        "lodgepole pine"          
# [7] "Spruce"  
length(unique(inputdata[FOREST_TYPE_est == "Spruce",]$SAMP_ID))
# 222 samples
fullmodel_spruce <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                                     random = ~corg(trait):SAMP_ID,
                                     rcov = ~corg(trait):units,
                                     data = inputdata[FOREST_TYPE_est == "Spruce",],
                                     family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_spruce)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     4.376721  4.230502  4.539779     1000 <0.001 ***
#   traitAGB_GI                  6.725537  6.571890  6.879370     1000 <0.001 ***
#   traitAGB_M                  -2.317497 -2.459980 -2.164215     1000 <0.001 ***
#   traitAGB:lnSActd            -5.706017 -6.066885 -5.401994     1000 <0.001 ***
#   traitAGB_GI:lnSActd         -4.282336 -4.600099 -3.969387     1000 <0.001 ***
#   traitAGB_M:lnSActd          -0.810901 -1.127488 -0.524856     1000 <0.001 ***
#   traitAGB:Yearctd             0.039709  0.031152  0.050051     1000 <0.001 ***
#   traitAGB_GI:Yearctd          0.056228  0.047607  0.065924     1000 <0.001 ***
#   traitAGB_M:Yearctd          -0.035224 -0.043835 -0.026962     1000 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.024580 -0.038328 -0.012072     1230 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd -0.022014 -0.034962 -0.009856     1000 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd   0.014308  0.001523  0.025827     1000  0.024 * 

###################
## for old forests, i.e., sa>80
###################
length(unique(inputdata[SA_est < 40,]$SAMP_ID))
# 314 samples
fullmodel_40less <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                          random = ~corg(trait):SAMP_ID,
                          rcov = ~corg(trait):units,
                          data = inputdata[SA_est < 40,],
                          family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_40less)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     2.094542  1.892947  2.340061   1000.0 <0.001 ***
#   traitAGB_GI                  4.158053  3.920300  4.366182   1000.0 <0.001 ***
#   traitAGB_M                  -0.947531 -1.174552 -0.739453   1000.0 <0.001 ***
#   traitAGB:lnSActd            -7.355606 -7.734584 -6.995546   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd         -5.720566 -6.088208 -5.349046   1000.0 <0.001 ***
#   traitAGB_M:lnSActd           0.371502  0.028553  0.702855   1000.0  0.030 *  
#   traitAGB:Yearctd             0.024854  0.015096  0.035423    884.3 <0.001 ***
#   traitAGB_GI:Yearctd          0.039217  0.029545  0.049377   1000.0 <0.001 ***
#   traitAGB_M:Yearctd          -0.055250 -0.064393 -0.045106   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.062640 -0.076961 -0.049223   1187.3 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd -0.050771 -0.064279 -0.037398   1127.5 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd  -0.003114 -0.016057  0.010749   1000.0  0.648    



length(unique(inputdata[SA_est >= 40 & SA_est < 60,]$SAMP_ID))
# 996 samples
fullmodel_4060 <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                          random = ~corg(trait):SAMP_ID,
                          rcov = ~corg(trait):units,
                          data = inputdata[SA_est >= 40 & SA_est < 60,],
                          family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_4060)
#                               post.mean l-95% CI u-95% CI eff.samp  pMCMC    
#   traitAGB                      3.14876  3.02771  3.26340     1000 <0.001 ***
#   traitAGB_GI                   4.81993  4.69674  4.93094     1000 <0.001 ***
#   traitAGB_M                   -1.08799 -1.21090 -0.97412     1000 <0.001 ***
#   traitAGB:lnSActd             -3.27390 -3.67690 -2.87340     1204 <0.001 ***
#   traitAGB_GI:lnSActd          -1.00911 -1.39691 -0.61709     1000 <0.001 ***
#   traitAGB_M:lnSActd           -0.06246 -0.46711  0.31014     1000  0.742    
#   traitAGB:Yearctd             -0.04387 -0.05084 -0.03726     1000 <0.001 ***
#   traitAGB_GI:Yearctd          -0.01947 -0.02639 -0.01271     1000 <0.001 ***
#   traitAGB_M:Yearctd           -0.06214 -0.06809 -0.05499     1000 <0.001 ***
#   traitAGB:lnSActd:Yearctd     -0.15146 -0.17069 -0.13399     1000 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd  -0.03721 -0.05465 -0.01776     1161 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd   -0.08482 -0.10298 -0.06559     1000 <0.001 ***


length(unique(inputdata[SA_est >= 60 & SA_est < 80,]$SAMP_ID))
# 804 samples
fullmodel_6080 <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                          random = ~corg(trait):SAMP_ID,
                          rcov = ~corg(trait):units,
                          data = inputdata[SA_est >= 60 & SA_est < 80,],
                          family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_6080)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     2.962226  2.863643  3.060504   1000.0 <0.001 ***
#   traitAGB_GI                  4.484251  4.388797  4.582260   1000.0 <0.001 ***
#   traitAGB_M                  -1.605571 -1.705874 -1.512365    885.7 <0.001 ***
#   traitAGB:lnSActd            -3.239427 -3.796532 -2.702615    884.0 <0.001 ***
#   traitAGB_GI:lnSActd         -1.372652 -1.893456 -0.796951   1000.0 <0.001 ***
#   traitAGB_M:lnSActd          -0.679236 -1.197166 -0.185129   1000.0  0.004 ** 
#   traitAGB:Yearctd             0.004036 -0.003579  0.011093   1000.0  0.304    
#   traitAGB_GI:Yearctd          0.014565  0.007935  0.022556   1000.0 <0.001 ***
#   traitAGB_M:Yearctd          -0.028711 -0.036116 -0.022207   1105.2 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.137675 -0.162595 -0.117374   1147.8 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd -0.059340 -0.082540 -0.037360   1095.7 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd  -0.068927 -0.090334 -0.047147   1121.3 <0.001 ***


length(unique(inputdata[SA_est >= 80 & SA_est < 100,]$SAMP_ID))
# 386 samples
fullmodel_80100 <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                           random = ~corg(trait):SAMP_ID,
                           rcov = ~corg(trait):units,
                           data = inputdata[SA_est >= 80 & SA_est < 100,],
                           family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_80100)
#                               post.mean  l-95% CI  u-95% CI eff.samp  pMCMC    
#   traitAGB                     1.376425  1.068505  1.720841   1000.0 <0.001 ***
#   traitAGB_GI                  2.977508  2.596573  3.261831   1000.0 <0.001 ***
#   traitAGB_M                  -1.828553 -2.128163 -1.541038   1000.0 <0.001 ***
#   traitAGB:lnSActd             2.493697  1.358654  3.400900   1000.0 <0.001 ***
#   traitAGB_GI:lnSActd          3.471863  2.568863  4.485124   1000.0 <0.001 ***
#   traitAGB_M:lnSActd          -0.161807 -1.133621  0.680704    905.1  0.734    
#   traitAGB:Yearctd            -0.037101 -0.053377 -0.023033   1000.0 <0.001 ***
#   traitAGB_GI:Yearctd         -0.002165 -0.018050  0.012823   1113.9  0.776    
#   traitAGB_M:Yearctd          -0.041048 -0.055204 -0.027273   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd    -0.038761 -0.071444 -0.003233    896.4  0.024 *  
#   traitAGB_GI:lnSActd:Yearctd -0.075807 -0.116069 -0.041672   1000.0 <0.001 ***
#   traitAGB_M:lnSActd:Yearctd   0.030863 -0.001388  0.065773   1048.5  0.076 .  


length(unique(inputdata[SA_est >= 100,]$SAMP_ID))
# 281 samples
fullmodel_100more <- MCMCglmm(cbind(AGB, AGB_GI, AGB_M)~trait/(lnSActd+Yearctd+lnSActd:Yearctd)-1,
                            random = ~corg(trait):SAMP_ID,
                            rcov = ~corg(trait):units,
                            data = inputdata[SA_est >= 100,],
                            family = c("gaussian", "gaussian", "gaussian"))
summary(fullmodel_100more)
#                                post.mean   l-95% CI   u-95% CI eff.samp  pMCMC    
#   traitAGB                     1.2961186  0.8600996  1.7168659   1166.7 <0.001 ***
#   traitAGB_GI                  3.6716507  3.2233594  4.0937779    896.9 <0.001 ***
#   traitAGB_M                  -2.2966029 -2.7609244 -1.8686872   1000.0 <0.001 ***
#   traitAGB:lnSActd             1.0349941  0.3582811  1.7219968   1154.5 <0.001 ***
#   traitAGB_GI:lnSActd          1.2575895  0.6124419  1.9097930    967.8  0.002 ** 
#   traitAGB_M:lnSActd          -0.4105427 -1.1175384  0.2485147   1000.0  0.256    
#   traitAGB:Yearctd            -0.0665610 -0.0835397 -0.0501843   1000.0 <0.001 ***
#   traitAGB_GI:Yearctd          0.0165639  0.0002556  0.0338949   1000.0  0.054 .  
#   traitAGB_M:Yearctd          -0.0754581 -0.0908108 -0.0583891   1000.0 <0.001 ***
#   traitAGB:lnSActd:Yearctd     0.0549119  0.0321602  0.0771697    792.7 <0.001 ***
#   traitAGB_GI:lnSActd:Yearctd -0.0308683 -0.0515377 -0.0069742   1244.1  0.002 ** 
#   traitAGB_M:lnSActd:Yearctd   0.0728672  0.0495171  0.0939817    816.7 <0.001 ***



