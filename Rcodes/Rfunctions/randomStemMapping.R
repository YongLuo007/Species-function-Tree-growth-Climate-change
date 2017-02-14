

randomStemMapping <- function(onePlotoneCensusdata,
                              SizeCutLength, plotSize){
  onePlotoneCensusdata <- onePlotoneCensusdata[order(Biomass),]
  onePlotoneCensusdata[,SizeRank:=1:length(Biomass)]
  onePlotoneCensusdata[, SizeClass:=cut(SizeRank, seq(min(SizeRank), max(SizeRank),
                                                      length = SizeCutLength),
                                        labels = paste("Size", 
                                                       1:(SizeCutLength-1), sep = ""),
                                        include.lowest = TRUE)]
  
  for(i in 1:(SizeCutLength-1)){
    indisizeclass <- onePlotoneCensusdata[SizeClass == paste("Size", i, sep = "")]
    NofTree <- round(nrow(indisizeclass)*10000/plotSize)
    pts <- data.table(data.frame(rpoint(NofTree), stringsAsFactors = FALSE))
    pts[,':='(x = 100*x-50, y = 100*y-50,
              TreeNumber = sample(unique(indisizeclass$TreeNumber),
                                  size = length(x),
                                  replace = TRUE))]
    ptss <- setkey(pts, TreeNumber)[setkey(indisizeclass[,.(TreeNumber,  
                                                            Year, PlotNumber,
                                                            Species,
                                                            Biomass)],
                                           TreeNumber),
                                    nomatch = 0]
    if(i == 1){
      alltreemapping <- ptss
    } else {
      alltreemapping <- rbind(alltreemapping, ptss)
    }
  }
  return(alltreemapping)
}



