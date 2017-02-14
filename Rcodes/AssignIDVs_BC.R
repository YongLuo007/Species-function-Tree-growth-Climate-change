rm(list = ls())
requiredPackages <- c("data.table", "dplyr", "ggplot2", "nlme", "MuMIn", "parallel")
lapply(requiredPackages, library, character.only = TRUE)

workPath <- "~/Github/Species-function-Tree-growth-Climate-change"
selectedTrees <- fread(file.path(workPath,"data", "finalBCdata.csv"))

selectedPlots <- fread(file.path(workPath, "data", "BCSelectedPlot.csv"))

standAge <- selectedPlots[,.(meanSA = mean(tot_stand_age)), 
                          by = c("SAMP_ID", "meas_yr")]

standAge[, baseYear:=min(meas_yr), by = SAMP_ID]
standAge[, baseSA := meanSA-(meas_yr-baseYear)]
standAge <- standAge[,.(baseSA = mean(baseSA), baseYear = mean(baseYear)), 
                     by = SAMP_ID]


selectedTrees <- setkey(selectedTrees, SAMP_ID)[setkey(standAge, SAMP_ID),
                                                nomatch = 0]
selectedTrees[, IniSA:=Year-baseYear+baseSA]

set(selectedTrees, , c("plotMeasureTime", "treeMeasureTime",
                       "treeMinGR", "baseSA", "baseYear"),
    NULL)

setnames(selectedTrees, "Year", "IniYear")

firstRun <- TRUE
if(firstRun){
  analysesData <- data.table::copy(selectedTrees)[, uniTreeID := paste(SAMP_ID, "_", uniTreeID, sep = "")]
  analysesData <- analysesData[allCensusLiveTree == "yes" & positiveGrowthTree == "yes",]
  myFunction <- function(sizeWeight, disWeight, analysesData, workPath){
    output <- data.table(sizeWeight = character(),
                         disWeight = numeric(), HAIC = numeric())
    for(i in sizeWeight){
      for(j in disWeight){
        newCIdata <- fread(file.path(workPath, "data", "AllCompetitioinData_BC",
                                     paste("CompetitionData_DW",j, "_SW", i, ".csv", sep = "")))
        analysesDataAll <- setkey(analysesData, 
                                  uniTreeID, IniYear)[setkey(newCIdata[, .(uniTreeID, IniYear, H)],
                                                             uniTreeID, IniYear),
                                                      nomatch = 0]
        
        analysesDataAll[,':='(logY = log(BiomassGR),
                              logHctd = log(H)-mean(log(H)))]
        HModel <- lme(logY~logHctd,
                      random = ~1|SAMP_ID/uniTreeID,
                      data = analysesDataAll,
                      control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))
        aictable <- data.table(sizeWeight = i, 
                               disWeight = j, HAIC = AIC(HModel))
        output <- rbind(output, aictable)
      }
    }
    return(output)
  }
  
  inputWeights <- list()
  m <- 1
  for(i in seq(0, 10, by = 0.1)){
    for(j in seq(0, 2, by = 0.1)){
      inputWeights[[m]] <- c(i, j)
      m <- m+1
    }
  }
  
  
  cl <- parallel::makeCluster(parallel::detectCores()-1)
  parallel::clusterExport(cl, c("lme", "AIC", "lmeControl", "data.table", "myFunction",
                                "setkey", "fread", "workPath", "analysesData"))
  
  allresults <- parLapply(cl, inputWeights, 
                          function(y) myFunction(sizeWeight = y[1], disWeight = y[2], 
                                                 analysesData = analysesData, workPath = workPath))
  stopCluster(cl)
  
  for(i in 1:length(allresults)){
    if(i == 1){
      output <- allresults[[i]]
    } else {
      output <- rbind(output, allresults[[i]])
    }
  }
  
  write.csv(output, file.path(workPath, "data", 
                              "bestAandB_BC.csv"),
            row.names = F)
} else {
  output <- fread(file.path(workPath, "data", 
                            "bestAandB_BC.csv"))
}


a <- melt(output, id.vars = c("sizeWeight", "disWeight"), 
          measure.vars = c("HAIC"),
          value.name = "Value")

a[,minvalue:=min(Value), by = c("variable")]
bestWeightTable <- a[Value == minvalue,]


HsizeWeight <- bestWeightTable[variable == "HAIC",]$sizeWeight
HdisWeight <- bestWeightTable[variable == "HAIC",]$disWeight
HCIdata <- fread(file.path(workPath, "data", "AllCompetitioinData_BC",
                           paste("CompetitionData_DW",HdisWeight, "_SW", HsizeWeight, ".csv", sep = "")))
selectedTrees[,uniTreeID:=paste(SAMP_ID, "_", uniTreeID, sep = "")]
selectedTrees <- setkey(selectedTrees, uniTreeID, IniYear)[setkey(HCIdata, uniTreeID, IniYear), 
                                                           nomatch = 0]

selectedTrees[,V1:=NULL]

write.csv(selectedTrees, file.path(workPath, "data", "finalData_BC.csv"),
          row.names = FALSE)


source(file.path(workPath, "Rcodes", "Rfunctions", "mixedModelSelection.R"))


analysesData <- selectedTrees[positiveGrowthTree == "yes" & allCensusLiveTree == "yes",]
analysesData[,Year:=(IniYear+FinYear)/2]


analysesData[,':='(logY = log(BiomassGR), 
                  logDBHctd = log(IniDBH)-mean(log(IniDBH)), 
                  Yearctd = Year-mean(Year),
                  logHctd = log(H)-mean(log(H)),
                  logSActd = log(IniSA+measInterval/2)-mean(log(IniSA+measInterval/2)))]
allHoutput <- mixedModelSelection(DV = "logY", 
                                  IDV = c("logDBHctd", "Yearctd", "logHctd",
                                          "logSActd"),
                                  maxInteraction = 2,
                                  ICTerm = "AIC",
                                  ICCut = 2,
                                  data = analysesData,
                                  random = ~1|SAMP_ID/uniTreeID, 
                                  control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))

allHbestFormula <- as.formula(paste("logY~", paste(allHoutput$bestIDV, collapse = "+")))
allHbestModel <- lme(fixed = allHbestFormula,
                     data = analysesData,
                     random = ~1|SAMP_ID/uniTreeID, 
                     control = lmeControl(opt="optim", maxIter=50000, msMaxIter = 50000))

save.image(file.path(workPath, "data", 
                     "BestYearModels.RData"))
