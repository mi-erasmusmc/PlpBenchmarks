#' @export
prepareData <- function(databaseDetails, 
                        restrictPlpDataSettings,
                        populationSettings, 
                        covariateSettings, 
                        requiredTrainPositiveEvents, 
                        covariateComparisson, 
                        testSplitFraction, 
                        dataSettings,
                        seed = 42, 
                        analysisName,
                        saveDirectory){
  
  rawDataLocation <- dataSettings$dataLocation
  
  requiredCovariates <- createProcessedDataSettings(covariateComparisson = covariateComparisson,
                                                    analysisName = analysisName, 
                                                    saveDirectory = saveDirectory)
  
  plpDataRaw = PatientLevelPrediction::loadPlpData(file.path(rawDataLocation))
  studyPopulationRaw = readRDS(file = file.path(rawDataLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  
  wholeData <- requiredCovariates %>%
    dplyr::filter(covariates == dataSettings$covariateComparisson)
  
  ParallelLogger::logInfo(paste("Reducing population to specified number for requiredTrainPositiveEvents (", requiredTrainPositiveEvents, ")." ))
  analysisExists <- file.exists(wholeData$processedPlpDataName)
  if (!analysisExists){
  reducedData <- reduceData(studyPopulation = studyPopulationRaw,
                            plpData = plpDataRaw,
                            requiredTrainPositiveEvents = requiredTrainPositiveEvents,
                            testSplitFraction = testSplitFraction, 
                            seed = seed)
  # the reduced data has covariates for demo, conds, drugs and prcdrs
  PatientLevelPrediction::savePlpData(reducedData$plpData, file = wholeData$processedPlpDataName)
  saveRDS(object = reducedData$studyPopulation, file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "studyPopulation.Rds", sep = "_")))
  saveRDS(object = reducedData$summaryStats,  file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "summaryStats.Rds", sep = "_")))
  ParallelLogger::logInfo(print(reducedData$summaryStats))
  } else {
    ParallelLogger::logInfo(paste("Sampled data with covariates ", requiredTrainPositiveEvents, " events in the training set for", analysisName, "exists at", file.path(saveDirectory, analysisName, "processedData")))
    reducedData <- vector("list", 2)
    reducedData$studyPopulation <- readRDS(file = file.path(saveDirectory, analysisName, "processedData", paste(analysisName, "studyPopulation.Rds", sep = "_")))
    reducedData$plpData <- PatientLevelPrediction::loadPlpData(file = wholeData$processedPlpDataName)
  }
  ParallelLogger::logInfo(paste("Done reducing study population."))
  
  ParallelLogger::logInfo(paste("Filtering covariates to create candidate predictor sets..."))
  
  for (i in 1:nrow(requiredCovariates)) {
    
    analysisExists <- file.exists(requiredCovariates$processedPlpDataName[i])
    if (!analysisExists){
      filteredCovariateData <- createComparissonCovariates(plpData = reducedData$plpData,
                                               studyPopulation = reducedData$studyPopulation,
                                               covariates = requiredCovariates$covariates[i])
      PatientLevelPrediction::savePlpData(filteredCovariateData, file = file.path(requiredCovariates$processedPlpDataName[i]))
      ParallelLogger::logInfo(paste("plpData with ", requiredCovariates$covariates[i], "created."))
      } else {
        ParallelLogger::logInfo(paste("Sampled data with ", requiredCovariates$covariates[i], "for", analysisName, "exists at", file.path(requiredCovariates$processedPlpDataName[i])))
      }
    
  }
  
  return(invisible())
}

#' @export
extractRawData <- function(databaseDetails, 
                           restrictPlpDataSettings,
                           populationSettings, 
                           covariateSettings, 
                           requiredTrainPositiveEvents, 
                           covariateComparisson, 
                           testSplitFraction, 
                           dataSettings, 
                           seed = 42, 
                           analysisName,
                           saveDirectory){
  
  # restrictPlpDataSettings <- dataSettings$restrictPlpDataSettings
  # covariateSettings <- dataSettings$covariateSettings
  dataLocation <- dataSettings$dataLocation
  
  ParallelLogger::logInfo(paste("Extracting raw plpData object..."))
  plpDataExists <- file.exists(file.path(dataLocation))
  if (!plpDataExists){
    plpDataRaw <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                     covariateSettings = covariateSettings,
                                                     restrictPlpDataSettings = restrictPlpDataSettings)
    PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(dataLocation) )
  } else {
    ParallelLogger::logInfo(paste('Raw data for', analysisName, 'exists at', file.path(dataLocation)))
  }
  ParallelLogger::logInfo(paste("Done extracting raw data covariates."))
  
  ParallelLogger::logInfo(paste("Creating study population..."))
  populationExists <- file.exists(file.path(dataLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  if (!populationExists){
    if(plpDataExists){
      plpDataRaw <- PatientLevelPrediction::loadPlpData(file = file.path(dataLocation))
    }
    studyPopulationRaw <- PatientLevelPrediction::createStudyPopulation(plpData = plpDataRaw,
                                                                        outcomeId = databaseDetails$outcomeIds, 
                                                                        populationSettings = populationSettings)
    saveRDS(object = studyPopulationRaw, file = file.path(dataLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  } else {
    ParallelLogger::logInfo(paste('Study population for', analysisName, 'exists at', file.path(dataLocation)))
  }
  ParallelLogger::logInfo(paste("Done creating study population."))
  
  return(invisible())
}

#' @export
createProcessedDataSettings <- function(covariateComparisson,
                                        analysisName, 
                                        saveDirectory){
  
  
  if(covariateComparisson == "demographics"){
    processedDataSettings <- data.frame(analysisName = analysisName, 
                                        covariateComparisson = covariateComparisson, 
                                        saveDirectory = saveDirectory, 
                                        covariates = c("demographics_only"),
                                        processedPlpDataName = file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")))
  }
  
  if(covariateComparisson == "demographics and conditions"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 2), 
                                        covariateComparisson = rep(covariateComparisson, 2), 
                                        saveDirectory = rep(saveDirectory, 2), 
                                        covariates = c("demographics only", "demographics and conditions"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")))
                                        )
  }
  
  if(covariateComparisson == "demographics and drugs"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 2), 
                                        covariateComparisson = rep(covariateComparisson, 2), 
                                        saveDirectory = rep(saveDirectory, 2), 
                                        covariates = c("demographics only", "demographics and drugs"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs")))
    )
  }
  
  if(covariateComparisson == "demographics and procedures"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 2), 
                                        covariateComparisson = rep(covariateComparisson, 2), 
                                        saveDirectory = rep(saveDirectory, 2), 
                                        covariates = c("demographics only", "demographics and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")))
    )
  }
  
  if(covariateComparisson == "demographics, conditions and drugs"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 4), 
                                        covariateComparisson = rep(covariateComparisson, 4), 
                                        saveDirectory = rep(saveDirectory, 4), 
                                        covariates = c("demographics only", "demographics and conds", "demographics and drugs", "demographics, conditionsa and drugs"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_druds")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_drugs")))
    )
  }
  
  if(covariateComparisson == "demographics, conditions and procedures"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 4), 
                                        covariateComparisson = rep(covariateComparisson, 4), 
                                        saveDirectory = rep(saveDirectory, 4), 
                                        covariates = c("demographics only", "demographics and conditions", "demographics and procedures", "demographics, conditions and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_prcdrs")))
    )
  }
  
  if(covariateComparisson == "demographics, drugs and procedures"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 4), 
                                        covariateComparisson = rep(covariateComparisson, 4), 
                                        saveDirectory = rep(saveDirectory, 4), 
                                        covariates = c("demographics only", "demographics and drugs", "demographics and procedures", "demographics, drugs and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs_prcdrs")))
    )
  }
  
  if(covariateComparisson == "demographics, conditions, drugs and procedures"){
    
    processedDataSettings <- data.frame(analysisName = rep(analysisName, 8), 
                                        covariateComparisson = rep(covariateComparisson, 8), 
                                        saveDirectory = rep(saveDirectory, 8), 
                                        covariates = c("demographics only", "demographics and conditions", "demographics and drugs", "demographics and procedures", "demographics, conditions and drugs", "demographics, conditions and procedures", "demographics, drugs and procedures", "demographics, conditions, drugs and procedures"),
                                        processedPlpDataName = c(file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_only")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds")),
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_drugs")),
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_drugs_prcdrs")), 
                                                                 file.path(saveDirectory, analysisName, "processedData", paste0(analysisName, "plpData_demo_conds_drugs_prcdrs")))
    ) 
  }
  
  return(processedDataSettings)
}

