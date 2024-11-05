#' @export
extractBenchmarkData <- function(databaseDetails, 
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