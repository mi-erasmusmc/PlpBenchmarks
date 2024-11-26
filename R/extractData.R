#' #' @export
#' extractBenchmarkData <- function(databaseDetails, 
#'                                  restrictPlpDataSettings,
#'                                  populationSettings, 
#'                                  covariateSettings, 
#'                                  requiredTrainPositiveEvents, 
#'                                  covariateComparisson, 
#'                                  testSplitFraction, 
#'                                  dataSettings, 
#'                                  seed = 42, 
#'                                  analysisName,
#'                                  saveDirectory){
#'     
#'     # dataLocation <- dataSettings$dataLocation
#'     dataLocation <- file.path(saveDirectory, "plpData")
#'     
#'     ParallelLogger::logInfo(paste("Extracting raw plpData object..."))
#'     
#'     plpDataExists <- file.exists(file.path(dataLocation))
#'     if (!plpDataExists){
#'       plpDataRaw <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
#'                                                        covariateSettings = covariateSettings,
#'                                                        restrictPlpDataSettings = restrictPlpDataSettings)
#'       PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(dataLocation) )
#'     } else {
#'       ParallelLogger::logInfo(paste('Raw data for', analysisName, 'exists at', file.path(dataLocation)))
#'     }
#'     ParallelLogger::logInfo(paste("Done extracting raw data covariates."))
#'     
#'     ParallelLogger::logInfo(paste("Creating study population..."))
#'     populationExists <- file.exists(file.path(dataLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
#'     if (!populationExists){
#'       if(plpDataExists){
#'         plpDataRaw <- PatientLevelPrediction::loadPlpData(file = file.path(dataLocation))
#'       }
#'       studyPopulationRaw <- PatientLevelPrediction::createStudyPopulation(plpData = plpDataRaw,
#'                                                                           outcomeId = databaseDetails$outcomeIds, 
#'                                                                           populationSettings = populationSettings)
#'       saveRDS(object = studyPopulationRaw, file = file.path(dataLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
#'     } else {
#'       ParallelLogger::logInfo(paste('Study population for', analysisName, 'exists at', file.path(dataLocation)))
#'     }
#'     ParallelLogger::logInfo(paste("Done creating study population."))
#'     
#'     return(invisible())
#'   }
#'   
#' @export
extractBenchmarkData <- function(benchmarkDesign, 
                                 createStudyPopulation = TRUE, 
                                 seed = 42){
  
  uniqueCohorts <- attr(benchmarkDesign, "uniqueCohortSettings")
  minimalCohortsToCreate <- uniqueCohorts$problemId
  minimalCohorts <- benchmarkDesign[c(minimalCohortsToCreate)]
  
  ParallelLogger::logInfo(paste("Will extract", nrow(uniqueCohorts), "unique plpData objects."))
  
  
  
  for (i in seq_along(minimalCohorts)) {
    ParallelLogger::logInfo(paste("Extracting raw plpData object for ", paste(uniqueCohorts$plpDataName[i])))
    databaseDetails <- minimalCohorts[[i]]$databaseDetails
    covariateSettings <- minimalCohorts[[i]]$covariateSettings
    restrictPlpDataSettings <- minimalCohorts[[i]]$restrictPlpDataSettings
    populationSettings <- minimalCohorts[[i]]$populationSettings
    analysisName <- minimalCohorts[[i]]$analysisName
    plpDataLocation <- uniqueCohorts$dataLocation[i]
    studyPopulationLocation <- file.path(stringr::str_replace(uniqueCohorts$dataLocation[i], "plpData", "studyPopulation"))

  plpDataExists <- file.exists(file.path(plpDataLocation))
  if (!plpDataExists){
    plpDataRaw <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                     covariateSettings = covariateSettings,
                                                     restrictPlpDataSettings = restrictPlpDataSettings)
    PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(plpDataLocation) )
  } else {
    ParallelLogger::logInfo(paste('Raw data for', analysisName, 'exists at', file.path(plpDataLocation)))
  }
  ParallelLogger::logInfo(paste("Done extracting raw data covariates."))
  
  ParallelLogger::logInfo(paste("Creating study population..."))
  if (dir.exists(studyPopulationLocation) == F){ 
    dir.create(studyPopulationLocation, recursive = T)
  }
  populationExists <- file.exists(file.path(studyPopulationLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  if (!populationExists){
    # if(plpDataExists){
      plpDataRaw <- PatientLevelPrediction::loadPlpData(file = file.path(plpDataLocation))
    # }
    studyPopulationRaw <- PatientLevelPrediction::createStudyPopulation(plpData = plpDataRaw,
                                                                        outcomeId = databaseDetails$outcomeIds, 
                                                                        populationSettings = populationSettings)
    plpDataRaw$population <- studyPopulationRaw
    PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(plpDataLocation))
    saveRDS(object = studyPopulationRaw, file = file.path(studyPopulationLocation, paste(analysisName, "studyPopulationRaw.Rds", sep = "_")))
  } else {
    ParallelLogger::logInfo(paste('Study population for', analysisName, 'exists at', file.path(studyPopulationLocation)))
  }
  
  }
  ParallelLogger::logInfo(paste("Done creating study population."))
  
  return(invisible())
}