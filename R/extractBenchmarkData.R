#' @title Extract the data for the benchmark
#' 
#' @description
#' Extracts the data needed to run the benchmark models and optionally creates the study population.
#' 
#' 
#' @param benchmarkDesign An object of class \code{benchmarkDesign}.
#' @param createStudyPopulation Whether to create the study population or not. 
#' 
#' @export
extractBenchmarkData <- function(benchmarkDesign, 
                                 createStudyPopulation = TRUE 
                                 # seed = sample(x = c(1:10000), 1)
                                 ){
  
  uniqueCohorts <- attr(benchmarkDesign, "uniquePlpData")
  minimalCohortsToCreate <- uniqueCohorts$problemId
  minimalCohorts <- benchmarkDesign[c(minimalCohortsToCreate)]
  
  ParallelLogger::logInfo(paste("Will extract", nrow(uniqueCohorts), "unique plpData objects."))
  
  
  
  for (i in seq_along(minimalCohorts)) {
   
    databaseDetails <- minimalCohorts[[i]]$databaseDetails
    covariateSettings <- minimalCohorts[[i]]$covariateSettings
    restrictPlpDataSettings <- minimalCohorts[[i]]$restrictPlpDataSettings
    populationSettings <- minimalCohorts[[i]]$populationSettings
    analysisName <- minimalCohorts[[i]]$analysisName
    plpDataLocation <- uniqueCohorts$dataLocation[i]
    # studyPopulationLocation <- file.path(stringr::str_replace(uniqueCohorts$dataLocation[i], "plpData", "studyPopulation"))

  plpDataExists <- file.exists(file.path(plpDataLocation))
  if (!plpDataExists){
    
    ParallelLogger::logInfo(paste("Extracting raw plpData object for ", paste(uniqueCohorts$plpDataName[i])))
    
    plpDataRaw <- PatientLevelPrediction::getPlpData(databaseDetails = databaseDetails, 
                                                     covariateSettings = covariateSettings,
                                                     restrictPlpDataSettings = restrictPlpDataSettings)
    PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(plpDataLocation) )
  } else {
    ParallelLogger::logInfo(paste('Raw data for', analysisName, 'already exists at', file.path(plpDataLocation)))
  }
  }
  ParallelLogger::logInfo(paste("Done extracting raw plpData."))
  
  if (createStudyPopulation) {
    
    uniquePopulation <- attr(benchmarkDesign, "benchmarkSettings")
    
    for (i in seq_along(benchmarkDesign)){
      
      databaseDetails = benchmarkDesign[[i]]$databaseDetails
      populationSettings = benchmarkDesign[[i]]$populationSettings
      
      studyPopulationLocation = uniquePopulation$populationLocation[i]
      plpDataLocation = uniquePopulation$dataLocation[i]
      plpDataName = uniquePopulation$plpDataName[i]
    
    if (dir.exists(studyPopulationLocation) == F){ 
      dir.create(studyPopulationLocation, recursive = T)
    }
    
  populationExists <- file.exists(file.path(studyPopulationLocation, paste(plpDataName, "studyPopulation.Rds", sep = "_")))
  if (!populationExists){
    # if(plpDataExists){
    ParallelLogger::logInfo(paste("Creating study population..."))
    
    plpDataRaw <- PatientLevelPrediction::loadPlpData(file = file.path(plpDataLocation))
    # }
    studyPopulationRaw <- PatientLevelPrediction::createStudyPopulation(plpData = plpDataRaw,
                                                                        outcomeId = databaseDetails$outcomeIds, 
                                                                        populationSettings = populationSettings)
    # plpDataRaw$population <- studyPopulationRaw
    PatientLevelPrediction::savePlpData(plpData = plpDataRaw, file = file.path(plpDataLocation))
    saveRDS(object = studyPopulationRaw, file = file.path(studyPopulationLocation, paste(plpDataName, "studyPopulation.Rds", sep = "_")))
    
    ParallelLogger::logInfo(paste("Study population created and exists at", file.path(studyPopulationLocation)))
  } else {
    ParallelLogger::logInfo(paste('Study population for', analysisName, 'already exists at', file.path(studyPopulationLocation)))
  }
    }
  }
  
  ParallelLogger::logInfo(paste("Done creating study population."))
  
  return(invisible())
}