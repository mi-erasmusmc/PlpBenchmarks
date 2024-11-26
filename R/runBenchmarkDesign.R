#' @export
runBenchmarkDesign <- function(benchmarkDesign, 
                               seed = sample(x = c(1:10000), 1)){
  
  checkmate::check_numeric(seed)
  checkmate::check_null(benchmarkDesign)
  checkmate::check_class(benchmarkDesign, "benchmarkDesign")
  
  ParallelLogger::logInfo(paste('Preparing to run', length(benchmarkDesign), "models."))
  
  benchmarkSettings <- attr(benchmarkDesign, "settings")
  
  for (i in seq_along(benchmarkDesign)) {
    
    plpDataLocation <- benchmarkSettings$dataLocation[i]
    populationLocation <- file.path(stringr::str_replace(benchmarkSettings$dataLocation[i], "plpData", "studyPopulation"), paste0(benchmarkSettings$analysisId[benchmarkSettings$problemId[i]] , "_studyPopulationRaw.Rds"))
    
    plpData <- PatientLevelPrediction::loadPlpData(plpDataLocation)
    population <- readRDS(populationLocation)
    plpData$population <- population
    
    outcomeId <- benchmarkDesign[[i]]$outcomeId
    analysisName <- benchmarkDesign[[i]]$analysisName
    analysisId <- benchmarkDesign[[i]]$analysisName
    populationSettings <- benchmarkDesign[[i]]$populationSettings
    splitSettings <- benchmarkDesign[[i]]$splitSettings
    sampleSettings <- benchmarkDesign[[i]]$sampleSettings
    preprocessSettings <- benchmarkDesign[[i]]$preprocessSettings
    featureEngineeringSettings <- benchmarkDesign[[i]]$featureEngineeringSettings
    modelSettings <- benchmarkDesign[[i]]$modelSettings
    logSettings <- benchmarkDesign[[i]]$logSettings
    executeSettings <- benchmarkDesign[[i]]$executeSettings
    saveDirectory <- benchmarkDesign[[i]]$saveDirectory
    
    analysisExists <- file.exists(file.path(saveDirectory, "plpResult", "runPlp.rds"))
    if(!analysisExists){
      ParallelLogger::logInfo(paste('Preparing to run', names(benchmarkDesign[i]), "analysis."))
  
  result <- PatientLevelPrediction::runPlp(plpData = plpData,
                                           outcomeId = outcomeId, 
                                           analysisId = analysisName,
                                           analysisName = analysisName,
                                           populationSettings = populationSettings, 
                                           splitSettings = splitSettings,
                                           sampleSettings = sampleSettings, 
                                           featureEngineeringSettings = featureEngineeringSettings, 
                                           preprocessSettings = preprocessSettings,
                                           modelSettings = modelSettings,
                                           logSettings = logSettings,  
                                           executeSettings = executeSettings, 
                                           saveDirectory = file.path(dirname(saveDirectory))
  )
    } else {
      ParallelLogger::logInfo(paste('Model for analysis', names(benchmarkDesign)[[i]], "already exists."))
    }
  }
  
}